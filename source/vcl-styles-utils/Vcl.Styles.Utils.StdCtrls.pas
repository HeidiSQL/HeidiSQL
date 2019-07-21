// **************************************************************************************************
//
// Unit Vcl.Styles.Utils.StdCtrls
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
unit Vcl.Styles.Utils.StdCtrls;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.CommCtrl,
  Vcl.Themes,
  Vcl.Graphics,
  Vcl.Styles.Utils.SysStyleHook,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Styles.Utils.Forms,
  Vcl.GraphUtil,
  Vcl.Controls;

const
  BS_SPLITBUTTON = $0000000C;
{$EXTERNALSYM BS_DEFSPLITBUTTON}
  BS_DEFSPLITBUTTON = $0000000D;
{$EXTERNALSYM BS_COMMANDLINK}
  BS_COMMANDLINK = $0000000E;
{$EXTERNALSYM BS_DEFCOMMANDLINK}
  BS_DEFCOMMANDLINK = $0000000F;

type
  TSysCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);

  TSysButtonStyleHook = class(TMouseTrackSysControlStyleHook)
  private
    function GetCaptionRect(Canvas: TCanvas): TRect;
    function GetBoxRect: TRect;
    function IsCheckBox: Boolean;
    function IsRadioButton: Boolean;
    function IsGroupBox: Boolean;
    function IsPushButton: Boolean;
    function IsSplitButton: Boolean;
    function IsCommandButton: Boolean;
    function GetTextAlign: TTextFormat;
    function GetShowText: Boolean;
    function GetCheckBoxState: TSysCheckBoxState;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    function IsOwnerDraw: Boolean;
  protected
    procedure DrawCheckBoxText(DC: HDC; Text: String;
      LDetails: TThemedElementDetails; R: TRect); virtual;
    procedure PaintButton(Canvas: TCanvas); virtual;
    procedure PaintCheckBox(Canvas: TCanvas); virtual;
    procedure PaintRadioButton(Canvas: TCanvas); virtual;
    procedure PaintGroupBox(Canvas: TCanvas); virtual;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateColors; override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property CheckBox: Boolean read IsCheckBox;
    property CommandButton: Boolean read IsCommandButton;
    property RadioButton: Boolean read IsRadioButton;
    property GroupBox: Boolean read IsGroupBox;
    property PushButton: Boolean read IsPushButton;
    property SplitButton: Boolean read IsSplitButton;
    property CheckBoxState: TSysCheckBoxState read GetCheckBoxState;
    property TextAlign: TTextFormat read GetTextAlign;
    property ShowText: Boolean read GetShowText;
    property OwnerDraw: Boolean read IsOwnerDraw;
  end;

  TSysEditStyleHook = class(TMouseTrackSysControlStyleHook)
  private
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
  protected
    procedure PaintNC(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateColors; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    function GetBorderSize: TRect; override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

  TSysMemoStyleHook = class(TSysScrollingStyleHook)
  strict protected
    procedure UpdateColors; override;
    procedure WndProc(var Message: TMessage); override;
    function GetBorderSize: TRect; override;
  public
    constructor Create(AHandle: THandle); override;
  end;

  TSysListBoxStyleHook = class(TSysScrollingStyleHook)
  protected
    function GetBorderSize: TRect; override;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateColors; override;
    procedure PaintBackground(Canvas: TCanvas); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

  TSysComboBoxStyleHook = class(TMouseTrackSysControlStyleHook)
  strict private
    FDownPos, FMovePos: TPoint;
    FDownSliderPos: Integer;
    FOldIdx, FInvsibleCount, FSliderSize: Integer;
    FVSliderState, FVUpState, FVDownState: TThemedScrollBar;
    FIgnoreStyleChanged: Boolean;
    FMouseOnButton: Boolean;
    FListHandle, FEditHandle: HWnd;
    FListBoxInstance: Pointer;
    FDefListBoxProc: Pointer;
    FListBoxTimerCode: Integer;
    FListBoxUpBtnDown, FListBoxDownBtnDown, FListBoxTrackUpDown,
      FListBoxTrackDownDown: Boolean;
    procedure DrawListBoxVertScroll(DC: HDC);
    procedure DrawListBoxBorder;
    function IsDroppedDown: Boolean;
    function GetButtonRect: TRect;
    function Style: TComboBoxStyle;
    function ListBoxBoundsRect: TRect;
    function ListBoxClientRect: TRect;
    procedure ListBoxSetTimer(const ATimerCode: Integer);
    procedure ListBoxStopTimer;
    function ListBoxVertScrollRect: TRect;
    function ListBoxVertDownButtonRect: TRect;
    function ListBoxVertUpButtonRect: TRect;
    function ListBoxVertScrollArea: TRect;
    function ListBoxVertSliderRect: TRect;
    function ListBoxVertTrackRect: TRect;
    function ListBoxVertTrackRectUp: TRect;
    function ListBoxVertTrackRectDown: TRect;
    procedure PaintListBoxBorder(Canvas: TCanvas; const R: TRect);
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMDrawItem(var Message: TWMDrawItem); message WM_DRAWITEM;
    procedure WMParentNotify(var Message: TMessage); message WM_PARENTNOTIFY;
  strict protected
    procedure UpdateColors; override;
    function IsChildHandle(AHandle: HWnd): Boolean; override;
    procedure DrawItem(Canvas: TCanvas;const Index: UINT; const R: TRect;
     const Selected: Boolean); virtual;
    procedure HookListBox(AListHandle: HWnd);
    property ListBoxInstance: Pointer read FListBoxInstance;
    procedure ListBoxWndProc(var Msg: TMessage); virtual;
    property ListHandle: HWnd read FListHandle;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure PaintBorder(Canvas: TCanvas); virtual;
    procedure WndProc(var Message: TMessage); override;
    property ButtonRect: TRect read GetButtonRect;
    property MouseOnButton: Boolean read FMouseOnButton write FMouseOnButton;
    property DroppedDown: Boolean read IsDroppedDown;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

  TSysStaticStyleHook = class(TSysStyleHook)
  private
    FUpdatedColor: TColor;
    function GetIsText: Boolean;
    function GetTextFormat: TTextFormat;
    function GetIsFrameOrLine: Boolean;
  protected
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure UpdateColors; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property IsText: Boolean read GetIsText;
    property IsFrameOrLine: Boolean read GetIsFrameOrLine;
    property TextFormat: TTextFormat read GetTextFormat;
  end;

  TSysCheckBoxStyleHook = class(TMouseTrackSysControlStyleHook)
  strict private
    FPressed: Boolean;
    procedure WMLButtonDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMMouse); message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LBUTTONDBLCLK;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure BMSetCheck(var Message: TMessage); message BM_SETCHECK;
    function RightAlignment: Boolean;
  strict protected
    function GetDrawState(State: TSysCheckBoxState): TThemedButton; virtual;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure WndProc(var Message: TMessage); override;
    property Pressed: Boolean read FPressed;
  public
    constructor Create(AHandle: THandle); override;
  end;

  TSysRadioButtonStyleHook = class(TSysCheckBoxStyleHook)
  strict protected
    function GetDrawState(State: TSysCheckBoxState): TThemedButton; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;


implementation

uses
  Vcl.ExtCtrls,
  System.UITypes,
  Vcl.Styles.Utils.Misc,
  Vcl.Styles.Utils.SysControls;

{ TSysEditStyleHook }

constructor TSysEditStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23}
  StyleElements := [seFont, seBorder];
{$ELSE}
  OverridePaint := False;
  OverridePaintNC := True;
  OverrideFont := True;
{$IFEND}
end;

destructor TSysEditStyleHook.Destroy;
begin
  inherited;
end;

function TSysEditStyleHook.GetBorderSize: TRect;
begin
  if SysControl.HasBorder then
    Result := Rect(2, 2, 2, 2);
end;

procedure TSysEditStyleHook.MouseEnter;
begin
  InvalidateNC;
end;

procedure TSysEditStyleHook.MouseLeave;
begin
  InvalidateNC;
end;

procedure TSysEditStyleHook.PaintNC(Canvas: TCanvas);
var
  Details: TThemedElementDetails;
  R: TRect;
begin
  if StyleServicesEnabled and SysControl.HasBorder then
  begin
    if Focused then
      Details := StyleServices.GetElementDetails(teEditBorderNoScrollFocused)
    else if MouseInControl then
      Details := StyleServices.GetElementDetails(teEditBorderNoScrollHot)
    else if SysControl.Enabled then
      Details := StyleServices.GetElementDetails(teEditBorderNoScrollNormal)
    else
      Details := StyleServices.GetElementDetails(teEditBorderNoScrollDisabled);
    R := Rect(0, 0, SysControl.Width, SysControl.Height);
    InflateRect(R, -2, -2);
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    StyleServices.DrawElement(Canvas.Handle, Details,
      Rect(0, 0, SysControl.Width, SysControl.Height));
  end;
end;

procedure TSysEditStyleHook.UpdateColors;
const
  ColorStates: array [Boolean] of TStyleColor = (scEditDisabled, scEdit);
  FontColorStates: array [Boolean] of TStyleFont = (sfEditBoxTextDisabled,
    sfEditBoxTextNormal);
begin
  Color := StyleServices.GetStyleColor(ColorStates[SysControl.Enabled]);
{$IF CompilerVersion > 23}
  if seFont in StyleElements then
    FontColor := StyleServices.GetStyleFontColor
      (FontColorStates[SysControl.Enabled])
  else
    FontColor := clWindowText;
{$ELSE}
  FontColor := StyleServices.GetStyleFontColor
    (FontColorStates[SysControl.Enabled]);
{$IFEND}
end;

procedure TSysEditStyleHook.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  Params: PNCCalcSizeParams;
begin
  Handled := False;
  if (not StyleServicesEnabled) or (not OverridePaintNC) then
    Exit;

  Params := Message.CalcSize_Params;
  if SysControl.HasBorder then
    with Params^.rgrc[0] do
    begin
      Inc(Left, 2);
      Inc(Top, 2);
      Dec(Right, 2);
      Dec(Bottom, 2);
    end;
  Handled := True;
end;

procedure TSysEditStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CM_CTLCOLORMSGBOX .. CM_CTLCOLORSTATIC:
      begin
        { Change edit control color . }
        SetTextColor(Message.wParam, ColorToRGB(FontColor));
        SetBkColor(Message.wParam, ColorToRGB(Color));
        Message.Result := LRESULT(Brush.Handle);
      end;
  else
    inherited WndProc(Message);
  end;

end;

{ TSysListBoxStyleHook }

constructor TSysListBoxStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23}
  StyleElements := [seBorder];
{$ELSE}
  OverridePaint := False;
  OverridePaintNC := True;
  OverrideFont := False;
{$IFEND}
  //OverrideEraseBkgnd:=True;
end;

destructor TSysListBoxStyleHook.Destroy;
begin

  inherited;
end;

function TSysListBoxStyleHook.GetBorderSize: TRect;
begin
  Result := inherited GetBorderSize;
  if (SysControl.HasBorder) then
  begin
    Result := Rect(2, 2, 2, 2);
  end;
  if SameText(SysControl.ControlClassName, 'ComboLBox') then
  begin
    if SysControl.Parent.Style and CBS_SIMPLE = CBS_SIMPLE then
      Exit;
    Result := Rect(0, 0, 0, 0);
  end;
end;

procedure TSysListBoxStyleHook.PaintBackground(Canvas: TCanvas);
begin
  inherited;
end;

procedure TSysListBoxStyleHook.UpdateColors;
const
  ColorStates: array[Boolean] of TStyleColor = (scListBoxDisabled, scListBox);
  FontColorStates: array[Boolean] of TStyleFont = (sfListItemTextDisabled, sfListItemTextNormal);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  Brush.Color := LStyle.GetStyleColor(ColorStates[SysControl.Enabled]);
  FontColor := LStyle.GetStyleFontColor(FontColorStates[SysControl.Enabled]);
end;

procedure TSysListBoxStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

{ TSysButtonStyleHook }

constructor TSysButtonStyleHook.Create(AHandle: THandle);
begin
  inherited;
  ParentColor := True;
{$IF CompilerVersion > 23}
  StyleElements := [seFont, seClient, seBorder];
{$ELSE}
  OverridePaint := True;
  OverridePaintNC := True;
  OverrideFont := True;
{$IFEND}
  Color := StyleServices.GetStyleColor(scWindow);
end;

destructor TSysButtonStyleHook.Destroy;
begin

  inherited;
end;

procedure TSysButtonStyleHook.DrawCheckBoxText(DC: HDC; Text: String;
  LDetails: TThemedElementDetails; R: TRect);
var
  TextFormat: TTextFormat;
begin
  if ShowText then
  begin
    TextFormat := [tfVerticalCenter, tfHidePrefix];
    if (SysControl.Style and BS_MULTILINE = BS_MULTILINE) then
      include(TextFormat, tfWordBreak)
    else
      include(TextFormat, tfSingleLine);
    if (SysControl.Style and BS_LEFT = BS_LEFT) then
      include(TextFormat, tfLeft)
    else if (SysControl.Style and BS_RIGHT = BS_RIGHT) then
      include(TextFormat, tfRight)
    else if (SysControl.Style and BS_CENTER = BS_CENTER) then
      include(TextFormat, tfCenter);
    DrawText(DC, LDetails, SysControl.Text, R, TextFormat);
  end;
end;

function TSysButtonStyleHook.GetCheckBoxState: TSysCheckBoxState;
var
  LState: DWORD;
begin
  LState := SendMessage(Handle, BM_GETCHECK, 0, 0);
  Result := TSysCheckBoxState(LState)
end;

function TSysButtonStyleHook.GetShowText: Boolean;
begin
  Result := (SysControl.Style and BS_TEXT = BS_TEXT);
end;

function TSysButtonStyleHook.GetTextAlign: TTextFormat;
begin
  Result := [];
  with SysControl do
  begin
    // if Style and BS_LEFTTEXT then

  end;
end;

function TSysButtonStyleHook.IsCheckBox: Boolean;
begin
  with SysControl do
    Result := (Style and BS_CHECKBOX = BS_CHECKBOX) or
      (Style and BS_AUTOCHECKBOX = BS_AUTOCHECKBOX);
end;

function TSysButtonStyleHook.IsCommandButton: Boolean;
begin
  Result := (SysControl.Style and BS_COMMANDLINK = BS_COMMANDLINK) or
    (SysControl.Style and BS_DEFCOMMANDLINK = BS_DEFCOMMANDLINK);
end;

function TSysButtonStyleHook.IsGroupBox: Boolean;
begin
  Result := (SysControl.Style and BS_GROUPBOX = BS_GROUPBOX);
end;

function TSysButtonStyleHook.IsOwnerDraw: Boolean;
begin
  Result := (SysControl.Style and BS_OWNERDRAW = BS_OWNERDRAW);
end;

function TSysButtonStyleHook.IsPushButton: Boolean;
begin
  with SysControl do
    Result := (Style and BS_PUSHBUTTON = BS_PUSHBUTTON) or
      (not CheckBox and not RadioButton and not GroupBox and not CommandButton);
end;

function TSysButtonStyleHook.IsRadioButton: Boolean;
begin
  with SysControl do
    Result := (Style and BS_RADIOBUTTON = BS_RADIOBUTTON) or
      (Style and BS_AUTORADIOBUTTON = BS_AUTORADIOBUTTON);

 if Result then
      Result:= not IsSplitButton;
end;

function TSysButtonStyleHook.IsSplitButton: Boolean;
begin
  Result := (SysControl.Style and BS_SPLITBUTTON = BS_SPLITBUTTON) or
    (SysControl.Style and BS_DEFSPLITBUTTON = BS_DEFSPLITBUTTON);

end;

procedure TSysButtonStyleHook.MouseEnter;
begin
  // Invalidate;
end;

procedure TSysButtonStyleHook.MouseLeave;
begin
  Invalidate;
end;

procedure TSysButtonStyleHook.Paint(Canvas: TCanvas);
begin
  //OutputDebugString(PChar('Paint '+IntToHex(SysControl.Handle, 8)));
  if not GroupBox or CommandButton then
    PaintBackground(Canvas)
  else
    Exit;

  if CommandButton then
    PaintButton(Canvas)
  else
  if CheckBox then
    PaintCheckBox(Canvas)
  else
  if RadioButton then
    PaintRadioButton(Canvas)
  else
  if PushButton then
    PaintButton(Canvas);

end;

procedure TSysButtonStyleHook.PaintBackground(Canvas: TCanvas);
begin
  if not GroupBox then
    inherited;
end;

procedure TSysButtonStyleHook.PaintButton(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
  LRect: TRect;
  Detail: TThemedButton;
  X, Y, i: Integer;
  IW, IH, IY: Integer;
  TextFormat: TTextFormat;
  IL: BUTTON_IMAGELIST;
  LText: string;
  DrawRect: TRect;
  ThemeTextColor: TColor;
  Buffer: string;
  BufferLength: Integer;
begin
  LText := SysControl.Text;
  LRect := SysControl.ClientRect;

  if SysControl.Enabled then
    Detail := tbPushButtonNormal
  else
    Detail := tbPushButtonDisabled;

  if MouseDown then
    Detail := tbPushButtonPressed
  else
  if MouseInControl then
    Detail := tbPushButtonHot
  else
  if Focused then
    Detail := tbPushButtonDefaulted;

  LDetails := StyleServices.GetElementDetails(Detail);
  DrawRect := SysControl.ClientRect;
  StyleServices.DrawElement(Canvas.Handle, LDetails, LRect);


  if Button_GetImageList(handle, IL) and (IL.himl <> 0) and
     ImageList_GetIconSize(IL.himl, IW, IH) then
  begin
    if (GetWindowLong(Handle, GWL_STYLE) and BS_COMMANDLINK) = BS_COMMANDLINK then
      IY := DrawRect.Top + 15
    else
      IY := DrawRect.Top + (DrawRect.Height - IH) div 2;
    ImageList_Draw(IL.himl, 0, Canvas.Handle, DrawRect.Left + 3, IY, ILD_NORMAL);
    Inc(DrawRect.Left, IW + 3);
  end;

  if CommandButton then
  begin
      if IL.himl = 0 then
        Inc(DrawRect.Left, 35);
      Inc(DrawRect.Top, 15);
      Inc(DrawRect.Left, 5);
      Canvas.Font := SysControl.Font;
      TextFormat := TTextFormatFlags(DT_LEFT);
      if StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
         Canvas.Font.Color := ThemeTextColor;
      StyleServices.DrawText(Canvas.Handle, LDetails, LText, DrawRect, TextFormat, Canvas.Font.Color);
      SetLength(Buffer, Button_GetNoteLength(Handle) + 1);
      if Length(Buffer) <> 0 then
      begin
        BufferLength := Length(Buffer);
        if Button_GetNote(Handle, PChar(Buffer), BufferLength) then
        begin
          TextFormat := TTextFormatFlags(DT_LEFT or DT_WORDBREAK);
          Inc(DrawRect.Top, Canvas.TextHeight('Wq') + 2);
          Canvas.Font.Size := 8;
          StyleServices.DrawText(Canvas.Handle, LDetails, Buffer, DrawRect,
            TextFormat, Canvas.Font.Color);
        end;
      end;
      if IL.himl = 0 then
      begin
        if MouseDown then
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphPressed)
        else if MouseInControl then
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphHot)
        else if SysControl.Enabled then
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphNormal)
        else
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphDisabled);
        DrawRect.Right := 35;
        DrawRect.Left := 3;
        DrawRect.Top := 10;
        DrawRect.Bottom := DrawRect.Top + 32;
        StyleServices.DrawElement(Canvas.Handle, LDetails, DrawRect);
      end;

  end
  else
  if SplitButton then
    with Canvas, SysControl do
    begin
      { draw vertical line }
      Pen.Color := StyleServices.GetSystemColor(clBtnShadow);
      MoveTo(Width - 15, 3);
      LineTo(Width - 15, Height - 3);
      if Enabled then
        Pen.Color := StyleServices.GetSystemColor(clBtnHighLight)
      else
        Pen.Color := Font.Color;
      MoveTo(Width - 14, 3);
      LineTo(Width - 14, Height - 3);
      { Draw arrow }
      Pen.Color := Font.Color;
      X := Width - 8;
      Y := Height div 2 + 1;
      for i := 3 downto 0 do
      begin
        MoveTo(X - i, Y - i);
        LineTo(X + i + 1, Y - i);
      end;
    end;

  if ShowText and not IsCommandButton then
  begin
    TextFormat := [tfCenter, tfVerticalCenter, tfSingleLine, tfHidePrefix];
    if (SysControl.Style and BS_MULTILINE = BS_MULTILINE) then
    begin
      Exclude(TextFormat, tfSingleLine);
      include(TextFormat, tfWordBreak)
    end;


    DrawText(Canvas.Handle, LDetails, SysControl.Text, LRect, TextFormat);
  end;
end;

function TSysButtonStyleHook.GetBoxRect: TRect;
var
  DC: HDC;
  sSize: TSize;
begin
  DC := GetDC(Handle);
  with SysControl do
  begin
    GetTextExtentPoint32(DC, Text, Length(Text) - 1, sSize);
    Result := Rect(0, sSize.Height div 2 + 1, Width - 0, Height - 0);
  end;
  ReleaseDC(Handle, DC);
  DeleteDC(DC);
end;

function TSysButtonStyleHook.GetCaptionRect(Canvas: TCanvas): TRect;
const
  FCaptionMargin = 12;
begin
  with SysControl do
    if BiDiMode <> bmRightToLeft then
      Result := Rect(FCaptionMargin, 0, FCaptionMargin + Canvas.TextWidth(Text),
        Canvas.TextHeight(Text))
    else
      Result := Rect(Width - Canvas.TextWidth(Text) - FCaptionMargin, 0,
        Width - FCaptionMargin, Canvas.TextHeight(Text));
end;

procedure TSysButtonStyleHook.PaintRadioButton(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
  DC: HDC;
  LRect: TRect;
  Detail: TThemedButton;
  TxtRect, BoxRect: TRect;
  LState: TSysCheckBoxState;
  Size: TSize;
begin
  DC := Canvas.Handle;
  LRect := SysControl.ClientRect;
  LState := CheckBoxState;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(LRect);
  if SysControl.Enabled then
    Detail := tbRadioButtonUncheckedNormal
  else
    Detail := tbRadioButtonUncheckedDisabled;
  if MouseDown then
    Detail := tbRadioButtonUncheckedPressed
  else if MouseInControl then
    Detail := tbRadioButtonUncheckedHot;

  if LState = cbChecked then
    Detail := TThemedButton(Integer(Detail) + 4);

  Size.cx := GetSystemMetrics(SM_CXMENUCHECK);
  Size.cy := GetSystemMetrics(SM_CYMENUCHECK);

  LDetails := StyleServices.GetElementDetails(Detail);
  BoxRect := Rect(0, 0, Size.cx, Size.cy);
  RectVCenter(BoxRect, LRect);

  if (SysControl.Style and BS_LEFTTEXT = BS_LEFTTEXT) then
  begin
    BoxRect.Left := LRect.Right - BoxRect.Width - 2;
    BoxRect.Right := LRect.Right;
    TxtRect := Rect(LRect.Left + 1, LRect.Top, BoxRect.Left, LRect.Bottom);
  end
  else
  begin
    OffsetRect(BoxRect, 1, 0);
    TxtRect := Rect(BoxRect.Right + 2, LRect.Top, LRect.Right, LRect.Bottom);
  end;

  StyleServices.DrawElement(DC, LDetails, BoxRect);

  if Focused then
    Canvas.DrawFocusRect(LRect);

  DrawCheckBoxText(DC, SysControl.Text, LDetails, TxtRect);

end;

procedure TSysButtonStyleHook.PaintCheckBox(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
  DC: HDC;
  LRect: TRect;
  Detail: TThemedButton;
  TxtRect, BoxRect: TRect;
  LState: TSysCheckBoxState;
  Size: TSize;
begin
  DC := Canvas.Handle;
  LRect := SysControl.ClientRect;
  LState := CheckBoxState;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(LRect);
  if SysControl.Enabled then
    Detail := tbCheckBoxUncheckedNormal
  else
    Detail := tbCheckBoxUncheckedDisabled;
  if MouseDown then
    Detail := tbCheckBoxUncheckedPressed
  else if MouseInControl then
    Detail := tbCheckBoxUncheckedHot;

  if LState = cbChecked then
    Detail := TThemedButton(Integer(Detail) + 4);
  if LState = cbGrayed then
    Detail := TThemedButton(Integer(Detail) + 8);

  // LDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
  // StyleServices.GetElementSize(DC, LDetails, esActual, Size);
  Size.cx := GetSystemMetrics(SM_CXMENUCHECK);
  Size.cy := GetSystemMetrics(SM_CYMENUCHECK);

  LDetails := StyleServices.GetElementDetails(Detail);
  BoxRect := Rect(0, 0, Size.cx, Size.cy);
  BoxRect := RectVCenter(BoxRect, LRect);

  if (SysControl.Style and BS_LEFTTEXT = BS_LEFTTEXT) then
  begin
    BoxRect.Left := LRect.Right - BoxRect.Width - 2;
    BoxRect.Right := LRect.Right;
    TxtRect := Rect(LRect.Left + 1, LRect.Top, BoxRect.Left, LRect.Bottom);
  end
  else
  begin
    OffsetRect(BoxRect, 1, 0);
    TxtRect := Rect(BoxRect.Right + 2, LRect.Top, LRect.Right, LRect.Bottom);
  end;

  StyleServices.DrawElement(DC, LDetails, BoxRect);

  if Focused then
    Canvas.DrawFocusRect(LRect);

  DrawCheckBoxText(DC, SysControl.Text, LDetails, TxtRect);
end;

procedure TSysButtonStyleHook.PaintGroupBox(Canvas: TCanvas);
var
  R, CaptionRect: TRect;
  LDetails: TThemedElementDetails;
  SaveIndex: Integer;
  procedure DoDrawParentBackground(DC: HDC; ARect: TRect);
  begin
    if SysControl.ParentHandle > 0 then
      DrawParentBackground(DC, @ARect)
    else
    begin
      Canvas.Brush.Color := StyleServices.GetStyleColor(scWindow);
      Canvas.FillRect(ARect);
    end;
  end;

begin
  CaptionRect := GetCaptionRect(Canvas);
  R := GetBoxRect;

  if SysControl.Enabled then
    LDetails := StyleServices.GetElementDetails(tbGroupBoxNormal)
  else
    LDetails := StyleServices.GetElementDetails(tbGroupBoxDisabled);

  { Clean caption area }
  DoDrawParentBackground(Canvas.Handle, CaptionRect);
  ExcludeClipRect(Canvas.Handle, R.Left + 4, CaptionRect.Height + 2,
    R.Right - 4, R.Height - 2);
  { Clean GroupBox corners area }
  DoDrawParentBackground(Canvas.Handle, R);

  SaveIndex := SaveDC(Canvas.Handle);

  try
    ExcludeClipRect(Canvas.Handle, CaptionRect.Left, CaptionRect.Top,
      CaptionRect.Right, CaptionRect.Bottom);
    StyleServices.DrawElement(Canvas.Handle, LDetails, R);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;

  Inc(CaptionRect.Top, 3);
  { Paint Text }
  StyleServices.DrawText(Canvas.Handle, LDetails, SysControl.Text, CaptionRect,
    [tfSingleLine, tfVerticalCenter, tfLeft, tfHidePrefix]);

end;

procedure TSysButtonStyleHook.PaintNC(Canvas: TCanvas);
begin
  if GroupBox then
    PaintGroupBox(Canvas);
end;

procedure TSysButtonStyleHook.UpdateColors;
begin
  inherited;
end;

procedure TSysButtonStyleHook.WMEraseBkgnd(var Message: TMessage);
begin
  if (not OwnerDraw) and (not GroupBox and ParentBkGndPainted) then
    Message.Result := 1
  else
  begin
    Handled := False;
    Exit;
  end;
  Handled := True;
end;

procedure TSysButtonStyleHook.WMNCPaint(var Message: TMessage);
begin
  if (not OwnerDraw and ParentBkGndPainted) then
    Inherited
  else
  begin
    Handled := False;
    Exit;
  end;
  Handled := True;
end;

procedure TSysButtonStyleHook.WMPaint(var Message: TMessage);
begin
  if (not OwnerDraw and ParentBkGndPainted) then
    Inherited
  else
  begin
    Handled := False;
    Exit;
  end;
  Handled := True;
end;

procedure TSysButtonStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_ENABLE:
      begin
        { Check first if Window is visible
          if you dont check ..the InVisible window will be visible .
        }
        if SysControl.Visible then
          Invalidate;
      end;

    WM_STYLECHANGING, WM_STYLECHANGED:
      begin
        Invalidate;
      end;

    WM_SETTEXT:
      begin
        SetRedraw(False);
        CallDefaultProc(Message);
        SetRedraw(True);
        Invalidate;
      end;

    WM_SETFOCUS, WM_KILLFOCUS:
      begin
        inherited;
        Invalidate;
      end;

  else
    inherited;
  end;

end;

{ TSysMemoStyleHook }

constructor TSysMemoStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23}
  StyleElements := [seBorder, seFont];
{$ELSE}
  OverridePaintNC := True;
  OverrideFont := True;
{$IFEND}
  UpdateColors;
end;

function TSysMemoStyleHook.GetBorderSize: TRect;
begin
  if SysControl.HasBorder then
    Result := Rect(2, 2, 2, 2);
end;

procedure TSysMemoStyleHook.UpdateColors;
const
  ColorStates: array [Boolean] of TStyleColor = (scEditDisabled, scEdit);
  FontColorStates: array [Boolean] of TStyleFont = (sfEditBoxTextDisabled,
    sfEditBoxTextNormal);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  Brush.Color := LStyle.GetStyleColor(ColorStates[SysControl.Enabled]);
  FontColor := LStyle.GetStyleFontColor(FontColorStates[SysControl.Enabled]);
end;

procedure TSysMemoStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_ERASEBKGND:
      begin
        CallDefaultProc(Message);
        Exit;
      end;

    CN_CTLCOLORMSGBOX .. CN_CTLCOLORSTATIC:
      begin
        SetTextColor(Message.wParam, ColorToRGB(FontColor));
        SetBkColor(Message.wParam, ColorToRGB(Brush.Color));
        Message.Result := LRESULT(Brush.Handle);
      end;

    CM_ENABLEDCHANGED:
      begin
        UpdateColors;
        CallDefaultProc(Message);
      end
  else
    inherited WndProc(Message);
  end;
end;

{ TSysComboBoxStyleHook }
constructor TSysComboBoxStyleHook.Create(AHandle: THandle);
begin
  inherited;
  if Style = csSimple then
    OverrideEraseBkgnd := True;
  FMouseOnButton := False;
  FEditHandle := 0;
  FListHandle := 0;
  FListBoxInstance := nil;
  FIgnoreStyleChanged := False;
  FVSliderState := tsThumbBtnVertNormal;
  FVUpState := tsArrowBtnUpNormal;
  FVDownState := tsArrowBtnDownNormal;
  FSliderSize := 0;
  FListBoxTimerCode := 0;
  FListBoxUpBtnDown := False;
  FListBoxDownBtnDown := False;
  FListBoxTrackUpDown := False;
  FListBoxTrackDownDown := False;
  OverrideFont := True;
  UpdateColors;
end;

destructor TSysComboBoxStyleHook.Destroy;
begin
  if (FListHandle <> 0) and (FListBoxInstance <> nil) then
  begin
    SetWindowLong(FListHandle, GWL_WNDPROC, IntPtr(FDefListBoxProc));
    FreeObjectInstance(FListBoxInstance);
    FListBoxInstance := nil;
  end;
  if FListBoxTimerCode <> 0 then
    ListBoxStopTimer;
  inherited;
end;

procedure TSysComboBoxStyleHook.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = CBN_SELENDCANCEL) or
    (Message.NotifyCode = CBN_SELENDOK) or (Message.NotifyCode = CBN_CLOSEUP) or
    (Message.NotifyCode = CBN_DROPDOWN) or (Message.NotifyCode = CBN_SELCHANGE)
  then
  begin
    if FListBoxTimerCode <> 0 then
      ListBoxStopTimer;
    FMouseOnButton := False;
    Invalidate;
  end;
end;

procedure TSysComboBoxStyleHook.CNDrawItem(var Message: TWMDrawItem);
begin
  WMDrawItem(Message);
  Handled := True;
end;

procedure TSysComboBoxStyleHook.DrawItem(Canvas: TCanvas;const Index: UINT;
  const R: TRect;const Selected: Boolean);
var
  DIS: TDrawItemStruct;
begin
  FillChar(DIS, SizeOf(DIS), 0);
  DIS.CtlType := ODT_COMBOBOX;
  DIS.CtlID := GetDlgCtrlID(Handle);
  DIS.itemAction := ODA_DRAWENTIRE;
  DIS.HDC := Canvas.Handle;
  DIS.hwndItem := Handle;
  DIS.rcItem := R;
  DIS.itemID := Index;
  DIS.itemData := SendMessage(FListHandle, LB_GETITEMDATA, 0, 0);
  if Selected then
    DIS.itemState := DIS.itemState or ODS_FOCUS or ODS_SELECTED;

  SendMessage(Handle, WM_DRAWITEM, Handle, LPARAM(@DIS));
end;

procedure TSysComboBoxStyleHook.DrawListBoxBorder;
var
  R: TRect;
  Canvas: TCanvas;
  SaveIdx: Integer;
  P: TPoint;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetWindowDC(FListHandle);
    P := Point(0, 0);
    ClientToScreen(FListHandle, P);
    GetWindowRect(FListHandle, R);
    P.X := P.X - R.Left;
    P.Y := P.Y - R.Top;
    if (R.Width < 5000) and (R.Height < 5000) then
    begin
      GetClientRect(FListHandle, R);
      ExcludeClipRect(Canvas.Handle, P.X, P.Y, R.Right - R.Left + P.X,
        R.Bottom - R.Top + P.Y);
      GetWindowRect(FListHandle, R);
      OffsetRect(R, -R.Left, -R.Top);
      SaveIdx := SaveDC(Canvas.Handle);
      try
        PaintListBoxBorder(Canvas, R);
      finally
        RestoreDC(Canvas.Handle, SaveIdx);
      end;
      DrawListBoxVertScroll(Canvas.Handle);
    end;
  finally
    ReleaseDC(FListHandle, Canvas.Handle);
    Canvas.Handle := 0;
    Canvas.Free;
  end;
end;

procedure TSysComboBoxStyleHook.DrawListBoxVertScroll(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  Canvas: TCanvas;
  R: TRect;
begin
  if GetWindowLong(FListHandle, GWL_STYLE) and WS_VSCROLL = 0 then
    Exit;
  Canvas := TCanvas.Create;
  try
    if DC <> 0 then
      Canvas.Handle := DC
    else
      Canvas.Handle := GetWindowDC(FListHandle);
    if ListBoxVertScrollRect.Width > 0 then
    begin
      B := TBitmap.Create;
      try
        B.Width := ListBoxVertScrollRect.Width;
        B.Height := ListBoxVertScrollRect.Height;
        MoveWindowOrg(B.Canvas.Handle, -ListBoxVertScrollRect.Left,
          -ListBoxVertScrollRect.Top);

        if StyleServices.Available then
        begin
          R := ListBoxVertScrollRect;
          R.Top := ListBoxVertUpButtonRect.Bottom;
          R.Bottom := ListBoxVertDownButtonRect.Top;
          if R.Height > 0 then
          begin
            Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
            StyleServices.DrawElement(B.Canvas.Handle, Details, R);
          end;
          Details := StyleServices.GetElementDetails(FVSliderState);
          StyleServices.DrawElement(B.Canvas.Handle, Details,
            ListBoxVertSliderRect);
          Details := StyleServices.GetElementDetails(FVUpState);
          StyleServices.DrawElement(B.Canvas.Handle, Details,
            ListBoxVertUpButtonRect);
          Details := StyleServices.GetElementDetails(FVDownState);
          StyleServices.DrawElement(B.Canvas.Handle, Details,
            ListBoxVertDownButtonRect);
        end;

        MoveWindowOrg(B.Canvas.Handle, ListBoxVertScrollRect.Left,
          ListBoxVertScrollRect.Top);
        Canvas.Draw(ListBoxVertScrollRect.Left, ListBoxVertScrollRect.Top, B);
      finally
        B.Free;
      end;
    end;
  finally
    if DC <> 0 then
      Canvas.Handle := 0
    else
    begin
      ReleaseDC(FListHandle, Canvas.Handle);
      Canvas.Handle := 0;
    end;
    Canvas.Free;
  end;
end;

function TSysComboBoxStyleHook.IsDroppedDown: Boolean;
begin
  if Handle <> 0 then
    Result := LongBool(SendMessage(Handle, CB_GETDROPPEDSTATE, 0, 0))
  else
    Result := False;
end;

function TSysComboBoxStyleHook.GetButtonRect: TRect;
begin
  Result := SysControl.ClientRect;
  InflateRect(Result, -2, -2);
  if SysControl.BiDiMode <> bmRightToLeft then
    Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL) + 1
  else
    Result.Right := Result.Left + GetSystemMetrics(SM_CXVSCROLL) - 1;
end;

procedure TSysComboBoxStyleHook.HookListBox(AListHandle: HWnd);
begin
  if (AListHandle <> 0) and (FListBoxInstance = nil) then
  begin
    FListHandle := AListHandle;
    FListBoxInstance := MakeObjectInstance(ListBoxWndProc);
    FDefListBoxProc := Pointer(GetWindowLong(FListHandle, GWL_WNDPROC));
    SetWindowLong(FListHandle, GWL_WNDPROC, IntPtr(FListBoxInstance));
  end;
end;

function TSysComboBoxStyleHook.IsChildHandle(AHandle: HWnd): Boolean;
begin
  Result := (FEditHandle <> 0) and (FEditHandle = AHandle);

end;

function TSysComboBoxStyleHook.ListBoxBoundsRect: TRect;
begin
  GetWindowRect(FListHandle, Result);
end;

function TSysComboBoxStyleHook.ListBoxClientRect: TRect;
begin
  GetClientRect(FListHandle, Result);
end;

procedure TSysComboBoxStyleHook.ListBoxSetTimer(const ATimerCode: Integer);
begin
  if FListBoxTimerCode <> 0 then
    ListBoxStopTimer;
  FListBoxTimerCode := ATimerCode;
  if ATimerCode < 4 then
    SetTimer(FListHandle, 1, 300, nil)
  else
    SetTimer(FListHandle, 1, 50, nil);
end;

procedure TSysComboBoxStyleHook.ListBoxStopTimer;
begin
  FListBoxTimerCode := -1;
  KillTimer(FListHandle, 1);
end;

function TSysComboBoxStyleHook.ListBoxVertDownButtonRect: TRect;
begin
  Result := ListBoxVertScrollRect;
  if Result.Width > 0 then
    Result.Top := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB)
  else
    Result := TRect.Empty;
end;

function TSysComboBoxStyleHook.ListBoxVertScrollArea: TRect;
begin
  if GetWindowLong(FListHandle, GWL_STYLE) and WS_VSCROLL = 0 then
  begin
    Result := TRect.Empty;
    Exit;
  end;
  Result := ListBoxBoundsRect;
  OffsetRect(Result, -Result.Left, -Result.Top);
  if SysControl.BiDiMode <> bmRightToLeft then
    Result.Left := Result.Right - GetSystemMetrics(SM_CYVSCROLL) - 1
  else
    Result.Right := Result.Left + GetSystemMetrics(SM_CYVSCROLL);
end;

function TSysComboBoxStyleHook.ListBoxVertScrollRect: TRect;
begin
  Result := ListBoxBoundsRect;
  OffsetRect(Result, -Result.Left, -Result.Top);
  InflateRect(Result, -1, -1);
  OffsetRect(Result, 1, 1);
  if SysControl.BiDiMode <> TBidiModeDirection.bmRightToLeft then
    Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL)
  else
    Result.Right := Result.Left + GetSystemMetrics(SM_CXVSCROLL);
  if ListBoxBoundsRect.Height > 30 then OffsetRect(Result, -1, -1);

end;

function TSysComboBoxStyleHook.ListBoxVertSliderRect: TRect;
var
  i, LVisibleHeight, LTotalHeight, LSize, LTotalSize, LFinalHeight, LItemHeight,
    LBoundsHeight, LBorderHeight: Integer;
begin
  Result := ListBoxVertScrollRect;
  Result.Top := ListBoxVertUpButtonRect.Bottom;
  Result.Bottom := ListBoxVertDownButtonRect.Top;
  LSize := Result.Bottom - Result.Top;
  LTotalSize := SendMessage(FListHandle, LB_GETCOUNT, 0, 0) * LSize;
  if LTotalSize = 0 then
    Exit;
  Result.Top := Result.Top + Round((SendMessage(FListHandle, LB_GETTOPINDEX, 0,
    0) / SendMessage(FListHandle, LB_GETCOUNT, 0, 0)) * LSize);

  LTotalHeight := 1;
  FInvsibleCount := 0;
  LBoundsHeight := ListBoxBoundsRect.Height;
  for i := 0 to SendMessage(FListHandle, LB_GETCOUNT, 0, 0) - 1 do
  begin
    LItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, i, 0);
    LTotalHeight := LTotalHeight + LItemHeight;
    if (LTotalHeight > LBoundsHeight) and (FInvsibleCount = 0) then
      FInvsibleCount := SendMessage(FListHandle, LB_GETCOUNT, 0, 0) - i;
  end;

  LVisibleHeight := 0;
  for i := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0)
    to SendMessage(FListHandle, LB_GETCOUNT, 0, 0) - 1 do
  begin
    LVisibleHeight := LVisibleHeight + SendMessage(FListHandle,
      LB_GETITEMHEIGHT, i, 0);
    if Style <> csSimple then
      LBorderHeight := 2
    else
      LBorderHeight := 4;
    if LVisibleHeight >= ListBoxBoundsRect.Height - LBorderHeight then
      Break;
  end;

  Result.Bottom := Result.Top + Round((LVisibleHeight / LTotalHeight) * LSize);
  if (i = SendMessage(FListHandle, LB_GETCOUNT, 0, 0) - 1) and
    (Result.Bottom <> ListBoxVertDownButtonRect.Top) then
  begin
    LFinalHeight := Result.Height;
    Result.Bottom := ListBoxVertDownButtonRect.Top;
    Result.Top := Result.Bottom - LFinalHeight;
  end;
  FSliderSize := Round((LVisibleHeight / LTotalHeight) * LSize);
end;

function TSysComboBoxStyleHook.ListBoxVertTrackRect: TRect;
begin
  Result := ListBoxVertScrollRect;
  if Result.Width > 0 then
  begin
    Result.Top := Result.Top + GetSystemMetrics(SM_CYVTHUMB);
    Result.Bottom := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB);
  end
  else
    Result := TRect.Empty;
end;

function TSysComboBoxStyleHook.ListBoxVertTrackRectDown: TRect;
begin
  Result := ListBoxVertTrackRect;
  if (Result.Width > 0) and (ListBoxVertSliderRect.Height > 0) then
    Result.Top := ListBoxVertSliderRect.Bottom;
end;

function TSysComboBoxStyleHook.ListBoxVertTrackRectUp: TRect;
begin
  Result := ListBoxVertTrackRect;
  if (Result.Width > 0) and (ListBoxVertSliderRect.Height > 0) then
    Result.Bottom := ListBoxVertSliderRect.Top;
end;

function TSysComboBoxStyleHook.ListBoxVertUpButtonRect: TRect;
begin
  Result := ListBoxVertScrollRect;
  if Result.Width > 0 then
    Result.Top := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB)
  else
    Result := TRect.Empty;
end;

procedure TSysComboBoxStyleHook.ListBoxWndProc(var Msg: TMessage);
var
  MsgHandled: Boolean;

  procedure WMNCCalcSize(var Msg: TWMNCCalcSize);
  var
    LCalcSizeParams: PNCCalcSizeParams;
    LWindowPos: PWindowPos;
    LLeft, LRight, LTop, LBottom: Integer;
    LStyle, LNewStyle: Integer;
  begin
    LStyle := GetWindowLong(FListHandle, GWL_STYLE);
    if ((LStyle and WS_VSCROLL = WS_VSCROLL) or
      (LStyle and WS_HSCROLL = WS_HSCROLL)) then
    begin
      LNewStyle := LStyle and not WS_VSCROLL and not WS_HSCROLL;
      FIgnoreStyleChanged := True;
      SetWindowLong(FListHandle, GWL_STYLE, LNewStyle);
      Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle,
        TMessage(Msg).Msg, TMessage(Msg).wParam, TMessage(Msg).LPARAM);
      SetWindowLong(FListHandle, GWL_STYLE, LStyle);
      FIgnoreStyleChanged := False;
    end
    else
      Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle,
        TMessage(Msg).Msg, TMessage(Msg).wParam, TMessage(Msg).LPARAM);

    if (Msg.CalcValidRects) then
    begin
      LCalcSizeParams := Msg.CalcSize_Params;
      if SysControl.BiDiMode <> bmRightToLeft then
      begin
        LLeft := 1;
        if LStyle and WS_VSCROLL = WS_VSCROLL then
          LRight := ListBoxVertScrollRect.Width + 1
        else
          LRight := 1;
      end
      else
      begin
        LRight := 1;
        if LStyle and WS_VSCROLL = WS_VSCROLL then
          LLeft := ListBoxVertScrollRect.Width + 1
        else
          LLeft := 1;
      end;

      LTop := 1;
      LBottom := 1;
      LWindowPos := LCalcSizeParams.lppos;
      with LCalcSizeParams^.rgrc[0] do
      begin
        Left := LWindowPos^.X;
        Top := LWindowPos^.Y;
        Right := LWindowPos^.X + LWindowPos^.cx;
        Bottom := LWindowPos^.Y + LWindowPos^.cy;
        Left := Left + LLeft;
        Top := Top + LTop;
        Right := Right - LRight;
        Bottom := Bottom - LBottom;
      end;
      LCalcSizeParams^.rgrc[1] := LCalcSizeParams^.rgrc[0];
      Msg.CalcSize_Params := LCalcSizeParams;
      Msg.Result := WVR_VALIDRECTS;
    end;
    Msg.Result := 0;
    MsgHandled := True;
  end;

  procedure WMMouseWheel(var Msg: TWMMouseWheel);
  var
    Index: Integer;
    R: TRect;
  begin
    SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
    Index := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0);
    if Msg.WheelDelta < 0 then
      Inc(Index)
    else
      Dec(Index);
    SendMessage(FListHandle, LB_SETTOPINDEX, Index, 0);
    SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
    R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
    RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
    DrawListBoxVertScroll(0);
    MsgHandled := True;
  end;

  procedure WMNCLButtonDblClk(var Msg: TWMMouse);
  var
    R: TRect;
    P: TPoint;
  begin
    P := Point(Msg.XPos, Msg.YPos);
    if ListBoxVertScrollArea.Contains(P) then
    begin
      if ListBoxVertUpButtonRect.Contains(Point(Msg.XPos, Msg.YPos)) then
      begin
        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, SendMessage(FListHandle,
          LB_GETTOPINDEX, 0, 0) - 1, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
        DrawListBoxVertScroll(0);
        Exit;
      end;

      if ListBoxVertDownButtonRect.Contains(Point(Msg.XPos, Msg.YPos)) then
      begin
        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, SendMessage(FListHandle,
          LB_GETTOPINDEX, 0, 0) + 1, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
        DrawListBoxVertScroll(0);
        Exit;
      end;
    end;
    MsgHandled := True;
  end;

  procedure WMLButtonDown(var Msg: TWMMouse);
  var
    P: TPoint;
    R: TRect;
    ItemHeight, VisibleCount, TopIndex: Integer;
  begin
    MsgHandled := False;
    P := Point(Msg.XPos, Msg.YPos);
    if SysControl.BiDiMode = bmRightToLeft then
      P.X := -P.X;
    FDownPos := P;
    if ListBoxVertScrollArea.Contains(P) then
    begin
      if Style = csSimple then
        SetCapture(FListHandle);
      FDownPos := P;
      if ListBoxVertTrackRectUp.Contains(P) then
      begin
        ItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
        if ItemHeight > 0 then
          VisibleCount := ListBoxClientRect.Height div ItemHeight
        else
          VisibleCount := 0;
        TopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) -
          VisibleCount + 1;
        if TopIndex < 0 then
          TopIndex := 0;
        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, TopIndex, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
        DrawListBoxVertScroll(0);
        ListBoxSetTimer(3);
      end
      else if ListBoxVertTrackRectDown.Contains(P) then
      begin
        ItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
        if ItemHeight > 0 then
          VisibleCount := ListBoxClientRect.Height div ItemHeight
        else
          VisibleCount := 0;
        TopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) +
          VisibleCount - 1;
        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, TopIndex, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
        DrawListBoxVertScroll(0);
        ListBoxSetTimer(4);
      end
      else if ListBoxVertSliderRect.Contains(P) then
      begin
        FVSliderState := tsThumbBtnVertPressed;
        FDownSliderPos := FDownPos.Y - ListBoxVertSliderRect.Top;
        DrawListBoxVertScroll(0);
      end
      else if ListBoxVertDownButtonRect.Contains(P) then
      begin
        FListBoxDownBtnDown := True;
        FVDownState := tsArrowBtnDownPressed;
        DrawListBoxVertScroll(0);

        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, SendMessage(FListHandle,
          LB_GETTOPINDEX, 0, 0) + 1, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);

        ListBoxSetTimer(2);
      end
      else if ListBoxVertUpButtonRect.Contains(P) then
      begin
        FListBoxUpBtnDown := True;
        FVUpState := tsArrowBtnUpPressed;
        DrawListBoxVertScroll(0);

        SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
        SendMessage(FListHandle, LB_SETTOPINDEX, SendMessage(FListHandle,
          LB_GETTOPINDEX, 0, 0) - 1, 0);
        SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
        R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
        RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);

        ListBoxSetTimer(1);
      end;
      MsgHandled := True;
    end
    else
    begin
      if (FVSliderState <> tsThumbBtnVertNormal) or
        (FVUpState <> tsArrowBtnUpNormal) or
        (FVDownState <> tsArrowBtnDownNormal) then
      begin
        FVSliderState := tsArrowBtnUpNormal;
        FVUpState := tsArrowBtnUpNormal;
        FVDownState := tsArrowBtnDownNormal;
        DrawListBoxVertScroll(0);
      end;
    end;
    FOldIdx := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0);
  end;

  procedure WMMouseMove(var Msg: TWMMouse);
  var
    P: TPoint;
    NewIndex, Index: Integer;
    Dist: Integer;
    R: TRect;
  begin
    P := Point(Msg.XPos, Msg.YPos);
    if SysControl.BiDiMode = bmRightToLeft then
      P.X := -P.X;

    FMovePos := P;
    if (FVSliderState = tsThumbBtnVertPressed) then
    begin
      Index := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0);
      Dist := (ListBoxVertScrollRect.Height - ListBoxVertUpButtonRect.Height -
        ListBoxVertDownButtonRect.Height - ListBoxVertSliderRect.Height);
      if Dist > 0 then
      begin
        NewIndex :=
          Round((((FMovePos.Y - FDownSliderPos - ListBoxVertUpButtonRect.Bottom)
          / Dist) * FInvsibleCount));
        if NewIndex <> Index then
        begin
          if NewIndex < 0 then
            NewIndex := 0;
          if NewIndex >= SendMessage(FListHandle, LB_GETCOUNT, 0, 0) then
            NewIndex := SendMessage(FListHandle, LB_GETCOUNT, 0, 0) - 1;
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX, NewIndex, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
          DrawListBoxVertScroll(0);
        end;
      end;
      MsgHandled := True;
      Exit;
    end;

    if FListBoxUpBtnDown and not ListBoxVertUpButtonRect.Contains(P) and
      (FVUpState = tsArrowBtnUpPressed) then
    begin
      FVUpState := tsArrowBtnUpNormal;
      DrawListBoxVertScroll(0);
      ListBoxStopTimer;
      Exit;
    end;

    if FListBoxUpBtnDown and ListBoxVertUpButtonRect.Contains(P) and
      (FVUpState = tsArrowBtnUpNormal) then
    begin
      FVUpState := tsArrowBtnUpPressed;
      DrawListBoxVertScroll(0);
      ListBoxSetTimer(5);
      Exit;
    end;

    if FListBoxDownBtnDown and not ListBoxVertDownButtonRect.Contains(P) and
      (FVDownState = tsArrowBtnDownPressed) then
    begin
      FVDownState := tsArrowBtnDownNormal;
      DrawListBoxVertScroll(0);
      ListBoxStopTimer;
      Exit;
    end;

    if FListBoxDownBtnDown and ListBoxVertDownButtonRect.Contains(P) and
      (FVDownState = tsArrowBtnDownNormal) then
    begin
      FVDownState := tsArrowBtnDownPressed;
      DrawListBoxVertScroll(0);
      ListBoxSetTimer(6);
      Exit;
    end;

    if ListBoxVertScrollArea.Contains(P) then
    begin
      if ListBoxVertSliderRect.Contains(P) and
        (FVSliderState = tsThumbBtnVertNormal) then
      begin
        FVSliderState := tsThumbBtnVertHot;
        DrawListBoxVertScroll(0);
      end
      else if not ListBoxVertSliderRect.Contains(P) and
        (FVSliderState = tsThumbBtnVertHot) then
      begin
        FVSliderState := tsThumbBtnVertNormal;
        DrawListBoxVertScroll(0);
      end
      else if ListBoxVertUpButtonRect.Contains(P) and
        (FVUpState = tsArrowBtnUpNormal) then
      begin
        FVUpState := tsArrowBtnUpHot;
        DrawListBoxVertScroll(0);
      end
      else if not ListBoxVertUpButtonRect.Contains(P) and
        (FVUpState = tsArrowBtnUpHot) then
      begin
        FVUpState := tsArrowBtnUpNormal;
        DrawListBoxVertScroll(0);
      end
      else if ListBoxVertDownButtonRect.Contains(P) and
        (FVDownState = tsArrowBtnDownNormal) then
      begin
        FVDownState := tsArrowBtnDownHot;
        DrawListBoxVertScroll(0);
      end
      else if not ListBoxVertDownButtonRect.Contains(P) and
        (FVDownState = tsArrowBtnDownHot) then
      begin
        FVDownState := tsArrowBtnDownNormal;
        DrawListBoxVertScroll(0);
      end;
      MsgHandled := True;
    end
    else
    begin
      if (FVSliderState <> tsThumbBtnVertNormal) or
        (FVUpState <> tsArrowBtnUpNormal) or (FVUpState <> tsArrowBtnDownNormal)
      then
      begin
        if FListBoxTimerCode <> 0 then
          ListBoxStopTimer;
        FVSliderState := tsThumbBtnVertNormal;
        FVUpState := tsArrowBtnUpNormal;
        FVDownState := tsArrowBtnDownNormal;
        DrawListBoxVertScroll(0);
      end;
    end;
  end;

  procedure WMLButtonUp(var Msg: TWMMouse);
  var
    P: TPoint;
  begin
    FListBoxUpBtnDown := False;
    FListBoxDownBtnDown := False;
    FListBoxTrackUpDown := False;
    FListBoxTrackDownDown := False;

    P := Point(Msg.XPos, Msg.YPos);
    if SysControl.BiDiMode = bmRightToLeft then
      P.X := -P.X;

    if (Style = csSimple) and ListBoxVertScrollArea.Contains(FDownPos) then
      ReleaseCapture;

    if ListBoxVertSliderRect.Contains(P) then
      FVSliderState := tsThumbBtnVertHot
    else
      FVSliderState := tsThumbBtnVertNormal;

    if ListBoxVertUpButtonRect.Contains(P) then
      FVUpState := tsArrowBtnUpHot
    else
      FVUpState := tsArrowBtnUpNormal;

    if ListBoxVertDownButtonRect.Contains(P) then
      FVDownState := tsArrowBtnDownHot
    else
      FVDownState := tsArrowBtnDownNormal;

    DrawListBoxVertScroll(0);

    if FListBoxTimerCode <> 0 then
      ListBoxStopTimer;

    MsgHandled := ListBoxVertScrollArea.Contains(P);
  end;

  procedure WMNCLButtonDown(var Msg: TWMMouse);
  var
    P: TPoint;
  begin
    if Style <> csSimple then
      SetCapture(FListHandle);
    P := Point(Msg.XPos, Msg.YPos);
    ScreenToClient(FListHandle, P);
    with P do
    begin
      Msg.XPos := X;
      Msg.YPos := Y;
    end;
    WMLButtonDown(Msg);
    MsgHandled := True;
  end;

  procedure WMPrint(var Msg: TMessage);
  var
    SaveIndex: Integer;
    Canvas: TCanvas;
    R: TRect;
  begin
    Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle, Msg.Msg,
      Msg.wParam, Msg.LPARAM);

    if (Msg.LPARAM and PRF_NONCLIENT = PRF_NONCLIENT) and (Msg.wParam > 0) then
    begin
      SaveIndex := 0;
      Canvas := TCanvas.Create;
      try
        SaveIndex := SaveDC(Msg.wParam);
        Canvas.Handle := Msg.wParam;
        GetWindowRect(FListHandle, R);
        OffsetRect(R, -R.Left, -R.Top);
        ExcludeClipRect(Canvas.Handle, R.Left + 2, R.Top + 2, R.Right - 2,
          R.Bottom - 2);
        PaintListBoxBorder(Canvas, R);
      finally
        if SaveIndex <> 0 then
          RestoreDC(Canvas.Handle, SaveIndex);
        Canvas.Handle := 0;
        Canvas.Free;
      end;
      DrawListBoxVertScroll(Msg.wParam);
    end;
    MsgHandled := True;
  end;

  procedure WMTimer(var Msg: TMessage);
  var
    R: TRect;
    ItemHeight, VisibleCount, TopIndex: Integer;
  begin
    case FListBoxTimerCode of
      1:
        ListBoxSetTimer(5);
      2:
        ListBoxSetTimer(6);
      3:
        ListBoxSetTimer(7);
      4:
        ListBoxSetTimer(8);
      5:
        begin
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX,
            SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) - 1, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
          DrawListBoxVertScroll(0);
        end;
      6:
        begin
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX,
            SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) + 1, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
          DrawListBoxVertScroll(0);
        end;
      7:
        begin
          if ListBoxVertSliderRect.Contains(FMovePos) or
            (FMovePos.Y > ListBoxVertSliderRect.Bottom) then
          begin
            ListBoxStopTimer;
            Exit;
          end;
          ItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
          if ItemHeight > 0 then
            VisibleCount := ListBoxClientRect.Height div ItemHeight
          else
            VisibleCount := 0;
          TopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) -
            VisibleCount + 1;
          if TopIndex < 0 then
            TopIndex := 0;
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX, TopIndex, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
          DrawListBoxVertScroll(0);
        end;
      8:
        begin
          if ListBoxVertSliderRect.Contains(FMovePos) or
            (FMovePos.Y < ListBoxVertSliderRect.Top) then
          begin
            ListBoxStopTimer;
            Exit;
          end;
          ItemHeight := SendMessage(FListHandle, LB_GETITEMHEIGHT, 0, 0);
          if ItemHeight > 0 then
            VisibleCount := ListBoxClientRect.Height div ItemHeight
          else
            VisibleCount := 0;
          TopIndex := SendMessage(FListHandle, LB_GETTOPINDEX, 0, 0) +
            VisibleCount - 1;
          SendMessage(FListHandle, WM_SETREDRAW, 0, 0);
          SendMessage(FListHandle, LB_SETTOPINDEX, TopIndex, 0);
          SendMessage(FListHandle, WM_SETREDRAW, 1, 0);
          R := Rect(0, 0, ListBoxBoundsRect.Width, ListBoxBoundsRect.Height);
          RedrawWindow(FListHandle, @R, 0, RDW_INVALIDATE or RDW_ERASE);
          DrawListBoxVertScroll(0);
        end;
    end;
  end;

begin

  MsgHandled := False;
  if ListBoxVertScrollArea.Height = 0 then
  begin
    case Msg.Msg of
      WM_NCCALCSIZE:
        WMNCCalcSize(TWMNCCalcSize(Msg));
      WM_NCPAINT:
        begin
          DrawListBoxBorder;
          MsgHandled := True;
        end;
    end;
  end
  else
    case Msg.Msg of
      WM_NCHITTEST:
        if Style = csSimple then
        begin
          Msg.Result := HTCLIENT;
          MsgHandled := True;
        end;
      WM_MOUSELEAVE, WM_NCMOUSELEAVE:
        if Style = csSimple then
        begin
          FVSliderState := tsThumbBtnVertNormal;
          FVUpState := tsArrowBtnUpNormal;
          FVDownState := tsArrowBtnDownNormal;
          DrawListBoxVertScroll(0);
        end;
      WM_TIMER:
        WMTimer(Msg);
      WM_UpdateUIState:
        MsgHandled := True;
      WM_NCCALCSIZE:
        WMNCCalcSize(TWMNCCalcSize(Msg));
      WM_MOUSEWHEEL:
        WMMouseWheel(TWMMouseWheel(Msg));
      WM_NCLButtonDblClk:
        WMNCLButtonDblClk(TWMMouse(Msg));
      WM_LBUTTONDOWN:
        WMLButtonDown(TWMMouse(Msg));
      WM_MOUSEMOVE:
        WMMouseMove(TWMMouse(Msg));
      WM_LBUTTONUP:
        WMLButtonUp(TWMMouse(Msg));
      WM_NCLButtonDown:
        WMNCLButtonDown(TWMMouse(Msg));
      WM_NCLButtonUp, WM_NCMouseMove:
        MsgHandled := True;
      WM_PRINT:
        WMPrint(Msg);
      WM_KEYDOWN, WM_KEYUP:
        begin
          Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle, Msg.Msg,
            Msg.wParam, Msg.LPARAM);
          DrawListBoxVertScroll(0);
          MsgHandled := True;
        end;
      WM_NCPAINT:
        begin
          DrawListBoxBorder;
          DrawListBoxVertScroll(0);
          MsgHandled := True;
        end;
      LB_SETTOPINDEX:
        begin
          Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle, Msg.Msg,
            Msg.wParam, Msg.LPARAM);
          DrawListBoxVertScroll(0);
          MsgHandled := True;
        end;
      WM_STYLECHANGED, WM_STYLECHANGING:
        if FIgnoreStyleChanged then
        begin
          Msg.Result := 0;
          MsgHandled := True;
        end;

    end;
  if not MsgHandled then
    Msg.Result := CallWindowProc(FDefListBoxProc, FListHandle, Msg.Msg,
      Msg.wParam, Msg.LPARAM);
end;

procedure TSysComboBoxStyleHook.MouseEnter;
begin
  inherited;
  Invalidate;
end;

procedure TSysComboBoxStyleHook.MouseLeave;
begin
  inherited;
  if not DroppedDown and FMouseOnButton then
  begin
    FMouseOnButton := False;
    Invalidate;
  end
end;

procedure TSysComboBoxStyleHook.PaintBorder(Canvas: TCanvas);
var
  R, ControlRect, EditRect, ListRect: TRect;
  DrawState: TThemedComboBox;
  BtnDrawState: TThemedComboBox;
  Details: TThemedElementDetails;
  Buffer: TBitmap;
begin
  if not StyleServices.Available then
    Exit;

  if not SysControl.Enabled then
    BtnDrawState := tcDropDownButtonDisabled
  else if DroppedDown then
    BtnDrawState := tcDropDownButtonPressed
  else if (FMouseOnButton and MouseInControl) then
    BtnDrawState := tcDropDownButtonHot
  else
    BtnDrawState := tcDropDownButtonNormal;

  if not SysControl.Enabled then
    DrawState := tcBorderDisabled
  else if SysControl.Focused then
    DrawState := tcBorderFocused
  else if MouseInControl then
    DrawState := tcBorderHot
  else
    DrawState := tcBorderNormal;

  Buffer := TBitmap.Create;
  Buffer.SetSize(SysControl.Width, SysControl.Height);
  try
    R := Rect(0, 0, Buffer.Width, Buffer.Height);
    // draw border + client in buffer
    Details := StyleServices.GetElementDetails(DrawState);
    if (Style = csSimple) and (FListHandle <> 0) then
    begin
      GetWindowRect(FListHandle, ListRect);
      GetWindowRect(Handle, ControlRect);
      R.Bottom := ListRect.Top - ControlRect.Top;
      StyleServices.DrawElement(Buffer.Canvas.Handle, Details, R);
      R := Rect(0, SysControl.Height - (ControlRect.Bottom - ListRect.Bottom),
        SysControl.Width, SysControl.Height);
      with Buffer.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := StyleServices.GetSystemColor(clBtnFace);
        FillRect(R);
      end;
      R := Rect(0, 0, Buffer.Width, Buffer.Height);
      R.Bottom := ListRect.Top - ControlRect.Top;
    end
    else
      StyleServices.DrawElement(Buffer.Canvas.Handle, Details, R);

    // if not (seClient in SysControl.StyleElements) and (FEditHandle = 0) then
    // begin
    // R := SysControl.ClientRect;
    // InflateRect(R, -3, -3);
    // R.Right := ButtonRect.Left - 2;
    // with Buffer.Canvas do
    // begin
    // Brush.Color := TWinControlClass(Control).Color;
    // FillRect(R);
    // end;
    // end;
    // draw button in buffer
    if Style <> csSimple then
    begin
      Details := StyleServices.GetElementDetails(BtnDrawState);
      StyleServices.DrawElement(Buffer.Canvas.Handle, Details, ButtonRect);
    end;
    // calculation of exclude area for drawing buffer
    if (SendMessage(Handle, CB_GETCURSEL, 0, 0) >= 0) and (FEditHandle = 0) then
    begin
      R := SysControl.ClientRect;
      InflateRect(R, -3, -3);
      R.Right := ButtonRect.Left - 2;
      ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    end
    else if FEditHandle <> 0 then
    begin
      GetWindowRect(Handle, R);
      GetWindowRect(FEditHandle, EditRect);
      OffsetRect(EditRect, -R.Left, -R.Top);
      with EditRect do
        ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    end;
    // draw buffer
    Canvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
  end;
end;

procedure TSysComboBoxStyleHook.PaintListBoxBorder(Canvas: TCanvas;
  const R: TRect);
begin
  with Canvas do
  begin
    Brush.Color := StyleServices.GetSystemColor(clWindowFrame);
    FillRect(R);
  end;
end;

function TSysComboBoxStyleHook.Style: TComboBoxStyle;
const
  ComboBoxStyles: array [TComboBoxStyle] of DWORD = (CBS_DROPDOWN, CBS_SIMPLE,
    CBS_DROPDOWNLIST, CBS_DROPDOWNLIST or CBS_OWNERDRAWFIXED,
    CBS_DROPDOWNLIST or CBS_OWNERDRAWVARIABLE);
var
  LStyle: Cardinal;
begin
  if Handle <> 0 then
  begin
    LStyle := GetWindowLong(Handle, GWL_STYLE);
    Result := csDropDown;
    if LStyle and ComboBoxStyles[csDropDown] = ComboBoxStyles[csDropDown] then
      Result := csDropDown;
    if LStyle and ComboBoxStyles[csSimple] = ComboBoxStyles[csSimple] then
      Result := csSimple;
    if LStyle and ComboBoxStyles[csDropDownList] = ComboBoxStyles[csDropDownList]
    then
      Result := csDropDownList;
    if LStyle and ComboBoxStyles[csOwnerDrawFixed] = ComboBoxStyles
      [csOwnerDrawFixed] then
      Result := csOwnerDrawFixed;
    if LStyle and ComboBoxStyles[csOwnerDrawVariable] = ComboBoxStyles
      [csOwnerDrawVariable] then
      Result := csOwnerDrawVariable;
  end
  else
    Result := csDropDown;
end;
{$HINTS OFF}

procedure TSysComboBoxStyleHook.UpdateColors;
const
  ColorStates: array [Boolean] of TStyleColor = (scComboBoxDisabled,
    scComboBox);
  FontColorStates: array [Boolean] of TStyleFont = (sfComboBoxItemDisabled,
    sfComboBoxItemNormal);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  Color := StyleServices.GetStyleColor(ColorStates[SysControl.Enabled]);
{$IF CompilerVersion > 23}
  if OverrideFont then
    FontColor := StyleServices.GetStyleFontColor(FontColorStates[True])
  else
    FontColor := clWindowText;
{$ELSE}
  FontColor := StyleServices.GetStyleFontColor
    (FontColorStates[SysControl.Enabled]);
  Brush.Color := LStyle.GetStyleColor(ColorStates[SysControl.Enabled]);
{$IFEND}
end;
{$HINTS ON}

procedure TSysComboBoxStyleHook.WMCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = CBN_SELENDCANCEL) or
    (Message.NotifyCode = CBN_SELENDOK) or (Message.NotifyCode = CBN_CLOSEUP) or
    (Message.NotifyCode = CBN_DROPDOWN) or (Message.NotifyCode = CBN_SELCHANGE)
  then
  begin
    if FListBoxTimerCode <> 0 then
      ListBoxStopTimer;
    FMouseOnButton := False;
    Invalidate;
  end;
end;

procedure TSysComboBoxStyleHook.WMDrawItem(var Message: TWMDrawItem);
begin
  CallDefaultProc(TMessage(Message));
  Handled := True;
end;

procedure TSysComboBoxStyleHook.WMMouseMove(var Message: TWMMouse);
var
  P: TPoint;
  R: TRect;
  FOldMouseOnButton: Boolean;
begin
  CallDefaultProc(TMessage(Message));
  inherited;

  P := Point(Message.XPos, Message.YPos);
  FOldMouseOnButton := FMouseOnButton;
  R := ButtonRect;
  if R.Contains(P) then
    FMouseOnButton := True
  else
    FMouseOnButton := False;

  if FOldMouseOnButton <> FMouseOnButton then
    InvalidateRect(Handle, @R, False);

  Handled := True;
end;

procedure TSysComboBoxStyleHook.WMPaint(var Message: TMessage);
var
  R: TRect;
  Canvas: TCanvas;
  PS: TPaintStruct;
  SaveIndex: Integer;
  DC: HDC;
  //LItemIndex: UINT;
  LDetails: TThemedElementDetails;
begin
  DC := Message.wParam;
  Canvas := TCanvas.Create;
  try
    if DC = 0 then
      Canvas.Handle := BeginPaint(Handle, PS)
    else
      Canvas.Handle := DC;

    SaveIndex := SaveDC(Canvas.Handle);
    try
      PaintBorder(Canvas);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;

    if (Style <> csSimple) and (FEditHandle = 0) then
    begin
      R := SysControl.ClientRect;
      InflateRect(R, -3, -3);
      if SysControl.BiDiMode <> bmRightToLeft then
        R.Right := ButtonRect.Left - 1
      else
        R.Left := ButtonRect.Right + 1;
      SaveIndex := SaveDC(Canvas.Handle);
      try
        IntersectClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
        //LItemIndex := UINT(SendMessage(SysControl.Handle, CB_GETCURSEL, 0, 0));
        Canvas.Brush.Color := StyleServices.GetSystemColor(clWindow);
        Canvas.FillRect(R);
        if (SysControl.Style and CBS_OWNERDRAWFIXED = CBS_OWNERDRAWFIXED) or
          (SysControl.Style and CBS_OWNERDRAWVARIABLE = CBS_OWNERDRAWVARIABLE)
        then
        begin
          //DrawItem(Canvas, LItemIndex, R, Focused);
          LDetails := StyleServices.GetElementDetails
            (TThemedComboBox.tcComboBoxDontCare);
          DrawText(Canvas.Handle, LDetails, SysControl.Text, R,
            [tfLeft, tfVerticalCenter, tfSingleLine]);
        end
        else
        begin
          LDetails := StyleServices.GetElementDetails
            (TThemedComboBox.tcComboBoxDontCare);
          DrawText(Canvas.Handle, LDetails, SysControl.Text, R,
            [tfLeft, tfVerticalCenter, tfSingleLine]);
        end;
      finally
        RestoreDC(Canvas.Handle, SaveIndex);
      end;
    end;

  finally
    Canvas.Handle := 0;
    Canvas.Free;
    if DC = 0 then
      EndPaint(Handle, PS);
  end;
  Handled := True;
end;

procedure TSysComboBoxStyleHook.WMParentNotify(var Message: TMessage);
begin
  if (FListHandle = 0) and (LoWord(Message.wParam) = WM_CREATE) then
  begin
    if (Message.LPARAM <> 0) and (FListBoxInstance = nil) then
      HookListBox(Message.LPARAM);
  end
  else if (FEditHandle = 0) and (LoWord(Message.wParam) = WM_CREATE) then
    FEditHandle := Message.LPARAM;
end;

procedure TSysComboBoxStyleHook.WndProc(var Message: TMessage);
const
  States: array [Boolean] of TStyleColor = (scEditDisabled, scComboBox);
begin
  case Message.Msg of

    CB_SETCURSEL, WM_KILLFOCUS:
      begin
        SetRedraw(False); // do not allow default drawing .
        CallDefaultProc(Message);
        SetRedraw(True); // allow vcl style drawing .
        Invalidate;
        Exit;
      end;

    WM_CTLCOLORMSGBOX .. WM_CTLCOLORSTATIC,
      CN_CTLCOLORMSGBOX .. CN_CTLCOLORSTATIC:
      begin
        SetTextColor(Message.wParam, ColorToRGB(FontColor));
        Brush.Color := StyleServices.GetStyleColor(States[SysControl.Enabled]);
        SetBkColor(Message.wParam, ColorToRGB(Brush.Color));
        Message.Result := LRESULT(Brush.Handle);
      end;

    CM_ENABLEDCHANGED:
      begin
        UpdateColors;
        CallDefaultProc(Message); // Allow control to handle message
      end;
    CM_FOCUSCHANGED:
      begin
        Invalidate;
        // Handled := False; // Allow control to handle message
        CallDefaultProc(Message);
      end;
  else
    inherited WndProc(Message);
  end;
end;

{ TSysStaticStyleHook }

constructor TSysStaticStyleHook.Create(AHandle: THandle);
var
  Style: DWORD;
begin
  Style := GetWindowLongPtr(AHandle, GWL_STYLE);
  if (Style and SS_ICON <> SS_ICON) and (Style and SS_BITMAP <> SS_BITMAP) then

    inherited;
  FUpdatedColor := 0;

{$IF CompilerVersion > 23}
  StyleElements := [seFont, seBorder, seClient];
{$ELSE}
  OverridePaint := True;
  OverridePaintNC := True;
  OverrideFont := True;
{$IFEND}
  UpdateColors;
end;

destructor TSysStaticStyleHook.Destroy;
begin
  inherited;
end;

function TSysStaticStyleHook.GetIsFrameOrLine: Boolean;
begin
  with SysControl do
    Result :=
      (Style and SS_ETCHEDFRAME = SS_ETCHEDFRAME) or
      (Style and SS_ETCHEDHORZ = SS_ETCHEDHORZ) or
      (Style and SS_SUNKEN = SS_SUNKEN) or
      (Style and SS_ETCHEDVERT = SS_ETCHEDVERT);
end;

function TSysStaticStyleHook.GetIsText: Boolean;
begin
  with SysControl do
    Result := (Style and SS_ICON <> SS_ICON) and
      (Style and SS_BITMAP <> SS_BITMAP) and
      (Style and SS_GRAYRECT <> SS_GRAYRECT) and
      (Style and SS_GRAYFRAME <> SS_GRAYFRAME) and
      (Style and SS_OWNERDRAW <> SS_OWNERDRAW) and
      (Style and SS_REALSIZEIMAGE <> SS_REALSIZEIMAGE) and
      (Style and SS_ICON <> SS_ICON) and (Style and SS_USERITEM <> SS_USERITEM)
      and (Style and SS_REALSIZEIMAGE <> SS_REALSIZEIMAGE) and
      (Style and SS_SIMPLE <> SS_SIMPLE);
end;

function TSysStaticStyleHook.GetTextFormat: TTextFormat;
const
 SS_EDITCONTROL = $2000;
begin
  Result := [tfHidePrefix];
  with SysControl do
  begin
    if Style and SS_LEFT = SS_LEFT then
      include(Result, tfLeft)
    else if Style and SS_RIGHT = SS_RIGHT then
      include(Result, tfRight)
    else if Style and SS_CENTER = SS_CENTER then
      include(Result, tfCenter);

    if Style and SS_ENDELLIPSIS = SS_ENDELLIPSIS then
      include(Result, tfEndEllipsis);

    if Style and SS_PATHELLIPSIS = SS_PATHELLIPSIS then
      include(Result, tfPathEllipsis);

    if Style and SS_WORDELLIPSIS = SS_WORDELLIPSIS then
      include(Result, tfWordEllipsis);

    if Style and SS_NOPREFIX = SS_NOPREFIX then
      include(Result, tfNoPrefix);

    if Style and SS_EDITCONTROL = SS_EDITCONTROL then
      include(Result, tfEditControl);

    if not(Style and SS_ENDELLIPSIS = SS_ENDELLIPSIS) and
      not(Style and SS_PATHELLIPSIS = SS_PATHELLIPSIS) and
      not(Style and SS_WORDELLIPSIS = SS_WORDELLIPSIS) then
      include(Result, tfWordBreak);
  end;
end;

procedure TSysStaticStyleHook.Paint(Canvas: TCanvas);
const
  States: array [Boolean] of TThemedTextLabel = (ttlTextLabelDisabled,
    ttlTextLabelNormal);
var
  LDetails: TThemedElementDetails;
  LRect: TRect;
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
  DrawText(Canvas.Handle, LDetails, SysControl.Text, LRect, TextFormat);
end;

procedure TSysStaticStyleHook.PaintNC(Canvas: TCanvas);
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

procedure TSysStaticStyleHook.UpdateColors;
const
  ColorStates: array [Boolean] of TStyleColor = (scEditDisabled, scEdit);
  FontColorStates: array [Boolean] of TStyleFont = (sfEditBoxTextDisabled,
    sfEditBoxTextNormal);
begin
  Color := StyleServices.GetStyleColor(scWindow);
  FontColor := StyleServices.GetSystemColor(clWindowText);
  //Addlog(Format('UpdateColors Handle %d Color %d FontColor %d ',[SysControl.Handle, Color, FontColor]));
end;

procedure TSysStaticStyleHook.WndProc(var Message: TMessage);
begin
   //Addlog(Format('TSysStaticStyleHook $0x%x %s', [SysControl.Handle, WM_To_String(Message.Msg)]));
  case Message.Msg of

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

{ TSysCheckBoxStyleHook }
function RectVCenter(var R: TRect; Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);

  Result := R;
end;

procedure TSysCheckBoxStyleHook.BMSetCheck(var Message: TMessage);
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);
  Invalidate;
  Handled := True;
end;

constructor TSysCheckBoxStyleHook.Create(AHandle: THandle);
begin
  inherited;
  OverridePaint := True;
  OverrideEraseBkgnd := True;
  // DoubleBuffered := True;
end;

function TSysCheckBoxStyleHook.GetDrawState(State: TSysCheckBoxState)
  : TThemedButton;
begin
  Result := tbButtonDontCare;

  if not SysControl.Enabled then
    case State of
      cbUnchecked:
        Result := tbCheckBoxUncheckedDisabled;
      cbChecked:
        Result := tbCheckBoxCheckedDisabled;
      cbGrayed:
        Result := tbCheckBoxMixedDisabled;
    end
  else if Pressed and MouseInControl then
    case State of
      cbUnchecked:
        Result := tbCheckBoxUncheckedPressed;
      cbChecked:
        Result := tbCheckBoxCheckedPressed;
      cbGrayed:
        Result := tbCheckBoxMixedPressed;
    end
  else if MouseInControl then
    case State of
      cbUnchecked:
        Result := tbCheckBoxUncheckedHot;
      cbChecked:
        Result := tbCheckBoxCheckedHot;
      cbGrayed:
        Result := tbCheckBoxMixedHot;
    end
  else
    case State of
      cbUnchecked:
        Result := tbCheckBoxUncheckedNormal;
      cbChecked:
        Result := tbCheckBoxCheckedNormal;
      cbGrayed:
        Result := tbCheckBoxMixedNormal;
    end;
end;

procedure TSysCheckBoxStyleHook.MouseEnter;
begin
  inherited;
  Invalidate;
  Handled := True;
end;

procedure TSysCheckBoxStyleHook.MouseLeave;
begin
  inherited;
  Invalidate;
  Handled := True;
end;

procedure TSysCheckBoxStyleHook.Paint(Canvas: TCanvas);
var
  State: TSysCheckBoxState;
  Details: TThemedElementDetails;
  R: TRect;
  Spacing: Integer;
  BoxSize: TSize;
  LCaption: string;
  LRect: TRect;
  ElementSize: TElementSize;
begin
  if StyleServices.Available then
  begin
    State := TSysCheckBoxState(SendMessage(Handle, BM_GETCHECK, 0, 0));
    Details := StyleServices.GetElementDetails(GetDrawState(State));

    Spacing := 3;
    LRect := System.Classes.Rect(0, 0, 20, 20);
    ElementSize := esActual;
    R := SysControl.ClientRect;
    with StyleServices do
      if not GetElementSize(Canvas.Handle,
        GetElementDetails(tbCheckBoxCheckedNormal), LRect, ElementSize, BoxSize)
      then
      begin
        BoxSize.cx := GetSystemMetrics(SM_CXMENUCHECK);
        BoxSize.cy := GetSystemMetrics(SM_CYMENUCHECK);
      end;
    if not RightAlignment then
    begin
      R := Rect(0, 0, BoxSize.cx, BoxSize.cy);
      RectVCenter(R, Rect(0, 0, SysControl.Width, SysControl.Height));
    end
    else
    begin
      R := Rect(SysControl.Width - BoxSize.cx - 1, 0, SysControl.Width,
        SysControl.Height);
      RectVCenter(R, Rect(SysControl.Width - BoxSize.cy - 1, 0,
        SysControl.Width, SysControl.Height));
    end;

    StyleServices.DrawElement(Canvas.Handle, Details, R);
    Canvas.Font := SysControl.Font;

    R := Rect(0, 0, SysControl.Width - BoxSize.cx - 10, SysControl.Height);
    LCaption := Text;
    Winapi.Windows.DrawText(Canvas.Handle, PWideChar(LCaption),
      Length(LCaption), R, SysControl.DrawTextBiDiModeFlags(DT_CALCRECT or
      DT_EXPANDTABS));

    if not RightAlignment then
      RectVCenter(R, Rect(BoxSize.cx + Spacing, 0, SysControl.Width,
        SysControl.Height))
    else
    begin
      if SysControl.BiDiMode <> bmRightToLeft then
        RectVCenter(R, Rect(3, 0, SysControl.Width - BoxSize.cx - Spacing,
          SysControl.Height))
      else
        RectVCenter(R, Rect(SysControl.Width - BoxSize.cx - Spacing - R.Right,
          0, SysControl.Width - BoxSize.cx - Spacing, SysControl.Height));
    end;

    DrawControlText(Canvas, Details, LCaption, R,
      SysControl.DrawTextBiDiModeFlags(DT_LEFT or DT_VCENTER or DT_EXPANDTABS));

    if Focused then
    begin
      InflateRect(R, 2, 1);
      if R.Top < 0 then
        R.Top := 0;
      if R.Bottom > SysControl.Height then
        R.Bottom := SysControl.Height;
      Canvas.Brush.Color := StyleServices.GetSystemColor(clBtnFace);
      Canvas.DrawFocusRect(R);
    end;
  end;
end;

procedure TSysCheckBoxStyleHook.PaintBackground(Canvas: TCanvas);
var
  Details: TThemedElementDetails;
begin
  if StyleServices.Available then
  begin
    Details.Element := teButton;
    if StyleServices.HasTransparentParts(Details) then
      StyleServices.DrawParentBackground(Handle, Canvas.Handle, Details, False);
  end;
end;

function TSysCheckBoxStyleHook.RightAlignment: Boolean;
begin
  Result := (SysControl.BiDiMode = bmRightToLeft) or
    (GetWindowLong(Handle, GWL_STYLE) and BS_RIGHTBUTTON = BS_RIGHTBUTTON);

end;

procedure TSysCheckBoxStyleHook.WMKeyDown(var Message: TWMKeyDown);
begin
  if Message.CharCode = VK_SPACE then
    SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  if Message.CharCode = VK_SPACE then
  begin
    SetRedraw(True);
    Invalidate;
  end;
  Handled := True;
end;

procedure TSysCheckBoxStyleHook.WMKeyUp(var Message: TWMKeyUp);
begin
  if Message.CharCode = VK_SPACE then
    SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  if Message.CharCode = VK_SPACE then
  begin
    SetRedraw(True);
    Invalidate;
  end;
  Handled := True;
end;

procedure TSysCheckBoxStyleHook.WMLButtonDblClk(var Message: TWMMouse);
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);
  Invalidate;
  Handled := True;
end;

procedure TSysCheckBoxStyleHook.WMLButtonDown(var Message: TWMMouse);
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);
  FPressed := True;
  Invalidate;
  Handled := True;
end;

procedure TSysCheckBoxStyleHook.WMLButtonUp(var Message: TWMMouse);
begin
  SetRedraw(False);
  CallDefaultProc(TMessage(Message));
  SetRedraw(True);
  FPressed := False;
  Invalidate;
  Handled := True;
end;

procedure TSysCheckBoxStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

{ TSysRadioButtonStyleHook }

constructor TSysRadioButtonStyleHook.Create(AHandle: THandle);
begin
  inherited;
  OverridePaint := True;
  OverrideEraseBkgnd := True;
  // DoubleBuffered := True;
end;

function TSysRadioButtonStyleHook.GetDrawState(State: TSysCheckBoxState)
  : TThemedButton;
begin
  Result := tbButtonDontCare;

  if not SysControl.Enabled then
    case State of
      cbUnchecked:
        Result := tbRadioButtonUncheckedDisabled;
      cbChecked:
        Result := tbRadioButtonCheckedDisabled;
    end
  else if Pressed and MouseInControl then
    case State of
      cbUnchecked:
        Result := tbRadioButtonUncheckedPressed;
      cbChecked:
        Result := tbRadioButtonCheckedPressed;
    end
  else if MouseInControl then
    case State of
      cbUnchecked:
        Result := tbRadioButtonUncheckedHot;
      cbChecked:
        Result := tbRadioButtonCheckedHot;
    end
  else
    case State of
      cbUnchecked:
        Result := tbRadioButtonUncheckedNormal;
      cbChecked:
        Result := tbRadioButtonCheckedNormal;
    end;
end;

procedure TSysRadioButtonStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

initialization

if StyleServices.Available then
begin
  with TSysStyleManager do
  begin
    RegisterSysStyleHook(WC_BUTTON, TSysButtonStyleHook);
    RegisterSysStyleHook(WC_EDIT, TSysEditStyleHook);
    RegisterSysStyleHook('ComboLBox', TSysListBoxStyleHook);
    RegisterSysStyleHook(WC_COMBOBOX, TSysComboBoxStyleHook);
    RegisterSysStyleHook( 'ListBox', TSysListBoxStyleHook);
    RegisterSysStyleHook( 'Static', TSysStaticStyleHook);
  end;
end;

finalization

with TSysStyleManager do
begin
  UnRegisterSysStyleHook(WC_BUTTON, TSysButtonStyleHook);
  UnRegisterSysStyleHook(WC_EDIT, TSysEditStyleHook);
  UnRegisterSysStyleHook('ComboLBox', TSysListBoxStyleHook);
  UnRegisterSysStyleHook(WC_COMBOBOX, TSysComboBoxStyleHook);
  UnRegisterSysStyleHook('ListBox', TSysListBoxStyleHook);
  UnRegisterSysStyleHook('Static', TSysStaticStyleHook);
end;

end.
