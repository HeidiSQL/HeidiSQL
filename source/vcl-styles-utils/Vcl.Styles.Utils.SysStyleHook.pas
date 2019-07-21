// **************************************************************************************************
//
// Unit Vcl.Styles.SysStyleHook
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
// The Original Code is uSysStyleHook.pas.
//
// Portions created by Mahdi Safsafi [SMP3]   e-mail SMP@LIVE.FR
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2019 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.Utils.SysStyleHook;

interface

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.UxTheme,
  Winapi.CommCtrl,
  Vcl.Themes,
  Vcl.ExtCtrls,
  Vcl.Controls,
  Vcl.Graphics;

const
  CM_BASE = WM_USER + $113;
  CM_CTLCOLORBTN = CM_BASE + WM_CTLCOLORBTN;
  CM_CTLCOLORDLG = CM_BASE + WM_CTLCOLORDLG;
  CM_CTLCOLOREDIT = CM_BASE + WM_CTLCOLOREDIT;
  CM_CTLCOLORLISTBOX = CM_BASE + WM_CTLCOLORLISTBOX;
  CM_CTLCOLORMSGBOX = CM_BASE + WM_CTLCOLORMSGBOX;
  CM_CTLCOLORSCROLLBAR = CM_BASE + WM_CTLCOLORSCROLLBAR;
  CM_CTLCOLORSTATIC = CM_BASE + WM_CTLCOLORSTATIC;
  CM_SCROLLTRACKING = CM_BASE + 350;
  CM_PARENTHOOKED = CM_BASE + 360;
  CM_CONTROLHOOKED = CM_BASE + 361;
  CM_INITCHILDS = CM_BASE + 362;
  CM_CONTROLHOOKEDDIRECTLY = CM_BASE + 363;

type
  TBidiModeDirection = (bmLeftToRight, bmRightToLeft);

type
  TSysStyleHook = class;
  TMouseTrackSysControlStyleHook = class;
  TSysControl = class;
  TSysStyleHookClass = class of TSysStyleHook;

{$REGION 'TSysControl'}

  TSysControl = class
  private
    FFont: TFont;
    FParent: TSysControl;
    FHandle: THandle;
    FWindowClassName : string;
    FDestroyed  : Boolean;
    function GetParent: TSysControl;
    function GetParentHandle: THandle;
    function GetText: String;
    function GetStyle: NativeInt;
    function GetExStyle: NativeInt;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetBorder: Boolean;
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    function GetClientRect: TRect;
    function GetWinRect: TRect;
    function GetClientEdge: Boolean;
    function GetControlClassName: String;
    function GetWndProc: NativeInt;
    procedure SetWndProc(Value: NativeInt);
    function GetBidiMode: TBidiModeDirection;
    procedure SetExStyle(const Value: NativeInt);
    procedure SetStyle(const Value: NativeInt);
    function GetControlID: Integer;
    function GetBoundsRect: TRect;
    function GetFont: TFont;
    function IsControlChild: Boolean;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
  public
    constructor Create(AHandle: THandle); virtual;
    Destructor Destroy; override;
    property ClientHeight: Integer read GetClientHeight;
    property ClientWidth: Integer read GetClientWidth;
    property Font: TFont read GetFont;
    property Parent: TSysControl read GetParent;
    property ParentHandle: THandle read GetParentHandle;
    property Handle: THandle read FHandle write FHandle;
    property Text: String read GetText;
    property Style: NativeInt read GetStyle write SetStyle;
    property ExStyle: NativeInt read GetExStyle write SetExStyle;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Left: Integer read GetLeft;
    property Top: Integer read GetTop;
    property HasBorder: Boolean read GetBorder;
    property Enabled: Boolean read GetEnabled;
    property Visible: Boolean read GetVisible;
    property ClientRect: TRect read GetClientRect;
    property WindowRect: TRect read GetWinRect;
    property HasClientEdge: Boolean read GetClientEdge;
    property ControlClassName: string read GetControlClassName;
    property WndProc: NativeInt read GetWndProc write SetWndProc;
    property BidiMode: TBidiModeDirection read GetBidiMode;
    property ControlID: Integer read GetControlID;
    property BoundsRect: TRect read GetBoundsRect;
    property IsChild: Boolean read IsControlChild;
    property Destroyed : Boolean read FDestroyed write FDestroyed;     //WM_DESTROY
    function DrawTextBiDiModeFlags(const Flags: Longint): Longint;
    function UseRightToLeftAlignment: Boolean; dynamic;
    function DrawTextBiDiModeFlagsReadingOnly: Longint;
    function UseRightToLeftReading: Boolean;
    function Focused: Boolean; dynamic;

  end;
{$ENDREGION}
{$REGION 'TSysStyleHook'}

  TSysStyleHook = class
  private
    FHandle: HWND;
    FProcInstance: Pointer;
    FOrgWndProc: NativeInt;
    FSysControl: TSysControl;
    FOverrideEraseBkgnd: Boolean;
    FOverridePaint: Boolean;
    FOverridePaintNC: Boolean;
    FOverrideFont: Boolean;
    FDoubleBuffered: Boolean;
    FPaintOnEraseBkgnd: Boolean;
    FFontColor: TColor;
    FBrush: TBrush;
    FHandled: Boolean;
    FParentColor: Boolean;
{$IF CompilerVersion > 23}
    FStyleElements: TStyleElements;
{$IFEND}
    FColor: TColor;
    FFont: TFont;
    FText: string;
    FHookedDirectly, FMustRemove: Boolean;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
{$IF CompilerVersion > 23}
    procedure SetStyleElements(Value: TStyleElements);
{$IFEND}
    function GetFontColor: TColor;
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetOverridePaint(const Value: Boolean);
    function GetFocused: Boolean;
    function GetParentHandle: HWND;
    procedure SetFont(Value: TFont);
    function UseLeftScrollBar: Boolean;
    function GetText: string;
  protected
    function GetBorderSize: TRect; virtual;
    function CheckIfParentBkGndPainted: Boolean; virtual;
    function CheckIfParentHooked: Boolean;
    procedure Paint(Canvas: TCanvas); virtual;
    procedure DrawParentBackground(DC: HDC); overload;
    procedure DrawParentBackground(DC: HDC; const ARect: PRect); overload;
    procedure PaintBorder(Control: TSysControl; const EraseLRCorner: Boolean);
    procedure DrawBorder(Canvas: TCanvas); virtual;
    procedure PaintBackground(Canvas: TCanvas); virtual;
    procedure PaintNC(Canvas: TCanvas); virtual;
    function CallDefaultProc(var Msg: TMessage): LRESULT;
    procedure SetRedraw(const Value: Boolean); overload;
    procedure SetRedraw(AHandle: HWND; const Value: Boolean); overload; virtual;
    function StyleServicesEnabled: Boolean;
    procedure WndProc(var Message: TMessage); virtual;
    function InternalPaint(DC: HDC): Boolean; virtual;
    procedure UpdateColors; virtual;
    function PaintControls(AControl: HWND; DC: HDC): Boolean;
    property HookedDirectly: Boolean read FHookedDirectly write FHookedDirectly;
    property MustRemove : Boolean read FMustRemove;
  public
    constructor Create(AHandle: THandle); virtual;
    Destructor Destroy; override;
    procedure Invalidate; virtual;
    procedure InvalidateNC; virtual;
    procedure Refresh; virtual;
    procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails; const S: string; var R: TRect; const Flags: Cardinal);
    function DrawTextCentered(DC: HDC; Details: TThemedElementDetails; const R: TRect; S: String; Const Flags: DWORD = 0): Integer;
    function DrawText(DC: HDC; Details: TThemedElementDetails; S: String; var R: TRect; Const Flags: TTextFormat = []): Integer;
    property Handle: HWND read FHandle;
    property ParentHandle: HWND read GetParentHandle;
    property Handled: Boolean read FHandled write FHandled;
    property SysControl: TSysControl read FSysControl write FSysControl;
{$IF CompilerVersion > 23}
    property StyleElements: TStyleElements read FStyleElements write SetStyleElements;
{$IFEND}
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered;
    property OverridePaint: Boolean read FOverridePaint write SetOverridePaint;
    property OverridePaintNC: Boolean read FOverridePaintNC write FOverridePaintNC;
    property OverrideFont: Boolean read FOverrideFont write FOverrideFont;
    property OverrideEraseBkgnd: Boolean read FOverrideEraseBkgnd write FOverrideEraseBkgnd;
    property FontColor: TColor read GetFontColor write FFontColor;
    property Color: TColor read GetColor write SetColor;
    property Brush: TBrush read FBrush;
    property Font: TFont read FFont write SetFont;
    property Focused: Boolean read GetFocused;
    property ParentBkGndPainted: Boolean read CheckIfParentBkGndPainted;
    property ParentColor: Boolean read FParentColor write FParentColor;
    property Text: string read GetText;
  end;

{$ENDREGION}
{$REGION 'TMouseTrackSysControlStyleHook'}

  TMouseTrackSysControlStyleHook = class(TSysStyleHook)
  private
    FMouseInControl: Boolean;
    FMouseInNCArea: Boolean;
    FHotTrackTimer: TComponent;
    FMouseDown: Boolean;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMNCMouseMove(var Message: TWMMouse); message WM_NCMOUSEMOVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
  protected
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    function IsChildHandle(AHandle: HWND): Boolean; virtual;
    procedure StartHotTrackTimer;
    procedure StopHotTrackTimer;
    procedure DoHotTrackTimer(Sender: TObject); virtual;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;

    property MouseInControl: Boolean read FMouseInControl write FMouseInControl;
    property MouseInNCArea: Boolean read FMouseInNCArea write FMouseInNCArea;
    property MouseDown: Boolean read FMouseDown;

  end;

{$ENDREGION}

function IsControlHooked(Handle: HWND): Boolean;

implementation

uses
  System.UITypes,
  Vcl.Styles.Utils.Misc,
  Vcl.Styles.Utils.SysControls;

// ------------------------------------------------------------------------------

function IsControlHooked(Handle: HWND): Boolean;
begin
  { Return True if Control is already hooked ! }
  Result := False;
  if Handle > 0 then
    Result := (SendMessage(Handle, CM_CONTROLHOOKED, 0, 0) = $77);
end;
// ------------------------------------------------------------------------------

{ TSysControl }
{$REGION 'TSysControl'}

constructor TSysControl.Create(AHandle: THandle);
begin
  inherited Create;
  FFont := nil;
  FParent := nil;
  Handle := AHandle;
  FWindowClassName := '';
  FDestroyed := False;
end;

destructor TSysControl.Destroy;
begin
  if Assigned(FParent) then
    FreeAndNil(FParent);
  if FFont <> nil then
    FFont.Free;
  inherited;
end;

function TSysControl.DrawTextBiDiModeFlags(const Flags: Integer): Longint;
begin
  Result := Flags;
  { do not change center alignment }
  if UseRightToLeftAlignment then
    if Result and DT_RIGHT = DT_RIGHT then
      Result := Result and not DT_RIGHT { removing DT_RIGHT, makes it DT_LEFT }
    else if not(Result and DT_CENTER = DT_CENTER) then
      Result := Result or DT_RIGHT;
  Result := Result or DrawTextBiDiModeFlagsReadingOnly;
end;

function TSysControl.DrawTextBiDiModeFlagsReadingOnly: Longint;
begin
  if UseRightToLeftReading then
    Result := DT_RTLREADING
  else
    Result := 0;
end;

function TSysControl.Focused: Boolean;
begin
  Result := (Handle <> 0) and (GetFocus = Handle);
end;

function TSysControl.GetBidiMode: TBidiModeDirection;
begin
  Result := bmLeftToRight;
  if Style <> 0 then
    if (ExStyle and WS_EX_RIGHT = WS_EX_RIGHT) or (ExStyle and WS_EX_RTLREADING = WS_EX_RTLREADING) or (ExStyle and WS_EX_LAYOUTRTL = WS_EX_LAYOUTRTL) then
      Result := bmRightToLeft;
end;

function TSysControl.GetBorder: Boolean;
begin
  Result := (Style and WS_BORDER = WS_BORDER) or (ExStyle and WS_EX_CLIENTEDGE = WS_EX_CLIENTEDGE);
end;

function TSysControl.GetBoundsRect: TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function TSysControl.GetClientEdge: Boolean;
begin
  Result := ExStyle and WS_EX_CLIENTEDGE = WS_EX_CLIENTEDGE;
end;

function TSysControl.GetClientHeight: Integer;
begin
  Result := ClientRect.Bottom;
end;

function TSysControl.GetClientRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  Winapi.Windows.GetClientRect(Handle, Result);
end;

function TSysControl.GetClientWidth: Integer;
begin
  Result := ClientRect.Right;
end;

function TSysControl.GetControlClassName: String;
begin
  if FWindowClassName='' then
    FWindowClassName := GetWindowClassName(Handle);
  Result:=FWindowClassName;
end;

function TSysControl.GetControlID: Integer;
begin
  Result := GetWindowLongPtr(Handle, GWL_ID);
end;

function TSysControl.GetEnabled: Boolean;
begin
  Result := False;
  if Handle > 0 then
    Result := IsWindowEnabled(Handle);
end;

function TSysControl.GetHeight: Integer;
begin
  Result := WindowRect.Height;
end;

function TSysControl.GetLeft: Integer;
begin
  Result := WindowRect.Left;
end;

function TSysControl.GetParent: TSysControl;
begin
  Result := nil;
  if Assigned(FParent) then
    FreeAndNil(FParent);
  if ParentHandle <> 0 then
  begin
    FParent := TSysControl.Create(ParentHandle);
    Result := FParent;
  end;
end;

function TSysControl.GetParentHandle: THandle;
begin
  Result := Winapi.Windows.GetParent(Handle);
end;

function TSysControl.GetStyle: NativeInt;
begin
  Result := GetWindowLongPtr(Handle, GWL_STYLE);
end;

function TSysControl.GetExStyle: NativeInt;
begin
  Result := GetWindowLongPtr(Handle, GWL_EXSTYLE);
end;

function TSysControl.GetFont: TFont;
var
  LogFont: TLogFont;
  hFont: HGDIOBJ;
begin
  if FFont <> nil then
    Exit(FFont);

  hFont := HGDIOBJ(SendMessage(Handle, WM_GETFONT, 0, 0));
  Result := TFont.Create;
  FillChar(LogFont, SizeOf(LogFont), 0);
  GetObject(hFont, SizeOf(LogFont), @LogFont);
  Result.Name := StrPas(LogFont.lffaceName);
  Result.Height := LogFont.lfHeight;
  if LogFont.lfWeight >= FW_MEDIUM then
    Result.Style := Result.Style + [fsBold];
  if LogFont.lfItalic <> 0 then
    Result.Style := Result.Style + [fsItalic];
  if LogFont.lfUnderline <> 0 then
    Result.Style := Result.Style + [fsUnderline];
  if LogFont.lfStrikeout <> 0 then
    Result.Style := Result.Style + [fsStrikeout];
  case (LogFont.lfPitchAndFamily and 3) of
    VARIABLE_PITCH: Result.Pitch := fpVariable;
    FIXED_PITCH: Result.Pitch := fpFixed;
  end;

  FFont := Result;
end;

function TSysControl.GetText: String;
var
  Buffer: array [0 .. 1023] of Char;
begin
  SetString(Result, Buffer, Winapi.Windows.GetWindowText(Handle, Buffer, Length(Buffer)));
end;

function TSysControl.GetTop: Integer;
begin
  Result := WindowRect.Top;
end;

function TSysControl.GetVisible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

function TSysControl.GetWidth: Integer;
begin
  Result := WindowRect.Width;
end;

function TSysControl.GetWinRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  GetWindowRect(Handle, Result);
end;

function TSysControl.GetWndProc: NativeInt;
begin
  Result := GetWindowLongPtr(Handle, GWL_WNDPROC);
end;

function TSysControl.IsControlChild: Boolean;
begin
  Result := (Style and WS_CHILD = WS_CHILD);
end;

procedure TSysControl.SetExStyle(const Value: NativeInt);
begin
  SetWindowLongPtr(Handle, GWL_EXSTYLE, Value);
end;

procedure TSysControl.SetStyle(const Value: NativeInt);
begin
  SetWindowLongPtr(Handle, GWL_STYLE, Value);
end;

procedure TSysControl.SetWndProc(Value: NativeInt);
begin
  if Value <> WndProc then
    SetWindowLongPtr(Handle, GWL_WNDPROC, Value);
end;

function TSysControl.UseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and (BidiMode = TBidiModeDirection.bmRightToLeft);
end;

function TSysControl.UseRightToLeftReading: Boolean;
begin
  Result := SysLocale.MiddleEast and (BidiMode <> TBidiModeDirection.bmLeftToRight);
end;

{$ENDREGION}
{ TSysStyleHook }
{$REGION 'TSysStyleHook'}

constructor TSysStyleHook.Create(AHandle: THandle);
begin
  FHandled := False;
  FSysControl := nil;
  FHandle := AHandle;
  FOrgWndProc := 0;
  FProcInstance := nil;
  FBrush := nil;
  FFont := TFont.Create;
{$IF CompilerVersion > 23}
  StyleElements := [];
{$IFEND}
  FMustRemove := False;
  FParentColor := False;
  FDoubleBuffered := False;
  FPaintOnEraseBkgnd := False;
  FHookedDirectly := False;
  OverridePaint := False;
  OverridePaintNC := False;
  OverrideEraseBkgnd := False;
  OverrideFont := False;
  if AHandle > 0 then
  begin
    FProcInstance := MakeObjectInstance(WndProc);
    FSysControl := TSysControl.Create(AHandle);
    FOrgWndProc := FSysControl.WndProc;
    // if FOrgWndProc > 0 then
    begin
      FSysControl.WndProc := LONG_PTR(FProcInstance);
      FBrush := TBrush.Create;
      UpdateColors;
    end;
  end;
end;

destructor TSysStyleHook.Destroy;
begin
  if FOrgWndProc <> 0 then
    FSysControl.WndProc := FOrgWndProc;

  if Assigned(FProcInstance) then
    FreeObjectInstance(FProcInstance);

  if Assigned(FSysControl) then
    FreeAndNil(FSysControl);

  if Assigned(FBrush) then
    FreeAndNil(FBrush);

  if Assigned(FFont) then
    FreeAndNil(FFont);

  inherited;
end;

function TSysStyleHook.CallDefaultProc(var Msg: TMessage): LRESULT;
begin
  Result := CallWindowProc(Pointer(FOrgWndProc), Handle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TSysStyleHook.DrawBorder(Canvas: TCanvas);
var
  BorderSize: TRect;
begin
  BorderSize := GetBorderSize;
  with BorderSize do
    if (Left > 0) and (Right > 0) and (Top > 0) and (Bottom > 0) then
      PaintBorder(SysControl, True);
end;

procedure TSysStyleHook.DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails; const S: string; var R: TRect; const Flags: Cardinal);
var
  ThemeTextColor: TColor;
  TextFormat: TTextFormatFlags;
begin
  Canvas.Font := SysControl.Font;
  TextFormat := TTextFormatFlags(Flags);
  if StyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor) then
  begin
    Canvas.Font.Color := ThemeTextColor;
    StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
  end
  else
  begin
    Canvas.Refresh;
    StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat);
  end;
end;

procedure TSysStyleHook.DrawParentBackground(DC: HDC; const ARect: PRect);
var
  Bmp: TBitmap;
  P: TPoint;
begin
  P := Point(0, 0);
  if ARect <> nil then
    P := Point(ARect.Left, ARect.Top);

  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(SysControl.Parent.Width, SysControl.Parent.Height);
    SendMessage(ParentHandle, WM_ERASEBKGND, Bmp.Canvas.Handle, $93);
    ClientToScreen(Handle, P);
    ScreenToClient(ParentHandle, P);
    if ARect <> nil then
      BitBlt(DC, ARect.Left, ARect.Top, ARect.Width, ARect.Height, Bmp.Canvas.Handle, P.X, P.Y, SRCCOPY)
    else
      BitBlt(DC, 0, 0, SysControl.Width, SysControl.Height, Bmp.Canvas.Handle, P.X, P.Y, SRCCOPY);
  finally
    Bmp.Free;
  end;

end;

function TSysStyleHook.DrawText(DC: HDC; Details: TThemedElementDetails; S: String; var R: TRect; const Flags: TTextFormat): Integer;
var
  DrawFlags: Cardinal;
  SaveIndex: Integer;
  LColor: TColor;
begin
  SaveIndex := SaveDC(DC);
  try
    SetBkMode(DC, TRANSPARENT);
    if not StyleServices.GetElementColor(Details, ecTextColor, LColor) then
      LColor := FontColor;
    if not OverrideFont then
      LColor := FontColor;
    SetTextColor(DC, ColorToRGB(LColor));
    DrawFlags := TTextFormatFlags(Flags);
    Result := Winapi.Windows.DrawText(DC, S, -1, R, DrawFlags);
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;

function TSysStyleHook.DrawTextCentered(DC: HDC; Details: TThemedElementDetails; const R: TRect; S: String; Const Flags: DWORD = 0): Integer;
var
  DrawRect: TRect;
  DrawFlags: Cardinal;
  DrawParams: TDrawTextParams;
  SaveIndex: Integer;
  LColor: TColor;
begin
  SaveIndex := SaveDC(DC);
  try
    SetBkMode(DC, TRANSPARENT);
    if not StyleServices.GetElementColor(Details, ecTextColor, LColor) then
      LColor := FontColor;
    if not OverrideFont then
      LColor := FontColor;
    SetTextColor(DC, ColorToRGB(LColor));
    DrawRect := R;
    DrawFlags := DT_END_ELLIPSIS or DT_WORDBREAK or DT_EDITCONTROL or DT_CENTER;
    if DrawFlags <> 0 then
      DrawFlags := DrawFlags or Flags;

    Winapi.Windows.DrawText(DC, PChar(S), -1, DrawRect, DrawFlags or DT_CALCRECT);
    DrawRect.Right := R.Right;
    if DrawRect.Bottom < R.Bottom then
      OffsetRect(DrawRect, 0, (R.Bottom - DrawRect.Bottom) div 2)
    else
      DrawRect.Bottom := R.Bottom;
    ZeroMemory(@DrawParams, SizeOf(DrawParams));
    DrawParams.cbSize := SizeOf(DrawParams);
    DrawTextEx(DC, PChar(S), -1, DrawRect, DrawFlags, @DrawParams);
    Result := DrawParams.uiLengthDrawn;
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;

function TSysStyleHook.GetFocused: Boolean;
begin
  Result := (GetFocus = Handle);
end;

function TSysStyleHook.GetBorderSize: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TSysStyleHook.GetColor: TColor;
begin
  // if OverrideEraseBkgnd then
  // Result := StyleServices.GetStyleColor(scWindow)
  // else
  Result := FColor;
end;

function TSysStyleHook.GetFontColor: TColor;
begin
  // if OverrideFont then
  // Result := StyleServices.GetSystemColor(clWindowText)
  // else
  Result := FFontColor;
end;

function TSysStyleHook.GetParentHandle: HWND;
begin
  Result := GetParent(Handle);
end;

function TSysStyleHook.GetText: string;
var
  Buffer: array [0 .. 255] of Char;
begin
  if (Handle <> 0) then
    SetString(Result, Buffer, Winapi.Windows.GetWindowText(Handle, Buffer, Length(Buffer)));
  FText := Result;
end;

function TSysStyleHook.InternalPaint(DC: HDC): Boolean;
begin
  Result := False;
end;

procedure TSysStyleHook.SetColor(const Value: TColor);
begin
  if (FBrush <> nil) and ((Value <> FColor) or (Value <> FBrush.Color)) then
  begin
    FColor := Value;
    FBrush.Color := Value;
  end;
end;

procedure TSysStyleHook.SetFont(Value: TFont);
begin
  if Value <> FFont then
    FFont.Assign(Value);
end;

procedure TSysStyleHook.SetOverridePaint(const Value: Boolean);
begin
  if Value then
    OverrideEraseBkgnd := Value;
  FOverridePaint := Value;
end;

procedure TSysStyleHook.SetRedraw(AHandle: HWND; const Value: Boolean);
begin
  SendMessage(AHandle, WM_SETREDRAW, wParam(Value), 0);
end;

procedure TSysStyleHook.SetRedraw(const Value: Boolean);
begin
  SetRedraw(Handle, Value);
end;

{$IF CompilerVersion > 23}

procedure TSysStyleHook.SetStyleElements(Value: TStyleElements);
begin
  if Value <> FStyleElements then
  begin
    FStyleElements := Value;
    OverridePaint := (seClient in FStyleElements);
    // OverrideEraseBkgnd := OverridePaint;
    OverridePaintNC := (seBorder in FStyleElements);
    OverrideFont := (seFont in FStyleElements);
  end;
end;
{$IFEND}

function TSysStyleHook.StyleServicesEnabled: Boolean;
begin
  Result := (StyleServices.Available) and not(StyleServices.IsSystemStyle);
  if Result then
    if not TSysStyleManager.HookVclControls then
      Result := not(IsVCLControl(Handle));
end;

procedure TSysStyleHook.UpdateColors;
begin
  if (OverrideEraseBkgnd) or (OverridePaint) then
    Color := StyleServices.GetStyleColor(scWindow)
  else
    Color := clBtnFace;
  if OverrideFont then
    FontColor := StyleServices.GetSystemColor(clWindowText)
  else
    FontColor := clBlack;
end;

function TSysStyleHook.UseLeftScrollBar: Boolean;
begin
  Result := (SysControl.ExStyle and WS_EX_LEFTSCROLLBAR = WS_EX_LEFTSCROLLBAR)
end;

procedure TSysStyleHook.Invalidate;
begin
  if FOverridePaintNC then
    InvalidateNC;
  InvalidateRect(Handle, nil, False);
end;

procedure TSysStyleHook.InvalidateNC;
begin
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TSysStyleHook.Paint(Canvas: TCanvas);
begin
 //
end;

procedure TSysStyleHook.PaintBackground(Canvas: TCanvas);
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(SysControl.ClientRect);
end;

procedure TSysStyleHook.PaintBorder(Control: TSysControl; const EraseLRCorner: Boolean);
var
  EmptyRect, DrawRect: TRect;
  DC: HDC;
  H, W: Integer;
  AStyle: Integer;
  Details: TThemedElementDetails;
  BorderSize: TRect;
begin
  BorderSize := GetBorderSize;
    Control.ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if (Control.ExStyle and WS_EX_CLIENTEDGE) <> 0 then
    begin
      GetWindowRect(Control.Handle, DrawRect);
      OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
      DC := GetWindowDC(Control.Handle);
      try
        EmptyRect := DrawRect;
        if EraseLRCorner then
        begin
          AStyle := GetWindowLong(Control.Handle, GWL_STYLE);
          if ((AStyle and WS_HSCROLL) <> 0) and ((AStyle and WS_VSCROLL) <> 0) then
          begin
            W := GetSystemMetrics(SM_CXVSCROLL);
            H := GetSystemMetrics(SM_CYHSCROLL);
            InflateRect(EmptyRect, -2, -2);
            with EmptyRect do
              if not UseLeftScrollBar then
                EmptyRect := Rect(Left, Bottom - H, Left + W, Bottom)
              else
                EmptyRect := Rect(Right - W, Bottom - H, Right, Bottom);
            FillRect(DC, EmptyRect, GetSysColorBrush(COLOR_BTNFACE));
          end;
        end;
        with DrawRect do
          ExcludeClipRect(DC, Left + BorderSize.Left, Top + BorderSize.Top, Right - BorderSize.Right, Bottom - BorderSize.Bottom);
        Details := StyleServices.GetElementDetails(teEditTextNormal);
        StyleServices.DrawElement(DC, Details, DrawRect);
      finally
        ReleaseDC(Control.Handle, DC);
      end;
    end;
end;

function TSysStyleHook.PaintControls(AControl: HWND; DC: HDC): Boolean;
var
  Child: HWND;
  SavedDC: HDC;
  SysChild: TSysControl;
  P: TPoint;
  FrameBrush: HBRUSH;
begin
  Result := False;
  Child := GetTopWindow(AControl);
  while Child <> 0 do
  begin
    Result := True;
    SysChild := TSysControl.Create(Child);
    with SysChild do
    begin
      SavedDC := SaveDC(DC);
      P := Point(Left, Top);
      ScreenToClient(ParentHandle, P);
      if Visible and IsChild and RectVisible(DC, Rect(P.X, P.Y, P.X + Width, P.Y + Height)) then
      begin
        MoveWindowOrg(DC, P.X, P.Y);
        IntersectClipRect(DC, 0, 0, Width, Height);
        SendMessage(Child, WM_PAINT, lParam(DC), 0);
        if SysChild.HasBorder then
        begin
          // SendMessage(Child, WM_NCPAINT, 0, 0);
          FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
          FrameRect(DC, System.Types.Rect(0, 0, Width, Height), FrameBrush);
          DeleteObject(FrameBrush);
          FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
          FrameRect(DC, System.Types.Rect(0, 0, Width + 1, Height + 1), FrameBrush);
          DeleteObject(FrameBrush);
        end;
      end;
      FreeAndNil(SysChild);
      PaintControls(Child, DC);
      RestoreDC(DC, SavedDC);
      Child := GetNextWindow(Child, GW_HWNDNEXT);
    end;
  end;
end;

procedure TSysStyleHook.PaintNC(Canvas: TCanvas);
begin

end;

procedure TSysStyleHook.DrawParentBackground(DC: HDC);
begin
  DrawParentBackground(DC, nil);
end;

procedure TSysStyleHook.Refresh;
begin
  SendMessage(Handle, WM_PAINT, 0, 0);
end;

procedure TSysStyleHook.WMEraseBkgnd(var Message: TMessage);
var
  DC: HDC;
  Canvas: TCanvas;
  SaveIndex: Integer;
begin
  Handled := False;

  if not StyleServicesEnabled then
    Exit;

  UpdateColors;

  if FOverrideEraseBkgnd then
  begin
    if not FDoubleBuffered then
    begin
      DC := HDC(Message.wParam);

      SaveIndex := 0;
      if DC = 0 then
        DC := GetDC(Handle)
      else
        SaveIndex := SaveDC(DC);

      Canvas := TCanvas.Create;
      try
        Canvas.Handle := DC;
        if Assigned(FFont) then
          Canvas.Font.Assign(FFont);

        if (FParentColor) and (ParentHandle > 0) then
          DrawParentBackground(Canvas.Handle)
        else
          PaintBackground(Canvas);

        if (FPaintOnEraseBkgnd) and (Message.lParam <> $93) then
          Paint(Canvas);
      finally
        Canvas.Handle := 0;
        Canvas.Free;
        if Message.wParam = 0 then
          ReleaseDC(Handle, DC)
        else if SaveIndex <> 0 then
          RestoreDC(DC, SaveIndex);
      end;
    end;
    Handled := True;
    Message.Result := 1;
  end;
end;

function TSysStyleHook.CheckIfParentBkGndPainted: Boolean;
var
  Test: Integer;
  PTest: PInteger;
  LParentHandle: HWND;
begin
  //Exit(True);
  Test := $93;
  PTest := @Test;
  Result := False;
  LParentHandle := GetParent(Handle);
  if LParentHandle > 0 then
  begin
    if not IsControlHooked(LParentHandle) then
      Exit(False);
    SendMessage(LParentHandle, WM_ERASEBKGND, 0, lParam(PTest));
    Result := (PTest^ = $11);
  end;
end;

function TSysStyleHook.CheckIfParentHooked: Boolean;
begin
  Result := (SendMessage(ParentHandle, CM_PARENTHOOKED, 0, 0) = $77);
end;

procedure TSysStyleHook.WMNCPaint(var Message: TMessage);
var
  Canvas: TCanvas;
begin
  Handled := False;
  if not StyleServicesEnabled then
    Exit;

  if FOverridePaintNC then
  begin
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := GetWindowDC(SysControl.Handle);
      if Assigned(FFont) then
        Canvas.Font.Assign(FFont);
      DrawBorder(Canvas);
      PaintNC(Canvas);
    finally
      ReleaseDC(Handle, Canvas.Handle);
      Canvas.Handle := 0;
      Canvas.Free;
    end;
    Handled := True;
  end;
end;

procedure TSysStyleHook.WMPaint(var Message: TMessage);
var
  OrgDC, DC: HDC;
  Buffer: TBitmap;
  Canvas: TCanvas;
  PS: TPaintStruct;

  function ClipControls(AControl: HWND; Siblings: Boolean): Boolean;
  var
    Child: HWND;
    SysChild: TSysControl;
    P: TPoint;
  begin
    Result := False;
    SysChild := nil;
    Child := GetTopWindow(AControl);
    if GetParent(Child) = Handle then
      while Child <> 0 do
      begin
        Result := True;
        SysChild := TSysControl.Create(Child);
        with SysChild, P do
        begin
          P := Point(Left, Top);
          ScreenToClient(Self.Handle, P);
          if Visible and IsChild and RectVisible(DC, Rect(X, Y, X + Width, Y + Height)) then
          begin
            ExcludeClipRect(DC, X, Y, X + Width, Y + Height);
          end;
          FreeAndNil(SysChild);
          if Siblings then
            ClipControls(Child, Siblings);
          Child := GetNextWindow(Child, GW_HWNDNEXT);
        end;
      end;
    if Assigned(SysChild) then
      FreeAndNil(SysChild);
  end;

  function DoClipControls: Boolean;
  begin
    Result := False;
    if SysControl.Style and WS_CLIPSIBLINGS = WS_CLIPSIBLINGS then
      Result := ClipControls(Handle, True)
    else if SysControl.Style and WS_CLIPCHILDREN = WS_CLIPCHILDREN then
      Result := ClipControls(Handle, False);
  end;

begin
  Handled := False;
  if not StyleServicesEnabled then
    Exit;

  if OverridePaint then
  begin
    OrgDC := HDC(Message.wParam);
    Canvas := TCanvas.Create;
    try

      if OrgDC <> 0 then
      begin
        Canvas.Handle := OrgDC;
        DC:= OrgDC;
      end
      else
      begin
        DC := GetDC(Handle);
        BeginPaint(SysControl.Handle, PS);
        Canvas.Handle := DC;
      end;

      if Assigned(FFont) then
        Canvas.Font.Assign(FFont);

      if not InternalPaint(Canvas.Handle) then
        if FDoubleBuffered and (DC = 0) then
        begin
          Buffer := TBitmap.Create;
          try
            Buffer.SetSize(SysControl.Width, SysControl.Height);
            DoClipControls;
            PaintBackground(Buffer.Canvas);
            Paint(Buffer.Canvas);
            // PaintControls(Handle,Canvas.Handle);
            Canvas.Draw(0, 0, Buffer);
          finally
            Buffer.Free;
          end;
        end
        else
        begin
          DoClipControls;
          Paint(Canvas);
          // PaintControls(Handle,Canvas.Handle);
        end;

      if OrgDC = 0 then
      begin
        ReleaseDC(SysControl.Handle, DC);
        EndPaint(SysControl.Handle, PS);
      end;

    finally
      Canvas.Handle := 0;
      Canvas.Free;
    end;
    Handled := True;
  end;

end;


procedure TSysStyleHook.WndProc(var Message: TMessage);
var
  TempResult: LRESULT;
  ChildHandle: HWND;
  ItemRemoved : Boolean;
begin
  case Message.Msg of

    CM_CONTROLHOOKEDDIRECTLY:
      begin
        { Child controls are not hooked inside the parent . }
        FHookedDirectly := True;
        Exit;
      end;

    CM_INITCHILDS:
      begin
        Message.Result := 0;
        with TSysStyleManager do
        begin
          for ChildHandle in ChildRegSysStylesList.Keys do
            if (not IsControlHooked(ChildHandle)) and (ChildRegSysStylesList[ChildHandle].Parent = Handle) then
            begin
              if not SysStyleHookList.ContainsKey(ChildHandle) then
              begin
                SysStyleHookList.Add(ChildHandle, ChildRegSysStylesList[ChildHandle].StyleHookClass.Create(ChildHandle));
                { Child control need to be repainted . }
                RedrawWindow(ChildHandle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INTERNALPAINT or RDW_INVALIDATE);
                { Send WM_NCCALCSIZE message to the child control . }
                SetWindowPos(ChildHandle, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_FRAMECHANGED);
                Message.Result := 1;
              end;
            end;
        end;
        Exit;
      end;

    CM_PARENTHOOKED, CM_CONTROLHOOKED:
      begin
        Message.Result := $77;
        Exit;
      end;

    WM_CHANGEUISTATE, WM_PARENTNOTIFY, WM_QUERYUISTATE:
      begin
        CallDefaultProc(Message);
        {
          Do not Send ===> Use Post
          Return first then hook child !
        }
        PostMessage(Handle, CM_INITCHILDS, 0, 0);
        Exit;
      end;

    WM_ERASEBKGND:
      begin
        if (Message.lParam > 0) and (Message.wParam = 0) and (FOverrideEraseBkgnd or FOverridePaint or FPaintOnEraseBkgnd) then
          if PInteger(Message.lParam)^ = $93 then
          begin
            { lParam = Result
              if (lParam=$11) then Parent background was painted .
            }
            PInteger(Message.lParam)^ := $11;
            { Do not process the default message ..
              this is only for test !! .
            }
            Exit; { Do not Dispatch . }
          end;
      end;

    WM_SETREDRAW:
      begin
        Message.Result := CallDefaultProc(Message);
        Dispatch(Message);
        Exit;
      end;

    WM_CTLCOLORMSGBOX .. WM_CTLCOLORSTATIC:
      begin
        // avoid use cuurent style colors on ignored controls
        if (not StyleServicesEnabled) or (not TSysStyleManager.UseStyleColorsChildControls and (not TSysStyleManager.SysStyleHookList.ContainsKey(Message.lParam))) then
        // if (not StyleServicesEnabled) then
        begin
          Message.Result := CallDefaultProc(Message);
          Exit;
        end;

        TempResult := SendMessage(Handle, CM_BASE + Message.Msg, Message.wParam, Message.lParam);
        Message.Result := SendMessage(Message.lParam, CM_BASE + Message.Msg, Message.wParam, Message.lParam);
        if Message.Result = 0 then
          Message.Result := TempResult;
        Exit;
      end;

    CM_CTLCOLORMSGBOX .. CM_CTLCOLORSTATIC:
      begin
        SetTextColor(Message.wParam, ColorToRGB(FontColor));
        SetBkColor(Message.wParam, ColorToRGB(FBrush.Color));
        Message.Result := LRESULT(FBrush.Handle);
        Exit;
      end;

    WM_DESTROY:
      begin
        Message.Result := CallDefaultProc(Message);
        Dispatch(Message);
        Exit;
      end;

    //The WM_NCDESTROY message is sent after the child windows have been destroyed.
    //In contrast, WM_DESTROY is sent before the child windows are destroyed.
    WM_NCDESTROY:
      begin
        Message.Result := CallDefaultProc(Message);
        ItemRemoved:=False;
        if TSysStyleManager.SysStyleHookList.ContainsKey(FHandle) then
        begin
//          OutputDebugString(PChar('SysStyleHookList WM_NCDESTROY Removed '+IntToHex(Handle, 8)));
//          TSysStyleManager.SysStyleHookList.Remove(FHandle);
          FMustRemove:=True;
          ItemRemoved:=True;
        end;

        if not ItemRemoved and TSysStyleManager.ChildRegSysStylesList.ContainsKey(FHandle) then
        begin
          TSysStyleManager.ChildRegSysStylesList.Remove(Handle);
          //OutputDebugString(PChar('ChildRegSysStylesList WM_NCDESTROY Removed '+IntToHex(Handle, 8)));
        end;

        for ChildHandle in TSysStyleManager.ChildRegSysStylesList.Keys do
            if (TSysStyleManager.ChildRegSysStylesList[ChildHandle].Parent = FHandle)  then
            begin
               TSysStyleManager.ChildRegSysStylesList.Remove(ChildHandle);
               //OutputDebugString(PChar('Sub ChildRegSysStylesList WM_NCDESTROY Removed '+IntToHex(ChildHandle, 8)));
            end;
        Exit;
      end;

  end;

  Dispatch(Message);
  if not Handled then
    Message.Result := CallDefaultProc(Message);
  Handled := False;
end;
{$ENDREGION}
{ TMouseTrackSysControlStyleHook }
{$REGION 'TMouseTrackSysControlStyleHook'}

constructor TMouseTrackSysControlStyleHook.Create(AHandle: THandle);
begin
  inherited;
  FMouseInControl := False;
  FMouseInNCArea := False;
  FHotTrackTimer := nil;
end;

destructor TMouseTrackSysControlStyleHook.Destroy;
begin
  if Assigned(FHotTrackTimer) then
    FreeAndNil(FHotTrackTimer);

  inherited;
end;

procedure TMouseTrackSysControlStyleHook.WMLButtonDown(var Message: TWMLButtonDown);
begin
  FMouseDown := True;
  inherited;
end;

procedure TMouseTrackSysControlStyleHook.WMLButtonUp(var Message: TWMLButtonUp);
begin
  FMouseDown := False;
  inherited;
end;

procedure TMouseTrackSysControlStyleHook.WMMouseMove(var Message: TWMMouse);
begin
  inherited;
  if not FMouseInControl and not FMouseInNCArea then
  begin
    FMouseInControl := True;
    StartHotTrackTimer;
    MouseEnter;
  end
  else if FMouseInNCArea and FMouseInControl then
  begin
    StopHotTrackTimer;
    FMouseInControl := False;
    MouseLeave;
  end;
end;

procedure TMouseTrackSysControlStyleHook.WMNCMouseMove(var Message: TWMMouse);
begin
  inherited;
  if not FMouseInControl then
  begin
    FMouseInControl := True;
    StartHotTrackTimer;
    MouseEnter;
  end;
end;

procedure TMouseTrackSysControlStyleHook.StartHotTrackTimer;
begin
  if FHotTrackTimer <> nil then
    StopHotTrackTimer;
  FHotTrackTimer := TTimer.Create(nil);
  TTimer(FHotTrackTimer).Interval := 100;
  TTimer(FHotTrackTimer).OnTimer := DoHotTrackTimer;
  TTimer(FHotTrackTimer).Enabled := True;

end;

procedure TMouseTrackSysControlStyleHook.StopHotTrackTimer;
begin
  if FHotTrackTimer <> nil then
  begin
    TTimer(FHotTrackTimer).Enabled := False;
    FreeAndNil(FHotTrackTimer);
  end;
end;

function TMouseTrackSysControlStyleHook.IsChildHandle(AHandle: HWND): Boolean;
begin
  Result := False;
end;

procedure TMouseTrackSysControlStyleHook.DoHotTrackTimer(Sender: TObject);
var
  P: TPoint;
  FWindowHandle: HWND;
begin
  GetCursorPos(P);
  FWindowHandle := WindowFromPoint(P);
  if (FWindowHandle <> Handle) and not IsChildHandle(FWindowHandle) then
  begin
    StopHotTrackTimer;
    FMouseInControl := False;
    MouseLeave;
  end;
end;

procedure TMouseTrackSysControlStyleHook.MouseEnter;
begin

end;

procedure TMouseTrackSysControlStyleHook.MouseLeave;
begin

end;
{$ENDREGION}

end.
