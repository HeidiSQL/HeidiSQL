unit ThemeMgr;

//----------------------------------------------------------------------------------------------------------------------
// Version 1.10.1
//
// Windows XP Theme Manager is freeware. You may freely use it in any software, including commercial software, provided
// you accept the following conditions:
//
// 1) The software may not be included into component collections and similar compilations which are sold. If you want
//    to distribute this software for money then contact me first and ask for my permission.
// 2) My copyright notices in the source code may not be removed or modified.
// 3) If you modify and/or distribute the code to any third party then you must not veil the original author. It must
//    always be clearly identifiable that I, Mike Lischke, am the original author.
// Although it is not required it would be a nice move to recognize my work by adding a citation to the application's
// about box or a similar place.
//
// The original code is ThemeMgr.pas, released 01. January 2002.
//
// The initial developer of the original code is:
//   Mike Lischke (public@soft-gems.net, www.soft-gems.net).
//
// Portions created by Mike Lischke are
// (C) 2001-2005 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// This unit contains the implementation of TThemeManager which is designed to fix certain VCL components to enable
// XP theme support in Delphi and BCB applications (version 6 and lower).
//
// TThemeManager uses global theming (all windows in the application use the same theme). Hence you don't
// need more than one instance in an application (except for DLLs). Having more than one instance in the same module
// (application, DLL) will disable subclassing of controls by all other but the first instance.
//
// Note: If you are using a Theme Manager in a DLL then make sure the handle of the application object in the DLL (which
//       is usually not allocated) is set to that of the main application, e.g. by passing it via an exported function.
//----------------------------------------------------------------------------------------------------------------------
//
// January 2005
//   - Bug fix: Test for Windows XP was wrong.
// 
// For full development history see help file.
// 
// Credits for their valuable help go to:
//   Bert Moorthaemer, Rob Schoenaker, John W. Long, Vassiliev V.V., Steve Moss, Torsten Detsch, Milan Vandrovec
//----------------------------------------------------------------------------------------------------------------------

interface

{$I Compilers.inc}

{$ifdef COMPILER_7_UP}
  ATTENTION! Theme support is already included in this Borland product.
  Remove the Delphi Gems Theme Manager from your project to compile it correctly!
{$endif COMPILER_7_UP}

// The CheckListSupport switch is used to remove support for TCheckListBox. The main reason for this
// is that TCheckListBox is in a special package (VCLX??.dpk), which you may not want to have included
// (particularly when using runtime packages). Disable the switch to remove the link to the package
// and remove the package reference from the ThemeManagerX.dpk file).
{$define CheckListSupport}

uses
  Windows, Classes, Messages, Graphics, Controls, StdCtrls, Buttons, Forms,
  ThemeSrv;

const
  TMVersion = '1.10.1';

  // Sent to any control to give it a chance to deny its subclassing. This is mainly useful for controls
  // which are derived from classes which are usually subclassed by the Theme Manager but do their own
  // painting. A control should return a value <> 0 if subclassing should not be done.
  CM_DENYSUBCLASSING = CM_BASE + 2000;

  {$ifndef COMPILER_5_UP}
    {$EXTERNALSYM WM_CHANGEUISTATE}
    WM_CHANGEUISTATE = $0127;
    {$EXTERNALSYM WM_UPDATEUISTATE}
    WM_UPDATEUISTATE = $0128;
    {$EXTERNALSYM WM_QUERYUISTATE}
    WM_QUERYUISTATE = $0129;
    UIS_CLEAR = 2;
    UISF_HIDEFOCUS = 1;
    UISF_HIDEACCEL = 2;
  {$endif COMPILER_5_UP}

  // These constants are not defined in Delphi/BCB 6 or lower.
  SPI_GETFOCUSBORDERWIDTH = $200E;
  SPI_SETFOCUSBORDERWIDTH = $200F;
  SPI_GETFOCUSBORDERHEIGHT = $2010;
  SPI_SETFOCUSBORDERHEIGHT = $2011;

type
  TThemeOption = (
    toAllowNonClientArea,    // Specifies that the nonclient areas of application windows will have visual styles applied.
    toAllowControls,         // Specifies that the controls used in an application will have visual styles applied.
    toAllowWebContent,       // Specifies that Web content displayed in an application will have visual styles applied.

    toSubclassAnimate,       // Enables subclassing of TAnimate controls (themed painting does not correctly work).
    toSubclassButtons,       // Enables subclassing of button controls (also checkbox, radio button).
    toSubclassCheckListbox,  // Enables subclassing of TCheckListBox.
    toSubclassDBLookup,      // Enables subclassing of TDBLookupControl. Only used in TThemeManagerDB.
    toSubclassFrame,         // Enables subclassing of frames (only available in Delphi 5 or higher).
    toSubclassGroupBox,      // Enables subclassing of group box controls.
    toSubclassListView,      // Enables subclassing of listview controls (including report mode bug fix).
    toSubclassPanel,         // Enables subclassing of panels.
    toSubclassTabSheet,      // Enables subclassing of tab sheet controls.
    toSubclassSpeedButtons,  // Enables subclassing of speed button controls.
    toSubclassSplitter,      // Enables subclassing of splitter controls.
    toSubclassStatusBar,     // Enables subclassing of status bar controls.
    toSubclassTrackBar,      // Enables subclassing of track bar controls (slight paint problems, though).
    toSubclassWinControl,    // Enables subclassing of all window controls not belonging to any of the other classes.

    toResetMouseCapture,     // If set then TToolButtons get their csCaptureMouse flag removed to properly show
                             // their pressed state.
    toSetTransparency,       // If set then TCustomLabel and TToolBar controls are automatically set to transparent.
    toAlternateTabSheetDraw  // If set then use alternate drawing for TTabSheet body.
  );
  TThemeOptions = set of TThemeOption;

const
  DefaultThemeOptions = [toAllowNonClientArea..toAllowWebContent, toSubclassButtons..toSetTransparency];

type
  // These message records are not declared in Delphi 6 and lower.
  TWMPrint = packed record
    Msg: Cardinal;
    DC: HDC;
    Flags: Cardinal;
    Result: Integer;
  end;

  TWMPrintClient = TWMPrint;

  TThemeManager = class;

  TAllowSubclassingEvent = procedure(Sender: TThemeManager; Control: TControl; var Allow: Boolean) of object;
  TControlMessageEvent = procedure(Sender: TThemeManager; Control: TControl; var Message: TMessage;
    var Handled: Boolean) of object;

  PControlMessageEvent = ^TControlMessageEvent;

  // The window procedure list maintains the connections between control instances and their old window procedures.
  TWindowProcList = class(TList)
  private
    FDirty: Boolean;
    FLastControl: TControl;
    FLastIndex: Integer;
    FOwner: TThemeManager;
    FNewWindowProc: TWndMethod;   // The new window procedure which handles the corrections for the control class.
    FControlClass: TControlClass; // The class for which this list is responsible.
  public
    constructor Create(Owner: TThemeManager; WindowProc: TWndMethod; ControlClass: TControlClass);
    destructor Destroy; override;

    function Add(Control: TControl): Integer;
    procedure Clear; override;
    procedure DispatchMessage(Control: TControl; var Message: TMessage);
    function Find(Control: TControl; out Index: Integer): Boolean;
    procedure Remove(Control: TControl);
  end;

  // TThemeManager is a class whose primary task is to fix various issues which show up when an application
  // is themed.
  TThemeManager = class(TComponent)
  private
    FOptions: TThemeOptions;           // Determines which parts are allowed to be themed.
    FPanelList,
    {$ifdef COMPILER_5_UP}
      FFrameList,                      // Frames are first available in Delphi 5.
    {$endif COMPILER_5_UP}
    FListViewList,
    FTabSheetList,
    FWinControlList,
    FGroupBoxList,
    FButtonControlList,
    FSpeedButtonList,
    FSplitterList,
    FTrackBarList,
    FAnimateList,
    FStatusBarList,
    {$ifdef CheckListSupport}
      FCheckListBoxList,
    {$endif CheckListSupport}
    FFormList: TWindowProcList;
    FListeners: TList;
    FPendingFormsList: TList;
    FPendingRecreationList: TList;
    FSubclassingDisabled: Boolean;     // Disable subclassing generally (e.g. for multi instancing).
    FHookWasInstalled: Boolean;

    FOnThemeChange: TNotifyEvent;      // Called when the Windows theme or an application option has changed.
    FOnControlMessage: TControlMessageEvent;
    FOnAllowSubclassing: TAllowSubclassingEvent;
    procedure AnimateWindowProc(Control: TControl; var Message: TMessage);
    procedure ButtonControlWindowProc(Control: TControl; var Message: TMessage);
    {$ifdef CheckListSupport}
      procedure CheckListBoxWindowProc(Control: TControl; var Message: TMessage);
    {$endif CheckListSupport}
    procedure FormWindowProc(Control: TControl; var Message: TMessage);
    {$ifdef COMPILER_5_UP}
      procedure FrameWindowProc(Control: TControl; var Message: TMessage);
    {$endif COMPILER_5_UP}
    function GetIsMainManager: Boolean;
    procedure GroupBoxWindowProc(Control: TControl; var Message: TMessage);
    procedure ListviewWindowProc(Control: TControl; var Message: TMessage);
    function MainWindowHook(var Message: TMessage): Boolean;
    procedure PanelWindowProc(Control: TControl; var Message: TMessage);
    procedure SetThemeOptions(const Value: TThemeOptions);
    procedure SpeedButtonWindowProc(Control: TControl; var Message: TMessage);
    procedure SplitterWindowProc(Control: TControl; var Message: TMessage);
    procedure StatusBarWindowProc(Control: TControl; var Message: TMessage);
    procedure TabSheetWindowProc(Control: TControl; var Message: TMessage);
    procedure TrackBarWindowProc(Control: TControl; var Message: TMessage);
    procedure WinControlWindowProc(Control: TControl; var Message: TMessage);

    procedure PreAnimateWindowProc(var Message: TMessage);
    procedure PreButtonControlWindowProc(var Message: TMessage);
    {$ifdef CheckListSupport}
      procedure PreCheckListBoxWindowProc(var Message: TMessage);
    {$endif CheckListSupport}
    procedure PreFormWindowProc(var Message: TMessage);
    {$ifdef COMPILER_5_UP}
      procedure PreFrameWindowProc(var Message: TMessage);
    {$endif COMPILER_5_UP}
    procedure PreGroupBoxWindowProc(var Message: TMessage);
    procedure PreListviewWindowProc(var Message: TMessage);
    procedure PrePanelWindowProc(var Message: TMessage);
    procedure PreSpeedButtonWindowProc(var Message: TMessage);
    procedure PreSplitterWindowProc(var Message: TMessage);
    procedure PreStatusBarWindowProc(var Message: TMessage);
    procedure PreTabSheetWindowProc(var Message: TMessage);
    procedure PreTrackBarWindowProc(var Message: TMessage);
    procedure PreWinControlWindowProc(var Message: TMessage);
  protected
    procedure AddRecreationCandidate(Control: TControl); virtual;
    procedure BroadcastThemeChange;
    class function CurrentThemeManager: TThemeManager;
    function DoAllowSubclassing(Control: TControl): Boolean; virtual;
    function DoControlMessage(Control: TControl; var Message: TMessage): Boolean; virtual;
    procedure DoOnThemeChange; virtual;
    procedure DrawBitBtn(Control: TBitBtn; var DrawItemStruct: TDrawItemStruct);
    procedure DrawButton(Control: TControl; Button: TThemedButton; DC: HDC; R: TRect; Focused: Boolean);
    function FindListener(AControlMessage: TControlMessageEvent; var Index: Integer): Boolean;
    procedure FixControls(Form: TCustomForm = nil);
    procedure ForceAsMainManager; virtual;
    procedure HandleControlChange(Control: TControl; Inserting: Boolean); virtual;
    function IsRecreationCandidate(Control: TControl): Boolean;
    procedure Loaded; override;
    function NeedsBorderPaint(Control: TControl): Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RemoveChildSubclassing(Control: TWinControl);
    procedure RemoveRecreationCandidate(Control: TControl);
    procedure UpdateThemes;
    procedure UpdateUIState(Control: TControl; CharCode: Word);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearLists;
    procedure CollectForms(Form: TCustomForm = nil);
    procedure CollectControls(Parent: TWinControl);
    procedure PerformEraseBackground(Control: TControl; DC: HDC);
    procedure RegisterListener(AControlMessage: TControlMessageEvent);
    procedure UnregisterListener(AControlMessage: TControlMessageEvent);

    property IsMainManager: Boolean read GetIsMainManager;
  published
    property Options: TThemeOptions read FOptions write SetThemeOptions default DefaultThemeOptions;

    property OnAllowSubclassing: TAllowSubclassingEvent read FOnAllowSubclassing write FOnAllowSubclassing;
    property OnControlMessage: TControlMessageEvent read FOnControlMessage write FOnControlMessage;
    property OnThemeChange: TNotifyEvent read FOnThemeChange write FOnThemeChange;
  end;

var
  IsWindowsXP: Boolean;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  SysUtils, ComCtrls, CommCtrl, SyncObjs, ExtCtrls, Grids, UxTheme
  {$ifdef CheckListSupport}
    , CheckLst
  {$endif CheckListSupport}
  ;

const
  WM_MAINMANAGERRELEASED = CN_NOTIFY + 100;

type
  {$ifndef COMPILER_6_UP}
    // TCustomStatusBar does not exist prior Delphi/BCB 6.
    TCustomStatusBar = TStatusBar;
  {$endif COMPILER_6_UP}
  
  PWindowProcEntry = ^TWindowProcEntry;
  TWindowProcEntry = record
    Control: TControl;
    OldWndProc: TWndMethod;
  end;

var
  Lock: TCriticalSection;

  {$ifdef Debug}
    SubclassCount: Integer;
  {$endif}

var
  MainManager: TThemeManager;
  GlobalCheckWidth,
  GlobalCheckHeight: Integer;

//----------------- Drawing helper routines ----------------------------------------------------------------------------

procedure GetCheckSize;

begin
  with TBitmap.Create do
  try
    Handle := LoadBitmap(0, PChar(32759));
    GlobalCheckWidth := Width div 4;
    GlobalCheckHeight := Height div 3;
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  // Used to access protected properties.
  TControlCast = class(TControl);

procedure CalcButtonLayout(Control: TControl; DC: HDC; const Client: TRect; const Offset: TPoint; var GlyphPos: TPoint;
  var TextBounds: TRect; BiDiFlags: Integer);

// This routine is nearly the same as the same named version in TButtonGlyph. The inclusion here is necessary
// because we need the same layout as in the VCL but the implementation of TButtonGlyph is hidden in Buttons and
// cannot be made accessible from here.

var
  TextPos: TPoint;
  ClientSize,
  GlyphSize,
  TextSize: TPoint;
  TotalSize: TPoint;
  Layout: TButtonLayout;
  Spacing: Integer;
  Margin: Integer;
  Glyph: TBitmap;
  NumGlyphs: Integer;
  Caption: TCaption;
  
begin
  if Control is TBitBtn then
  begin
    Layout := TBitBtn(Control).Layout;
    Spacing := TBitBtn(Control).Spacing;
    Margin := TBitBtn(Control).Margin;
    Glyph := TBitBtn(Control).Glyph;
    NumGlyphs := TBitBtn(Control).NumGlyphs;
    Caption := TBitBtn(Control).Caption;
  end
  else
  begin
    Layout := TSpeedButton(Control).Layout;
    Spacing := TSpeedButton(Control).Spacing;
    Margin := TSpeedButton(Control).Margin;
    Glyph := TSpeedButton(Control).Glyph;
    NumGlyphs := TSpeedButton(Control).NumGlyphs;
    Caption := TSpeedButton(Control).Caption;
  end;
    
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then
      Layout := blGlyphRight
    else
      if Layout = blGlyphRight then
        Layout := blGlyphLeft;

  // Calculate the item sizes.
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);

  if Assigned(Glyph) then
    GlyphSize := Point(Glyph.Width div NumGlyphs, Glyph.Height)
  else
    GlyphSize := Point(0, 0);
    
  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    Windows.DrawText(DC, PChar(Caption), Length(Caption), TextBounds, DT_CALCRECT or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom - TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  // If the layout has the glyph on the right or the left, then both the text and the glyph are centered vertically.
  // If the glyph is on the top or the bottom, then both the text and the glyph are centered horizontally.
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;
    
  // If there is no text or no bitmap, then Spacing is irrelevant.
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  // Adjust Margin and Spacing.
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y + Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y - (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;
    
  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  // Fixup the result variables.
  with GlyphPos do
  begin
    Inc(X, Client.Left + Offset.X);
    Inc(Y, Client.Top + Offset.Y);
  end;
  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.X);
end;

//----------------- TWindowProcList ------------------------------------------------------------------------------------

// For fixing various things in the VCL we have to subclass some of the VCL controls. For each class of control
// one instance of the TWindowProcList is used.

constructor TWindowProcList.Create(Owner: TThemeManager; WindowProc: TWndMethod; ControlClass: TControlClass);

begin
  inherited Create;

  FOwner := Owner;
  FNewWindowProc := WindowProc;
  FControlClass := ControlClass;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TWindowProcList.Destroy;

begin
  Clear;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function Compare(Item1, Item2: Pointer): Integer;

// Helper function for sort and find in window proc lists. They are sorted by control reference.

begin
  Result := Integer(PWindowProcEntry(Item1).Control) - Integer(PWindowProcEntry(Item2).Control);
end;

//----------------------------------------------------------------------------------------------------------------------

function TWindowProcList.Add(Control: TControl): Integer;

var
  I: Integer;
  Entry: PWindowProcEntry;
  ControlWndProc: TWndMethod;

begin
  Result := -1;

  if (Control is FControlClass) and not Find(Control, I) then
  begin
    {$ifdef Debug}
      Lock.Enter;
      try
        Inc(SubclassCount);
      finally
        Lock.Leave;
      end;
    {$endif Debug}

    New(Entry);
    Entry.Control := Control;
    Entry.OldWndProc := Control.WindowProc;

    // The following two lines make sure we get the original control, to which a message is sent, in our
    // proxy window procedures. This works because the Data member of the window proc does not get the reference to
    // the theme manager (as it would happen with ControlWindowProc := FNewWindowProc) but instead we explicitly
    // set the control's reference there (see also first proxy method implementation below).
    TMethod(ControlWndProc).Code := TMethod(FNewWindowProc).Code;
    TMethod(ControlWndProc).Data := Control;
    Control.WindowProc := ControlWndProc;

    Result := inherited Add(Entry);
    FDirty := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowProcList.Clear;

begin
  while Count > 0 do
    Remove(PWindowProcEntry(Items[0]).Control);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowProcList.DispatchMessage(Control: TControl; var Message: TMessage);

var
  I: Integer;
  Entry: PWindowProcEntry;

begin
  if Find(Control, I) then
  begin
    // If a window handle is being recreated then we must ensure the handle is really recreated not only destroyed
    // (this might happen when a hidden window's handle is recreated). Otherwise we will not get notified again about
    // the window's real destruction.
    if Message.Msg = CM_RECREATEWND then
      MainManager.AddRecreationCandidate(Control);

    Entry := Items[I];
    Entry.OldWndProc(Message);

    // If a control is being destroyed then we have to revert the subclassing.
    // We don't get any other opportunity to clean up since TComponent.Notification comes too late and is also not
    // called for controls, which are implicitely freed because their parent is freed.
    if Message.Msg = WM_DESTROY then
    begin
      // Remove any control, which is permanently destroyed, but take care for window recreations.
      if (csDestroying in Control.ComponentState) or not (MainManager.IsRecreationCandidate(Control)) then
        // This call will also remove any child subclassing.
        Remove(Control);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWindowProcList.Find(Control: TControl; out Index: Integer): Boolean;

// Binary search implementation to quickly find a control in the list.

var
  L, H,
  I, C: Integer;
  Dummy: TWindowProcEntry;

begin
  // First try the cached data to speed up retrieval.
  if Control = FLastControl then
  begin
    Result := True;
    Index := FLastIndex;
  end
  else
  begin
    if FDirty and (Count > 1) then
    begin
      Sort(Compare);
      FDirty := False;
    end;

    Result := False;
    Dummy.Control := Control;
    L := 0;
    H := Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := Compare(Items[I], @Dummy);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
          L := I;
        end;
      end;
    end;
    Index := L;
    if Result then
    begin
      FLastControl := Control;
      FLastIndex := L;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowProcList.Remove(Control: TControl);

var
  I: Integer;
  Entry: PWindowProcEntry;

begin
  if Find(Control, I) then
  begin
    Entry := Items[I];
    Delete(I);
    Entry.Control.WindowProc := Entry.OldWndProc;

    // Implicitly release all child subclassing.
    if Entry.Control is TWinControl then
      FOwner.RemoveChildSubclassing(Entry.Control as TWinControl);

    Dispose(Entry);

    {$ifdef Debug}
      Lock.Enter;
      try
        Dec(SubclassCount);
      finally
        Lock.Leave;                                   
      end;
    {$endif Debug}
  end;

  if I <= FLastIndex then
  begin
    FLastControl := nil;
    FLastIndex := -1;
  end;

  MainManager.RemoveRecreationCandidate(Control);
end;

//----------------- TThemeManager --------------------------------------------------------------------------------------

constructor TThemeManager.Create(AOwner: TComponent);

begin
  inherited;

  FListeners := TList.Create;
  FOptions := DefaultThemeOptions;

  FPendingFormsList := TList.Create;
  FPendingRecreationList := TList.Create;
  FListViewList := TWindowProcList.Create(Self, PreListviewWindowProc, TCustomListView);
  FTabSheetList := TWindowProcList.Create(Self, PreTabSheetWindowProc, TTabSheet);
  FGroupBoxList := TWindowProcList.Create(Self, PreGroupBoxWindowProc, TCustomGroupBox);
  FButtonControlList := TWindowProcList.Create(Self, PreButtonControlWindowProc, TButtonControl);
  FSpeedButtonList := TWindowProcList.Create(Self, PreSpeedButtonWindowProc, TSpeedButton);
  FSplitterList := TWindowProcList.Create(Self, PreSplitterWindowProc, TSplitter);
  FTrackBarList := TWindowProcList.Create(Self, PreTrackBarWindowProc, TTrackBar);
  FAnimateList := TWindowProcList.Create(Self, PreAnimateWindowProc, TAnimate);
  FStatusBarList := TWindowProcList.Create(Self, PreStatusBarWindowProc, TCustomStatusBar);
  {$ifdef CheckListSupport}
    FCheckListBoxList := TWindowProcList.Create(Self, PreCheckListBoxWindowProc, TCheckListBox);
  {$endif CheckListSupport}
  FFormList := TWindowProcList.Create(Self, PreFormWindowProc, TCustomForm);
  {$ifdef COMPILER_5_UP}
    FFrameList := TWindowProcList.Create(Self, PreFrameWindowProc, TCustomFrame);
  {$endif COMPILER_5_UP}  
  FPanelList := TWindowProcList.Create(Self, PrePanelWindowProc, TCustomPanel);
  FWinControlList := TWindowProcList.Create(Self, PreWinControlWindowProc, TWinControl);

  if csDesigning in ComponentState then
    FSubclassingDisabled := True
  else
  begin
    if ThemeServices.ThemesEnabled then
    begin
      Application.HookMainWindow(MainWindowHook);
      FHookWasInstalled := True;
    end
    else
      FHookWasInstalled := False;
    
    // Keep the reference of this instance if it is the first one created in the application.
    Lock.Enter;
    try
      // If this is not the first instance then disable subclassing.
      if MainManager = nil then
        MainManager := Self
      else
      begin
        FSubclassingDisabled := True;
        FOptions := MainManager.FOptions;
      end;
    finally
      Lock.Leave;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TThemeManager.Destroy;

begin
  FWinControlList.Free;
  FPanelList.Free;
  {$ifdef COMPILER_5_UP}
    FFrameList.Free;
  {$endif COMPILER_5_UP}
  FFormList.Free;
  {$ifdef CheckListSupport}
    FCheckListBoxList.Free;
  {$endif CheckListSupport}
  FStatusBarList.Free;
  FAnimateList.Free;
  FTrackBarList.Free;
  FSpeedButtonList.Free;
  FSplitterList.Free;
  FButtonControlList.Free;
  FListViewList.Free;
  FTabSheetList.Free;
  FGroupBoxList.Free;

  // Reset first manager reference if it is set to this instance.
  if not (csDesigning in ComponentState) then
  begin
    if FHookWasInstalled then
      Application.UnhookMainWindow(MainWindowHook);

    // We have to check the critical section here because it can happen that it is already freed (finalization section)
    // but there is still a theme manager instance lurking around, due to the finalization order.
    // If there is no lock anymore then the app. is being terminated and we don't need to set a new main manager.
    if Assigned(Lock) then
    begin
      Lock.Enter;
      try
        if MainManager = Self then
        begin
          MainManager := nil;
          if Application.Handle <> 0 then
            SendAppMessage(WM_MAINMANAGERRELEASED, 0, 0);
        end;
      finally
        Lock.Leave;
      end;
    end;
  end;

  FPendingFormsList.Free;
  FPendingRecreationList.Free;
  FListeners.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  // Used to access protected methods and properties.
  TWinControlCast = class(TWinControl);

procedure TThemeManager.AnimateWindowProc(Control: TControl; var Message: TMessage);

begin
  if not DoControlMessage(Control, Message) then
  begin
    if ThemeServices.ThemesEnabled then
    begin
      case Message.Msg of
        WM_ERASEBKGND:
          Message.Result := 1;
        CN_CTLCOLORSTATIC:
          if TAnimate(Control).Transparent then
            with TWMCtlColorStatic(Message) do
            begin
              // Return a brush corresponding to the control's fixed background color.
              // The animation control insists on always erasing its background.
              Result := GetSysColorBrush(TWinControlCast(Control).Color and not $80000000);
              {ThemeServices.DrawParentBackground(TWinControl(Control).Handle, ChildDC, nil, False);
              SetBkMode(ChildDC, TRANSPARENT);
              // Return an empty brush to prevent Windows from overpainting we just have created.
              Result := GetStockObject(NULL_BRUSH);}
            end
          else
            FAnimateList.DispatchMessage(Control, Message);
      else
        FAnimateList.DispatchMessage(Control, Message);
      end;
    end
    else
      FAnimateList.DispatchMessage(Control, Message);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.ButtonControlWindowProc(Control: TControl; var Message: TMessage);

var                                                     
  Details: TThemedElementDetails;
                                                                
begin
  if not DoControlMessage(Control, Message) then
  begin
    if ThemeServices.ThemesEnabled then
    begin
      case Message.Msg of
        CN_KEYDOWN,
        WM_SYSKEYDOWN,
        WM_KEYDOWN:
          begin
            UpdateUIState(Control, TWMKey(Message).CharCode);
            FButtonControlList.DispatchMessage(Control, Message);
          end;
        WM_ERASEBKGND:
          Message.Result := 1;
        CN_CTLCOLORBTN: // TButton background erasing. Necessary for some themes (like EclipseOSX).
          with TWMCtlColorBtn(Message) do
          begin
            if TWinControl(Control.Parent).DoubleBuffered then
              PerformEraseBackground(Control, ChildDC)
            else
              ThemeServices.DrawParentBackground(TWinControl(Control).Handle, ChildDC, nil, False);
            // Return an empty brush to prevent Windows from overpainting we just have created.
            Result := GetStockObject(NULL_BRUSH);
          end;
        CN_CTLCOLORSTATIC: // Background erasing for check boxes and radio buttons. 
          with TWMCtlColorStatic(Message) do
          begin
            if TWinControl(Control.Parent).DoubleBuffered then
              PerformEraseBackground(Control, ChildDC)
            else
              ThemeServices.DrawParentBackground(TWinControl(Control).Handle, ChildDC, nil, False);
            // Return an empty brush to prevent Windows from overpainting we just have created.
            Result := GetStockObject(NULL_BRUSH);
          end;
        CM_MOUSEENTER,
        CM_MOUSELEAVE:
          begin
            // Hot tracking for owner drawn buttons seems to be unsupported by Windows. So we have to work around that.
            if Control is TBitBtn then
              Control.Invalidate;
            FButtonControlList.DispatchMessage(Control, Message);
          end;
        CN_DRAWITEM: // Painting for owner drawn buttons.
          with TWMDrawItem(Message) do
          begin
            // This message is sent for bit buttons (TBitBtn) when they must be drawn. Since a bit button is a normal
            // Windows button (but with custom draw enabled) it is handled here too.
            // TSpeedButton is a TGraphicControl descentant and handled separately.
            Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
            ThemeServices.DrawParentBackground(TWinControl(Control).Handle, DrawItemStruct.hDC, @Details, True);

            // CN_DRAWITEM can also come in when the control is a subclassed button with enabled custom draw.
            // In this case the content of the control is fully controlled by the original source. So let it do
            // whatever it wants to do.
            if (Control is TBitBtn) or (Control is TSpeedButton) then
              DrawBitBtn(TBitBtn(Control), DrawItemStruct^)
            else
              FButtonControlList.DispatchMessage(Control, Message);
          end;
      else
        FButtonControlList.DispatchMessage(Control, Message);
      end;
    end
    else
      FButtonControlList.DispatchMessage(Control, Message);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{$ifdef CheckListSupport}
  type
    TCheckListBoxCast = class(TCheckListBox);

  procedure TThemeManager.CheckListBoxWindowProc(Control: TControl; var Message: TMessage);

  var
    DrawState: TOwnerDrawState;
    ListBox: TCheckListBoxCast;

    //--------------- local functions -------------------------------------------

    procedure DrawCheck(R: TRect; AState: TCheckBoxState; Enabled: Boolean);

    var
      DrawRect: TRect;
      Button: TThemedButton;
      Details: TThemedElementDetails;

    begin
      DrawRect.Left := R.Left + (R.Right - R.Left - GlobalCheckWidth) div 2;
      DrawRect.Top := R.Top + (R.Bottom - R.Top - GlobalCheckWidth) div 2;
      DrawRect.Right := DrawRect.Left + GlobalCheckWidth;
      DrawRect.Bottom := DrawRect.Top + GlobalCheckHeight;
      case AState of
        cbChecked:
          if Enabled then
            Button := tbCheckBoxCheckedNormal
          else
            Button := tbCheckBoxCheckedDisabled;
        cbUnchecked:
          if Enabled then
            Button := tbCheckBoxUncheckedNormal
          else
            Button := tbCheckBoxUncheckedDisabled;
        else // cbGrayed
          if Enabled then
            Button := tbCheckBoxMixedNormal
          else
            Button := tbCheckBoxMixedDisabled;
      end;

      Details := ThemeServices.GetElementDetails(Button);
      ThemeServices.DrawElement(ListBox.Canvas.Handle, Details, DrawRect, @DrawRect);
    end;

    //---------------------------------------------------------------------------
  
    procedure NewDrawItem(Index: Integer; Rect: TRect; DrawState: TOwnerDrawState);

    var
      Flags: Integer;
      Data: string;
      R: TRect;
      ACheckWidth: Integer;
      Enable: Boolean;

    begin
      with ListBox do
      begin
        // The checkbox is always drawn, regardless of the owner draw style.
        ACheckWidth := GetCheckWidth;
        if Index < Items.Count then
        begin
          R := Rect;
          // Delphi 4 has neither an enabled state nor a header state for items.
          Enable := Enabled {$ifdef COMPILER_6_UP} and ItemEnabled[Index] {$endif COMPILER_6_UP};
          if {$ifdef COMPILER_6_UP} not Header[Index] {$else} True {$endif COMPILER_6_UP} then
          begin
            if not UseRightToLeftAlignment then
            begin
              R.Right := Rect.Left;
              R.Left := R.Right - ACheckWidth;
            end
            else
            begin
              R.Left := Rect.Right;
              R.Right := R.Left + ACheckWidth;
            end;
            DrawCheck(R, State[Index], Enable);
          end
          else
          begin
            {$ifdef COMPILER_6_UP}
              Canvas.Font.Color := HeaderColor;
              Canvas.Brush.Color := HeaderBackgroundColor;
            {$endif COMPILER_6_UP}
          end;
          if not Enable then
            Canvas.Font.Color := clGrayText;
        end;

        if Assigned(OnDrawItem) and (Style <> lbStandard)then
          OnDrawItem(ListBox, Index, Rect, DrawState)
        else
        begin
          Canvas.FillRect(Rect);
          if Index < {$ifdef COMPILER_6_UP} Count {$else} Items.Count {$endif COMPILER_6_UP}then
          begin
            Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
            if not UseRightToLeftAlignment then
              Inc(Rect.Left, 2)
            else
              Dec(Rect.Right, 2);
            Data := '';
            {$ifdef COMPILER_6_UP}
              if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
                Data := DoGetData(Index)
              else
            {$endif COMPILER_6_UP}
                Data := Items[Index];

            DrawText(Canvas.Handle, PChar(Data), Length(Data), Rect, Flags);
          end;
        end;
      end;
    end;
  
    //--------------- end local function ----------------------------------------

  begin
    if not DoControlMessage(Control, Message) then
    begin
      if ThemeServices.ThemesEnabled then
      begin
        ListBox := TCheckListBoxCast(Control);
        case Message.Msg of
          CN_DRAWITEM:
            if {$ifdef COMPILER_6_UP} ListBox.Count > 0 {$else} ListBox.Items.Count > 0 {$endif COMPILER_6_UP} then
              with TWMDrawItem(Message).DrawItemStruct^, ListBox do
              begin
                if {$ifdef COMPILER_6_UP} not Header[itemID] {$else} True {$endif COMPILER_6_UP} then
                  if not UseRightToLeftAlignment then
                    rcItem.Left := rcItem.Left + GetCheckWidth
                  else
                    rcItem.Right := rcItem.Right - GetCheckWidth;
                {$ifdef COMPILER_5_UP}
                  DrawState := TOwnerDrawState(LongRec(itemState).Lo);
                {$else}
                  DrawState := TOwnerDrawState(Byte(LongRec(itemState).Lo));
                {$endif COMPILER_5_UP}
                Canvas.Handle := hDC;
                Canvas.Font := Font;
                Canvas.Brush := Brush;
                if (Integer(itemID) >= 0) and (odSelected in DrawState) then
                begin
                  Canvas.Brush.Color := clHighlight;
                  Canvas.Font.Color := clHighlightText
                end;
                if Integer(itemID) >= 0 then
                  NewDrawItem(itemID, rcItem, DrawState)
                else
                  Canvas.FillRect(rcItem);
                if odFocused in DrawState then
                  DrawFocusRect(hDC, rcItem);
                Canvas.Handle := 0;
              end;
        else
          FCheckListBoxList.DispatchMessage(Control, Message);
        end;
      end
      else
        FCheckListBoxList.DispatchMessage(Control, Message);
    end
    else
      FCheckListBoxList.DispatchMessage(Control, Message);
  end;

{$endif CheckListSupport}

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.FormWindowProc(Control: TControl; var Message: TMessage);

var
  DC: HDC;

begin
  case Message.Msg of
    CM_CONTROLLISTCHANGE: // Single control addition or removal.
      with TCMControlListChange(Message) do
        HandleControlChange(Control, Inserting);
  end;

  if not DoControlMessage(Control, Message) then
  begin
    if ThemeServices.ThemesEnabled then
    begin
      case Message.Msg of
        WM_PRINTCLIENT,
        WM_ERASEBKGND:
          begin
            if (Message.Msg=WM_PRINTCLIENT) then
              DC := TWMPrintClient(Message).DC
            else
              DC := TWMEraseBkGnd(Message).DC;

            // Get the parent to draw its background into the form's background.
            if not (Control.Parent is TWinControl) then
              FFormList.DispatchMessage(Control, Message)
            else
              if TWinControl(Control.Parent).DoubleBuffered then
                PerformEraseBackground(Control, DC)
              else
                if TWinControl(Control).DoubleBuffered then
                begin
                  if (Message.Msg <> WM_ERASEBKGND) or (Longint(DC) = TWMEraseBkGnd(Message).Unused) then
                    //  VCL mark for second pass, this time into the offscreen bitmap
                    PerformEraseBackground(Control, DC);
                end
            else
              DrawThemeParentBackground(TWinControl(Control).Handle, DC, nil);
            Message.Result := 1;
          end;
      else
        FFormList.DispatchMessage(Control, Message);
      end;
    end
    else
      FFormList.DispatchMessage(Control, Message);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{$ifdef COMPILER_5_UP}

  type
    // Used to access protected properties.
    TFrameCast = class(TCustomFrame);

  procedure TThemeManager.FrameWindowProc(Control: TControl; var Message: TMessage);

  var
    PS: TPaintStruct;
    Details: TThemedElementDetails;

  begin
    if not DoControlMessage(Control, Message) then
    begin
      if ThemeServices.ThemesEnabled then
      begin
        case Message.Msg of
          WM_ERASEBKGND:
            with TWMEraseBkGnd(Message) do
            begin
              // Get the parent to draw its background into the control's background.
              if TWinControl(Control.Parent).DoubleBuffered then
                PerformEraseBackground(Control, DC)
              else
              begin
                Details := ThemeServices.GetElementDetails(tbGroupBoxNormal);
                ThemeServices.DrawParentBackground(TWinControl(Control).Handle, DC, @Details, False);
              end;
              Result := 1;
            end;
          WM_PAINT:
            begin
              BeginPaint(TFrameCast(Control).Handle, PS);
              TFrameCast(Control).PaintControls(PS.hdc, nil);
              EndPaint(TFrameCast(Control).Handle, PS);
              Message.Result := 0;
            end;
        else
          FFrameList.DispatchMessage(Control, Message);
        end;
      end
      else
        FFrameList.DispatchMessage(Control, Message);
    end;
  end;
  
{$endif COMPILER_5_UP}

//----------------------------------------------------------------------------------------------------------------------

function TThemeManager.GetIsMainManager: Boolean;

begin
  Result := MainManager = Self;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  // Used to access protected properties.
  TGroupBoxCast = class(TCustomGroupBox);

procedure TThemeManager.GroupBoxWindowProc(Control: TControl; var Message: TMessage);

  //--------------- local function --------------------------------------------

  procedure NewPaint(DC: HDC);

  var
    CaptionRect,
    OuterRect: TRect;
    Size: TSize;
    LastFont: HFONT;
    Box: TThemedButton;
    Details: TThemedElementDetails;

  begin
    with TGroupBoxCast(Control) do
    begin
      LastFont := SelectObject(DC, Font.Handle);
      if Text <> '' then
      begin
        SetTextColor(DC, Graphics.ColorToRGB(Font.Color));
        // Determine size and position of text rectangle.
        // This must be clipped out before painting the frame.
        GetTextExtentPoint32(DC, PChar(Text), Length(Text), Size);
        CaptionRect := Rect(0, 0, Size.cx, Size.cy);
        if not UseRightToLeftAlignment then
          OffsetRect(CaptionRect, 8, 0)
        else
          OffsetRect(CaptionRect, Width - 8 - CaptionRect.Right, 0);
      end
      else
        CaptionRect := Rect(0, 0, 0, 0);

      OuterRect := ClientRect;
      OuterRect.Top := (CaptionRect.Bottom - CaptionRect.Top) div 2;
      with CaptionRect do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      if Control.Enabled then
        Box := tbGroupBoxNormal
      else
        Box := tbGroupBoxDisabled;
      Details := ThemeServices.GetElementDetails(Box);
      ThemeServices.DrawElement(DC, Details, OuterRect);

      SelectClipRgn(DC, 0);
      if Text <> '' then
        ThemeServices.DrawText(DC, Details, Text, CaptionRect, DT_LEFT, 0);
      SelectObject(DC, LastFont);
    end;
  end;

  //--------------- local function --------------------------------------------

var
  PS: TPaintStruct;
  Details: TThemedElementDetails;

begin
  if not DoControlMessage(Control, Message) then
  begin
    if ThemeServices.ThemesEnabled then
    begin
      case Message.Msg of
        WM_SYSKEYDOWN,
        CN_KEYDOWN,
        WM_KEYDOWN:
          begin
            UpdateUIState(Control, TWMKey(Message).CharCode);
            FGroupBoxList.DispatchMessage(Control, Message);
          end;
        WM_ERASEBKGND:
          with TWMEraseBkGnd(Message) do
          begin
            // Get the parent to draw its background into the control's background.
            if TWinControl(Control.Parent).DoubleBuffered then
              PerformEraseBackground(Control, DC)
            else
            begin
              Details := ThemeServices.GetElementDetails(tbGroupBoxNormal);
              ThemeServices.DrawParentBackground(TGroupBoxCast(Control).Handle, DC, @Details, True);
            end;
            Result := 1;
          end;
        WM_PAINT:
          begin
            BeginPaint(TGroupBoxCast(Control).Handle, PS);
            NewPaint(PS.hdc);
            TGroupBoxCast(Control).PaintControls(PS.hdc, nil);
            EndPaint(TGroupBoxCast(Control).Handle, PS);
            Message.Result := 0;
          end;
      else
        FGroupBoxList.DispatchMessage(Control, Message);
      end;
    end
    else
      FGroupBoxList.DispatchMessage(Control, Message);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.ListviewWindowProc(Control: TControl; var Message: TMessage);

begin
  if not DoControlMessage(Control, Message) then
  begin
    // In opposition to the other window procedures we should always apply the fix for TListView,
    // regardless of whether themes are enabled or not.
    if (Message.Msg = LVM_SETCOLUMN) or (Message.Msg = LVM_INSERTCOLUMN) then
    begin
      with PLVColumn(Message.LParam)^ do
      begin
        // Fix TListView report mode bug.
        if iImage = - 1 then
          Mask := Mask and not LVCF_IMAGE;
      end;
    end;

    // This special notification message is not handled in the VCL and creates an access violation when
    // passed to the default window procedure. Ignoring it does not seem to have any negative impact.
    if not ((Message.Msg = WM_NOTIFY) and (TWMNotify(Message).NMHdr.code = HDN_GETDISPINFOW)) then
      FListViewList.DispatchMessage(Control, Message);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeManager.MainWindowHook(var Message: TMessage): Boolean;

// Listens to messages sent to the application to know when a theme change occured.

var
  Form: TCustomForm;
  
begin
  Result := False;

  // If the main manager was destroyed then it posted this message to the application so all still existing
  // theme managers know a new election is due. Well, it is not purely democratic. The earlier a manager was created
  // the higher is the probability to get this message first and become the new main manager.
  if Message.Msg = WM_MAINMANAGERRELEASED then
  begin
    Lock.Enter;
    try
      // Check if the main manager role is still vacant.
      if MainManager = nil then
      begin
        MainManager := Self;
        FSubclassingDisabled := False;
        CollectForms;
      end;
    finally
      Lock.Leave;
    end;
  end;

  // Check first if there are still forms to subclass.
  while FPendingFormsList.Count > 0 do
  begin
    Form := TCustomForm(FPendingFormsList[0]);
    FPendingFormsList.Delete(0);
    FFormList.Add(Form);
    // Since we don't know how many controls on this form already have been created we better collect everything
    // which is already there. The window proc lists will take care not to add a control twice.
    if MainManager = Self then
      CollectControls(Form);
    if [toResetMouseCapture, toSetTransparency] * FOptions <> [] then
      FixControls(Form);
    // Sometimes not all controls are visually updated. Force it to be correct.
    RedrawWindow(Form.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN or RDW_VALIDATE);
  end;

  while FPendingRecreationList.Count > 0 do
  begin
    TWinControl(FPendingRecreationList[0]).HandleNeeded;
    CollectControls(TWinControl(FPendingRecreationList[0]));
    FPendingRecreationList.Delete(0);
  end;

  if Message.Msg = WM_THEMECHANGED then
  begin
    UpdateThemes;
    DoOnThemeChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  // Used to access protected properties.
  TPanelCast = class(TCustomPanel);

procedure TThemeManager.PanelWindowProc(Control: TControl; var Message: TMessage);

var
  DrawRect: TRect;
  DC: HDC;
  OldFont: HFONT;
  PS: TPaintStruct;
  Details: TThemedElementDetails;

  //--------------- local function --------------------------------------------

  procedure NewPaint;

  // This is an adapted version of the actual TCustomPanel.Paint procedure

  const
    Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);

  var
    Rect: TRect;
    TopColor, BottomColor: TColor;
    FontHeight: Integer;
    Flags: Longint;

    //------------- local functions -------------------------------------------

    procedure AdjustColors(Bevel: TPanelBevel);

    begin
      TopColor := clBtnHighlight;
      if Bevel = bvLowered then
        TopColor := clBtnShadow;
      BottomColor := clBtnShadow;
      if Bevel = bvLowered then
        BottomColor := clBtnHighlight;
    end;

    //------------- end local functions ---------------------------------------

  begin
    with TPanelCast(Control) do
    begin
      Canvas.Handle := DC;
      try
        Canvas.Font := Font;
        Rect := GetClientRect;
        if BevelOuter <> bvNone then
        begin
          AdjustColors(BevelOuter);
          Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
        end;
        InflateRect(Rect, -BorderWidth, -BorderWidth);
        if BevelInner <> bvNone then
        begin
          AdjustColors(BevelInner);
          Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
        end;
        if ParentColor or ((Control.Parent <> nil) and (Control.Parent.Brush.Color = Color)) then
        begin
          if TWinControl(Control.Parent).DoubleBuffered then
            PerformEraseBackground(Control, PS.hdc)
          else
          begin
            Details := ThemeServices.GetElementDetails(tbGroupBoxNormal);
            ThemeServices.DrawParentBackground(Handle, DC, @Details, False, @Rect);
          end
        end
        else
        begin
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := Color;
          FillRect(PS.hdc, Rect, Canvas.Brush.Handle);
        end;
        FontHeight := Canvas.TextHeight('W');
        with Rect do
        begin
          Top := ((Bottom + Top) - FontHeight) div 2;
          Bottom := Top + FontHeight;
        end;
        Flags := DT_EXPANDTABS or DT_VCENTER or Alignments[Alignment];
        Flags := DrawTextBiDiModeFlags(Flags);
        OldFont := SelectObject(DC, Font.Handle);
        SetBKMode(DC, TRANSPARENT);
        SetTextColor(DC, ColorToRGB(Font.Color));
        DrawText(DC, PChar(Caption), -1, Rect, Flags);
        SelectObject(DC, OldFont);
      finally
        Canvas.Handle := 0;
      end;
    end;
  end;

  //--------------- end local function ----------------------------------------

begin
  if not DoControlMessage(Control, Message) then
  begin
    if ThemeServices.ThemesEnabled and TPanelCast(Control).ParentColor or
      (Assigned(Control.Parent) and (Control.Parent.Brush.Color = TPanelCast(Control).Color)) then
    begin
      case Message.Msg of
        WM_ERASEBKGND:
          with TPanelCast(Control) do
          begin
            DC := TWMEraseBkGnd(Message).DC;
            // Get the parent to draw its background into the control's background.
            if TWinControl(Control.Parent).DoubleBuffered then
              PerformEraseBackground(Control, DC)
            else
            begin
              Details := ThemeServices.GetElementDetails(tbGroupBoxNormal);
              ThemeServices.DrawParentBackground(Handle, DC, @Details, False);
            end;
            Message.Result := 1;
          end;
        WM_NCPAINT:
          with TPanelCast(Control) do
          begin
            FPanelList.DispatchMessage(Control, Message);
            if BorderStyle <> bsNone then
            begin
              DrawRect := BoundsRect;
              OffsetRect(DrawRect, -Left, -Top);
              DC := GetWindowDC(Handle);
              try
                Details := ThemeServices.GetElementDetails(trBandNormal);
                ThemeServices.DrawEdge(DC, Details, DrawRect, EDGE_SUNKEN, BF_RECT);
              finally
                ReleaseDC(Handle, DC);
              end;
            end;
            Message.Result := 0;
          end;
        WM_PAINT:
          with TPanelCast(Control) do
          begin
            DC := BeginPaint(Handle, PS);
            NewPaint;
            PaintControls(DC, nil);
            EndPaint(Handle, PS);
            Message.Result := 0;
          end;
        WM_PRINTCLIENT:
          with TPanelCast(Control) do
          begin
            DC := TWMPrintClient(Message).DC;
            NewPaint;
            PaintControls(DC, nil);
            Message.Result := 0;
          end;
      else
        FPanelList.DispatchMessage(Control, Message);
      end;
    end
    else
      FPanelList.DispatchMessage(Control, Message);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.SetThemeOptions(const Value: TThemeOptions);

var
  Flags: Cardinal;
  I: Integer;

begin
  // If this instance is the main manager then apply the options directly. Otherwise let the current main manager do it.
  Lock.Enter;
  try
    if Assigned(MainManager) and (MainManager <> Self) then
      MainManager.Options := Value
    else
    begin
      if FOptions <> Value then
      begin
        FOptions := Value;

        if ThemeServices.ThemesAvailable and not FSubclassingDisabled and not (csDesigning in ComponentState) then
        begin
          Flags := 0;
          if toAllowNonClientArea in FOptions then
            Flags := Flags or STAP_ALLOW_NONCLIENT;
          if toAllowControls in FOptions then
            Flags := Flags or STAP_ALLOW_CONTROLS;
          if toAllowWebContent in FOptions then
            Flags := Flags or STAP_ALLOW_WEBCONTENT;
          SetThemeAppProperties(Flags);
          if ComponentState * [csLoading, csReading] = [] then
          begin
            UpdateThemes;

            // Tell the application that we changed the options.
            BroadcastThemeChange;
            // Notify all theme manager instances about the change.
            SendAppMessage(WM_THEMECHANGED, 0, 0);

            for I := 0 to Screen.FormCount - 1 do
              RedrawWindow(Screen.Forms[I].Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_INTERNALPAINT or
                RDW_ERASENOW or RDW_UPDATENOW or RDW_ALLCHILDREN);
          end;
        end;
      end;
    end;
  finally
    Lock.Leave;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TSpeedButtonCast = class(TSpeedButton);

procedure TThemeManager.SpeedButtonWindowProc(Control: TControl; var Message: TMessage);

var
  Button: TThemedButton;
  P: TPoint;

begin
  if not DoControlMessage(Control, Message) then
  begin
    if ThemeServices.ThemesEnabled then
    begin
      case Message.Msg of
        WM_PAINT:
          with TWMPaint(Message) do
          begin
            // We cannot use the theme parent paint for the background of general speed buttons (because they are not
            // window controls).
            PerformEraseBackground(Control, DC);

            // Speed buttons are not window controls and are painted by a call of their parent with a given DC.
            if not Control.Enabled then
              Button := tbPushButtonDisabled
            else
              if TSpeedButtonCast(Control).FState in [bsDown, bsExclusive] then
                Button := tbPushButtonPressed
              else
              with TSpeedButtonCast(Control) do
              begin
                // Check the hot style here. If the button has a flat style then this check is easy. Otherwise
                // some more work is necessary.
                Button := tbPushButtonNormal;
                if Flat then
                begin
                  if MouseInControl then
                    Button := tbPushButtonHot;
                end
                else
                begin
                  GetCursorPos(P);
                  if FindDragTarget(P, True) = Control then
                    Button := tbPushButtonHot;
                end;
              end;
            DrawButton(Control, Button, DC, Control.ClientRect, False);
            Message.Result := 0;
          end;
        CM_MOUSEENTER,
        CM_MOUSELEAVE:
          begin
            // Non-flat speed buttons don't have a hot-tracking style. We have to emulate this.
            if not TSpeedButtonCast(Control).Flat and Control.Enabled then
              Control.Invalidate;
            FSpeedButtonList.DispatchMessage(Control, Message);
          end;
      else
        FSpeedButtonList.DispatchMessage(Control, Message);
      end;
    end
    else
      FSpeedButtonList.DispatchMessage(Control, Message);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.SplitterWindowProc(Control: TControl; var Message: TMessage);

begin
  if not DoControlMessage(Control, Message) then
  begin
    if ThemeServices.ThemesEnabled then
    begin
      case Message.Msg of
        WM_PAINT:
          with TWMPaint(Message) do
          begin
            PerformEraseBackground(Control, DC);
            Message.Result := 0;
          end;
      else
        FSplitterList.DispatchMessage(Control, Message);
      end;
    end
    else
      FSplitterList.DispatchMessage(Control, Message);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TCustomStatusBarCast = class(TCustomStatusBar);

procedure TThemeManager.StatusBarWindowProc(Control: TControl; var Message: TMessage);

var
  Details: TThemedElementDetails;

begin
  if not DoControlMessage(Control, Message) then
  begin
    if ThemeServices.ThemesEnabled then
    begin
      case Message.Msg of
        WM_NCCALCSIZE:
          with TWMNCCalcSize(Message) do
          begin
            FStatusBarList.DispatchMessage(Control, Message);
            // We cannot simply override the window class' CS_HREDRAW and CS_VREDRAW styles but the following
            // does the job very well too.
            // Note: this may produce trouble with embedded controls (e.g. progress bars).
            if CalcValidRects then
              Result := Result or WVR_REDRAW;
          end;
        WM_ERASEBKGND:
          with TWMEraseBkGnd(Message) do
          begin
            Details := ThemeServices.GetElementDetails(tsStatusRoot);
            ThemeServices.DrawElement(DC, Details, Control.ClientRect);
            Message.Result := 1;
          end;
      else
        FStatusBarList.DispatchMessage(Control, Message);
      end;
    end
    else
      FStatusBarList.DispatchMessage(Control, Message);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.TabSheetWindowProc(Control: TControl; var Message: TMessage);

var
  DrawRect: TRect;
  Details: TThemedElementDetails;
  DC: HDC;

begin
  if not DoControlMessage(Control, Message) then
  begin
    if ThemeServices.ThemesEnabled then
    begin
      case Message.Msg of
        // Paint the border (and erase the background)
        WM_NCPAINT:
          with TTabSheet(Control) do
          begin
            DC := GetWindowDC(Handle);
            try
              // Exclude the client area from painting. We only want to erase the non-client area.
              DrawRect := ClientRect;
              OffsetRect(DrawRect, BorderWidth, BorderWidth);
              with DrawRect do
                ExcludeClipRect(DC, Left, Top, Right, Bottom);
              // The parent paints relative to the control's client area. We have to compensate for this by
              // shifting the dc's window origin.
              SetWindowOrgEx(DC, -BorderWidth, -BorderWidth, nil);
              Details := ThemeServices.GetElementDetails(ttBody);
              ThemeServices.DrawParentBackground(TWinControl(Control).Handle, DC, @Details, False);
            finally
              ReleaseDC(Handle, DC);
            end;
            Message.Result := 0;
          end;
        WM_PRINTCLIENT,
        WM_ERASEBKGND:
          begin
            if Message.Msg = WM_PRINTCLIENT then
              DC := TWMPrintClient(Message).DC
            else
              DC := TWMEraseBkGnd(Message).DC;
            // Using the parent's background here does not always work. Particularly, it does not work in cases
            // where the parent (pane) background does not include the body background. One way to solve this problem
            // would be to paint the body background here. However this produces a lot of problems all caused by
            // the fact that these backgrounds might be tiled or might otherwise have special drawing style.
            // Due to the near-to-non-existing documentation on all the themes APIs I use the lesser evil by default and
            // paint the parent background, which works in most cases very well.
            // However you may want to enable the other way, if needed.
            if toAlternateTabSheetDraw in FOptions then
            begin
              Details := ThemeServices.GetElementDetails(ttBody);
              DrawRect := Control.ClientRect;
              ThemeServices.DrawElement(DC, Details, DrawRect);
            end
            else
              ThemeServices.DrawParentBackground(TWinControl(Control).Handle, DC, nil, False);
            Message.Result := 1;
          end;
      else
        FTabSheetList.DispatchMessage(Control, Message);
      end;
    end
    else
      FTabSheetList.DispatchMessage(Control, Message);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.TrackBarWindowProc(Control: TControl; var Message: TMessage);

var
  Info: PNMCustomDraw;
  R: TRect;
  Rgn: HRGN;
  Details: TThemedElementDetails;
  Offset: Integer;
  FocusBorderWidth,
  FocusBorderHeight: Integer;

begin
  if not DoControlMessage(Control, Message) then
  begin
    if ThemeServices.ThemesEnabled then
    begin
      case Message.Msg of
        CN_NOTIFY:
          with TWMNotify(Message) do
            if NMHdr.code = NM_CUSTOMDRAW then
            begin
              Info := Pointer(NMHdr);
              case Info.dwDrawStage of
                CDDS_PREPAINT:
                  Result := CDRF_NOTIFYITEMDRAW;
                CDDS_ITEMPREPAINT:
                  with Control as TTrackBar do
                  begin
                    // Take action based on which item is about to be painted.
                    case Info.dwItemSpec of
                      TBCD_TICS: // Before re-painting ticks redo whole background.
                        begin
                          R := ClientRect;
                          // Leave room for the focus rectangle if there is one.
                          if Focused and ((Perform(WM_QUERYUISTATE, 0, 0) and UISF_HIDEFOCUS) = 0) then
                          begin
                            SystemParametersInfo(SPI_GETFOCUSBORDERWIDTH, 0, @FocusBorderWidth, 0);
                            SystemParametersInfo(SPI_GETFOCUSBORDERHEIGHT, 0, @FocusBorderHeight, 0);
                            InflateRect(R, -FocusBorderWidth, -FocusBorderHeight);
                          end;
                          ThemeServices.DrawParentBackground(Handle, Info.hDC, nil, False, @R);
                        end;
                      TBCD_CHANNEL: // Before re-painting channel just redo strip of background overlapped.
                        begin
                          // Retrieve the bounding box for the thumb.
                          SendMessage(Handle, TBM_GETTHUMBRECT, 0, Integer(@R));
                          // Extend this rectangle to the top/bottom or left/right border, respectively.
                          Offset := 0;
                          if Orientation = trHorizontal then
                          begin
                            // Leave room for the focus rectangle if there is one.
                            if Focused then
                            begin
                              SystemParametersInfo(SPI_GETFOCUSBORDERWIDTH, 0, @FocusBorderWidth, 0);
                              Inc(Offset, FocusBorderWidth);
                            end;
                            R.Left := ClientRect.Left + Offset;
                            R.Right := ClientRect.Right - Offset;
                          end
                          else
                          begin
                            // Leave room for the focus rectangle if there is one.
                            if Focused then
                            begin
                              SystemParametersInfo(SPI_GETFOCUSBORDERHEIGHT, 0, @FocusBorderHeight, 0);
                              Inc(Offset, FocusBorderWidth);
                            end;
                            R.Top := ClientRect.Top + Offset;
                            R.Bottom := ClientRect.Bottom - Offset;
                          end;
                          with R do
                            Rgn := CreateRectRgn(Left, Top, Right, Bottom);
                          SelectClipRgn(Info.hDC, Rgn);
                          Details := ThemeServices.GetElementDetails(ttbThumbTics);
                          ThemeServices.DrawParentBackground(Handle, Info.hDC, @Details, False);
                          DeleteObject(Rgn);
                          SelectClipRgn(Info.hDC, 0);
                        end;
                    end;
                    Result := CDRF_DODEFAULT;
                  end;
              else
                Result := CDRF_DODEFAULT;
              end;
            end;
      else
        FTrackBarList.DispatchMessage(Control, Message);
      end;
    end
    else
      FTrackBarList.DispatchMessage(Control, Message);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.WinControlWindowProc(Control: TControl; var Message: TMessage);

var
  DC: HDC;
  SavedDC: Integer;

begin
  if not DoControlMessage(Control, Message) then
  begin
    if ThemeServices.ThemesEnabled then
    begin
      case Message.Msg of
        CN_KEYDOWN,
        WM_SYSKEYDOWN,
        WM_KEYDOWN:
          begin
            UpdateUIState(Control, TWMKey(Message).CharCode);
            FWinControlList.DispatchMessage(Control, Message);
          end;
        WM_ERASEBKGND:
          begin
            if Control is TScrollingWinControl then
              with Control as TWinControl do
              begin
                DC := TWMEraseBkGnd(Message).DC;                     
                if DoubleBuffered then
                  PerformEraseBackground(Control, DC)
                else
                  ThemeServices.DrawParentBackground(Handle, DC, nil, False);
                Message.Result := 1;
              end
              else
                FWinControlList.DispatchMessage(Control, Message);
          end;
        WM_NCPAINT:
          begin
            FWinControlList.DispatchMessage(Control, Message);
            ThemeServices.PaintBorder(Control as TWinControl, Control is TCustomGrid);
          end;
        CN_CTLCOLORSTATIC:
          if Control is TCustomStaticText then
            with TWMCtlColorStatic(Message), Control as TWinControl do
            begin
              SetBkMode(ChildDC, Windows.TRANSPARENT);
              SavedDC := SaveDC(ChildDC);
              ThemeServices.DrawParentBackground(Handle, ChildDC, nil, False);
              FWinControlList.DispatchMessage(Control, Message);
              RestoreDC(ChildDC, SavedDC);
              // Return an empty brush to prevent Windows from overpainting what we just have created.
              Result := GetStockObject(NULL_BRUSH);
            end
          else
            FWinControlList.DispatchMessage(Control, Message);
      else
        FWinControlList.DispatchMessage(Control, Message);
      end;
    end
    else
      FWinControlList.DispatchMessage(Control, Message);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PreAnimateWindowProc(var Message: TMessage);

// This and the other proxy window procs do an important step to make the entire subclassing work here.
// Because we have only one window procedure for each class of subclassed controls (many to 1 relation), it is necessary
// to know to which control the message was sent originally (read: whose WindowProc property had been called). This is
// important because we have to forward the message to the original window procedure once we are finished with our own
// processing and sometimes properties of the control are needed too.
// When this method is called the hidden self parameter is not the actual theme manager instance but the
// control reference to which the message was sent originally. This is the result from the explicit Data member
// assignment done in TWindowProcList.Add. This is very helpful but has the side effect that we don't have the theme
// manager instance anymore (since the self param is the control). Thus we need another reference, which we have
// in the form of the main manager. Since only the main manager will subclass controls it is guaranteed that
// there is a valid reference when we arrive here (and in the other proxy methods). 

begin
  Assert(Assigned(MainManager));
  MainManager.AnimateWindowProc(TControl(Self), Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PreButtonControlWindowProc(var Message: TMessage);

// Read more about this code in PreAnimateWindowProc.

begin
  Assert(Assigned(MainManager));
  MainManager.ButtonControlWindowProc(TControl(Self), Message);
end;

//----------------------------------------------------------------------------------------------------------------------

{$ifdef CheckListSupport}

  procedure TThemeManager.PreCheckListBoxWindowProc(var Message: TMessage);

  // Read more about this code in PreAnimateWindowProc.

  begin
    Assert(Assigned(MainManager));
    MainManager.CheckListBoxWindowProc(TControl(Self), Message);
  end;

{$endif CheckListSupport}

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PreFormWindowProc(var Message: TMessage);

// Read more about this code in PreAnimateWindowProc.

begin
  Assert(Assigned(MainManager));
  MainManager.FormWindowProc(TControl(Self), Message);
end;

//----------------------------------------------------------------------------------------------------------------------

{$ifdef COMPILER_5_UP}

  procedure TThemeManager.PreFrameWindowProc(var Message: TMessage);

  // Read more about this code in PreAnimateWindowProc.

  begin
    Assert(Assigned(MainManager));
    MainManager.FrameWindowProc(TControl(Self), Message);
  end;

{$endif COMPILER_5_UP}

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PreGroupBoxWindowProc(var Message: TMessage);

// Read more about this code in PreAnimateWindowProc.

begin
  Assert(Assigned(MainManager));
  MainManager.GroupBoxWindowProc(TControl(Self), Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PreListviewWindowProc(var Message: TMessage);

// Read more about this code in PreAnimateWindowProc.

begin
  Assert(Assigned(MainManager));
  MainManager.ListviewWindowProc(TControl(Self), Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PrePanelWindowProc(var Message: TMessage);

// Read more about this code in PreAnimateWindowProc.

begin
  Assert(Assigned(MainManager));
  MainManager.PanelWindowProc(TControl(Self), Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PreSpeedButtonWindowProc(var Message: TMessage);

// Read more about this code in PreAnimateWindowProc.

begin
  Assert(Assigned(MainManager));
  MainManager.SpeedButtonWindowProc(TControl(Self), Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PreSplitterWindowProc(var Message: TMessage);

// Read more about this code in PreAnimateWindowProc.

begin
  Assert(Assigned(MainManager));
  MainManager.SplitterWindowProc(TControl(Self), Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PreStatusBarWindowProc(var Message: TMessage);

// Read more about this code in PreAnimateWindowProc.

begin
  Assert(Assigned(MainManager));
  MainManager.StatusBarWindowProc(TControl(Self), Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PreTabSheetWindowProc(var Message: TMessage);

// Read more about this code in PreAnimateWindowProc.

begin
  Assert(Assigned(MainManager));
  MainManager.TabSheetWindowProc(TControl(Self), Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PreTrackBarWindowProc(var Message: TMessage);

// Read more about this code in PreAnimateWindowProc.

begin
  Assert(Assigned(MainManager));
  MainManager.TrackBarWindowProc(TControl(Self), Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PreWinControlWindowProc(var Message: TMessage);

// Read more about this code in PreAnimateWindowProc.

begin
  Assert(Assigned(MainManager));
  MainManager.WinControlWindowProc(TControl(Self), Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.AddRecreationCandidate(Control: TControl);

begin
  if FPendingRecreationList.IndexOf(Control) = -1 then
    FPendingRecreationList.Add(Control);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.BroadcastThemeChange;

  //--------------- local function --------------------------------------------

  procedure BroadcastChildren(Control: TWinControl);

  var
    I: Integer;
    ChildControl: TWinControl;

  begin
    for I := 0 to Control.ControlCount - 1 do
      if Control.Controls[I] is TWinControl then
      begin
        ChildControl := TWinControl(Control.Controls[I]);
        if ChildControl.HandleAllocated then
          ChildControl.Perform(WM_THEMECHANGED, 0, 0);
        // We must force recreation of some window handles (to reapply all the control settings).
        if (ChildControl is TCustomListView) or (ChildControl is TCoolBar) then
          TWinControlCast(ChildControl).RecreateWnd
        else
          BroadcastChildren(ChildControl);
      end;
  end;

  //--------------- local function --------------------------------------------

var
  I: Integer;
  Form: TCustomForm;

begin 
  for I := 0 to Screen.FormCount - 1 do
  begin
    Form := Screen.Forms[I];
    Form.Perform(WM_THEMECHANGED, 0, 0);
    BroadcastChildren(Form);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TThemeManager.CurrentThemeManager: TThemeManager;

begin
  Result := MainManager;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeManager.DoAllowSubclassing(Control: TControl): Boolean;

begin
  Result := True;
  if Assigned(FOnAllowSubclassing) then
    FOnAllowSubclassing(Self,Control,Result)
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeManager.DoControlMessage(Control: TControl; var Message: TMessage): Boolean;

var
  I: Integer;
  Event: PControlMessageEvent;

begin
  Result := False;
  if Assigned(FOnControlMessage) then
    FOnControlMessage(Self, Control, Message, Result);

  if not Result then
  begin
    I := 0;
    while I < FListeners.Count do
    begin
      Event := FListeners[I];
      try
        Event^(Self, Control, Message, Result);
        if Result then
          Break;
        Inc(I);
      except
        // Raised an exception, so delete the registration
        UnregisterListener(Event^);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.DoOnThemeChange;

begin
  if Assigned(FOnThemeChange) then
    FOnThemeChange(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.DrawBitBtn(Control: TBitBtn; var DrawItemStruct: TDrawItemStruct);

var
  Button: TThemedButton;
  R: TRect;

  Wnd: HWND;
  P: TPoint;

begin
  with DrawItemStruct do
  begin
    // For owner drawn buttons we will never get the ODS_HIGHLIGHT flag. This makes it necessary to
    // check ourselves if the button is "hot".
    GetCursorPos(P);
    Wnd := WindowFromPoint(P);
    if Wnd = TWinControl(Control).Handle then
      itemState := itemState or ODS_HOTLIGHT;

    R := rcItem;

    if not Control.Enabled then
      Button := tbPushButtonDisabled
    else
      if (itemState and ODS_SELECTED) <> 0 then
        Button := tbPushButtonPressed
      else
        if (itemState and ODS_HOTLIGHT) <> 0 then
          Button := tbPushButtonHot
        else
          // It seems ODS_DEFAULT is never set, so we have to check the control's properties.
          if Control.Default or ((itemState and ODS_FOCUS) <> 0) then
            Button := tbPushButtonDefaulted
          else
            Button := tbPushButtonNormal;

    DrawButton(Control, Button, hDC, R, itemState and ODS_FOCUS <> 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.DrawButton(Control: TControl; Button: TThemedButton; DC: HDC; R: TRect; Focused: Boolean);

// Common paint routine for TBitBtn and TSpeedButton.

var
  TextBounds: TRect;
  LastFont: HFONT;
  Glyph: TBitmap;
  GlyphPos: TPoint;
  GlyphWidth: Integer;
  GlyphSourceX: Integer;
  GlyphMask: TBitmap;
  Offset: TPoint;
  ToolButton: TThemedToolBar;
  Details: TThemedElementDetails;

begin
  GlyphSourceX := 0;
  GlyphWidth := 0;
  
  ToolButton := ttbToolbarDontCare;
  if Control is TBitBtn then
  begin
    Glyph := TBitBtn(Control).Glyph;
    // Determine which image to use (if there is more than one in the glyph).
    with TBitBtn(Control), Glyph do
    begin
      if not Empty then
      begin
        GlyphWidth := Width div NumGlyphs;
        if not Enabled and (NumGlyphs > 1) then
          GlyphSourceX := GlyphWidth
        else
          if (Button = tbPushButtonPressed) and (NumGlyphs > 2) then
            GlyphSourceX := 2 * GlyphWidth;
      end;
    end;
  end
  else
  begin
    Glyph := TSpeedButton(Control).Glyph;
    with TSpeedButtonCast(Control) do
    begin
      // Determine which image to use (if there is more than one in the glyph).
      with Glyph do
        if not Empty then
        begin
          GlyphWidth := Width div NumGlyphs;
          if not Enabled and (NumGlyphs > 1) then
            GlyphSourceX := GlyphWidth
          else
            case FState of
              bsDown:
                if NumGlyphs > 2 then
                  GlyphSourceX := 2 * GlyphWidth;
              bsExclusive:
                if NumGlyphs > 3 then
                  GlyphSourceX := 3 * GlyphWidth;
            end;
        end;
      // If the speed button is flat then we use toolbutton images for drawing.
      if Flat then
      begin
        case Button of
          tbPushButtonDisabled:
            Toolbutton := ttbButtonDisabled;
          tbPushButtonPressed:
            Toolbutton := ttbButtonPressed;
          tbPushButtonHot:
            Toolbutton := ttbButtonHot;
          tbPushButtonNormal:
            Toolbutton := ttbButtonNormal;
        end;
      end;
    end;
  end;

  if ToolButton = ttbToolbarDontCare then
  begin
    Details := ThemeServices.GetElementDetails(Button);
    ThemeServices.DrawElement(DC, Details, R);
    R := ThemeServices.ContentRect(DC, Details, R);                         
  end
  else
  begin
    Details := ThemeServices.GetElementDetails(ToolButton);
    ThemeServices.DrawElement(DC, Details, R);
    R := ThemeServices.ContentRect(DC, Details, R);
  end;

  // The XP style does no longer indicate pressed buttons by moving the caption one pixel down and right.
  Offset := Point(0, 0);

  with TControlCast(Control) do
  begin
    LastFont := SelectObject(DC, Font.Handle);
    CalcButtonLayout(Control, DC, R, Offset, GlyphPos, TextBounds, DrawTextBidiModeFlags(0));
    // Note: Currently we cannot do text output via the themes services because the second flags parameter (which is
    // used for graying out strings) is ignored (bug in XP themes implementation?).
    // Hence we have to do it the "usual" way.
    if Button = tbPushButtonDisabled then
      SetTextColor(DC, ColorToRGB(clGrayText))
    else
      SetTextColor(DC, ColorToRGB(Font.Color));
    SetBkMode(DC, TRANSPARENT);
    DrawText(DC, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER);
    with Glyph do
      if not Empty then
      begin
        GlyphMask := TBitmap.Create;
        GlyphMask.Assign(Glyph);
        GlyphMask.Mask(Glyph.TransparentColor);
        TransparentStretchBlt(DC, GlyphPos.X, GlyphPos.Y, GlyphWidth, Height, Canvas.Handle, GlyphSourceX, 0,
          GlyphWidth, Height, GlyphMask.Canvas.Handle, GlyphSourceX, 0);
        GlyphMask.Free;
      end;
    SelectObject(DC, LastFont);
  end;

  if Focused then
  begin
    SetTextColor(DC, 0);
    DrawFocusRect(DC, R);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeManager.FindListener(AControlMessage: TControlMessageEvent; var Index: Integer): Boolean;

var
  I: Integer;

begin
  Result := False;
  for I := 0 to FListeners.Count - 1 do
    if @PControlMessageEvent(FListeners[I])^ = @AControlMessage then
    begin
      Result := True;
      Index := I;
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  // Cast to access the Transparent property in TCustomLabel which is protected there.
  TLabelCast = class(TCustomLabel);

procedure TThemeManager.FixControls(Form: TCustomForm);

// Iterates through all existing controls in all forms which are registered with Screen and checks for TToolBar and
// TCustomLabel. Both controls will get their Transparent property set to True.

var
  MakeTransparent: Boolean;
  RemoveMouseCapture: Boolean;

  //--------------- local function --------------------------------------------

  procedure IterateControls(Parent: TWinControl);

  var
    I, J: Integer;
    ToolBar: TToolBar;
    Control: TControl;
    
  begin
    for I := 0 to Parent.ControlCount - 1 do
    begin
      Control := Parent.Controls[I];
      // Allow all window controls to use themed background if they are placed on a tab sheet. This works only for controls
      // whose background is drawn by Windows and which can be transparent. There aren't many which qualify, though.
      if (Control is TWinControl) and ThemeServices.ThemesEnabled then
        EnableThemeDialogTexture(TWinControl(Control).Handle, ETDT_ENABLETAB);

      if Control is TToolBar then
      begin
        ToolBar := TToolBar(Control);
        if MakeTransparent then
          ToolBar.Transparent := True;
        if RemoveMouseCapture then
        begin
          for J := 0 to ToolBar.ButtonCount - 1 do
            if ToolBar.Buttons[J].Style <> tbsDropDown then
              ToolBar.Buttons[J].ControlStyle := ToolBar.Buttons[J].ControlStyle - [csCaptureMouse];
        end;
      end
      else
        if Control is TCustomLabel then
        begin
          if MakeTransparent then
            TLabelCast(Control).Transparent := True;
        end
        else
          if (Control is TWinControl) and (TWinControl(Control).ControlCount > 0) then
            IterateControls(Control as TWinControl);
    end;
  end;

  //--------------- end local function ----------------------------------------

var
  I: Integer;

begin
  MakeTransparent := toSetTransparency in FOptions;
  RemoveMouseCapture := toResetMouseCapture in FOptions;

  if Form = nil then
  begin
    for I := 0 to Screen.FormCount - 1 do
    begin
      Form := Screen.Forms[I];
      IterateControls(Form);
    end;
  end
  else
    IterateControls(Form);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.ForceAsMainManager;

// Forces this instance to become the main manager. This is useful for descentants to provide additional functionality.

begin
  if MainManager <> Self then
  begin
    Lock.Enter;
    try
      if Assigned(MainManager) then
      begin
        MainManager.FSubclassingDisabled := True;
        MainManager.ClearLists;
      end;

      MainManager := Self;
      FSubclassingDisabled := False;
      CollectForms;
    finally
      Lock.Release;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.HandleControlChange(Control: TControl; Inserting: Boolean);

var
  List: TWindowProcList;
  Index: Integer;
  WinControl: TWinControl;

begin
  List := nil;
  // Do subclassing work only on Windows XP or higher.
  if IsWindowsXP then
  begin
    if not ThemeServices.ThemesEnabled then
    begin
      // TCustomListview always must be subclassed.
      if Control is TCustomListView then
      begin
        if (toSubclassListView in FOptions) or not Inserting then
        begin
          List := FListViewList;
          // We have to force the listview to recreate its window handle (to reapply all the control settings).
          // However if it is already in our list then don't touch the window anymore.
          WinControl := Control as TWinControl;
          if Inserting and not List.Find(Control, Index) and WinControl.HandleAllocated then
            PostMessage(WinControl.Handle, CM_RECREATEWND, 0, 0);
        end;
      end;
    end
    else
    begin
      if Control is TButtonControl then
      begin
        if (toSubclassButtons in FOptions) or not Inserting then
          List := FButtonControlList;
      end
      else
        if Control is TSpeedButton then
        begin
          if (toSubclassSpeedButtons in FOptions) or not Inserting then
            List := FSpeedButtonList;
        end
        else
          if Control is TCustomGroupBox then
          begin
            if (toSubclassGroupBox in FOptions) or not Inserting then
              List := FGroupBoxList;
          end
          else
            if Control is TTabSheet then
            begin
              if (toSubclassTabSheet in FOptions) or not Inserting then
                List := FTabSheetList;
            end
            else
              if Control is TCustomPanel then
              begin
                if (toSubclassPanel in FOptions) or not Inserting then
                  List := FPanelList;
              end
              else
                {$ifdef COMPILER_5_UP}
                  if Control is TCustomFrame then
                  begin
                    if (toSubclassFrame in FOptions) or not Inserting then
                      List := FFrameList;
                  end
                  else
                {$endif COMPILER_5_UP}  
                  if Control is TCustomListView then
                  begin
                    if (toSubclassListView in FOptions) or not Inserting then
                    begin
                      List := FListViewList;
                      // We have to force the listview to recreate its window handle (to reapply all the control settings).
                      // However if it is already in our list then don't touch the window anymore.
                      WinControl := Control as TWinControl;
                      if Inserting and not List.Find(Control, Index) and WinControl.HandleAllocated then
                        PostMessage(WinControl.Handle, CM_RECREATEWND, 0, 0);
                    end;
                  end
                  else
                    if Control is TTrackBar then
                    begin
                      if (toSubclassTrackBar in FOptions) or not Inserting then
                        List := FTrackBarList;
                    end
                    else
                      {$ifdef CheckListSupport}
                        if Control is TCheckListBox then
                        begin
                          if (toSubclassCheckListBox in FOptions) or not Inserting then
                            List := FCheckListBoxList;
                        end
                        else
                      {$endif CheckListSupport}
                        if Control is TCustomStatusBar then
                        begin
                          if (toSubclassStatusBar in FOptions) or not Inserting then
                            List := FStatusBarList;
                        end
                        else
                          if Control is TSplitter then
                          begin
                            if (toSubclassSplitter in FOptions) or not Inserting then
                              List := FSplitterList;
                          end
                          else
                            if Control is TAnimate then
                            begin
                              if (toSubclassAnimate in FOptions) or not Inserting then
                                List := FAnimateList;
                            end
                            else
                              if Control is TCustomForm then
                              begin
                                List := FFormList;
                                if Inserting then
                                  FPendingFormsList.Remove(Control);
                              end
                              else
                                if Control is TWinControl then
                                begin
                                  if (toSubclassWinControl in FOptions) or not Inserting then
                                    List := FWinControlList;
                                end;
    end;

    if Assigned(List) then
    begin
      if Inserting and (DoAllowSubClassing(Control) and (Control.Perform(CM_DENYSUBCLASSING, 0, 0) = 0)) then
      begin
        List.Add(Control);

        // We need a notification for this control about its destruction.
        Control.FreeNotification(Self);

        // Automatically collect the child controls when a TWinControl is added.
        if (Control is TWinControl) and (TWinControl(Control).ControlCount > 0) then
          CollectControls(TWinControl(Control));
      end
      else
        List.Remove(Control);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeManager.IsRecreationCandidate(Control: TControl): Boolean;

// Tells the caller whether the given controls is being recreated.

begin
  Result := FPendingRecreationList.IndexOf(Control) > -1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.Loaded;

begin
  // Collect all controls which already exist. Those controls, which are later added/removed are handled by the
  // subclassing of their old/new parent.
  if (MainManager = Self) and not (csDesigning in ComponentState) then
    CollectForms;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TThemeManager.NeedsBorderPaint(Control: TControl): Boolean;

// Some controls need their frame (non-client area with 3D border) explicitely painted in a themed fashion.
// This method determines, which controls need this.

begin
  Result := (Control is TScrollingWinControl) or (Control is TCustomGrid) or (Control is TCustomRichEdit);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.Notification(AComponent: TComponent; Operation: TOperation);

begin
  if not (csDesigning in ComponentState) then
  begin
    case Operation of
      opInsert:
        // At this place we cannot subclass the control because it did not yet get its initial window procedure.
        // So we add it to an intermediate list and subclass it at a later moment.
        if (AComponent is TCustomForm) and (FPendingFormsList.IndexOf(AComponent) < 0) then
        begin
          if (MainManager = Self) then
          begin
            FPendingFormsList.Add(AComponent);
            // Under some circumstances (e.g. when a MDI child is created) there is no application message, which we
            // need to subclass the form. By posting a dummy message this problem is circumvented.
            PostMessage(Application.Handle, WM_NULL, 0, 0);
          end
          else
            MainManager.Notification(AComponent, Operation);
        end;
      opRemove:
        if (MainManager = Self) and (AComponent is TControl) then
        begin
          if AComponent is TCustomForm then
            // A form is being destroyed. Remove it from the pending forms list if it is still there.
            FPendingFormsList.Remove(AComponent);
          HandleControlChange(AComponent as TControl, False);
        end;
    end;
  end;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.RemoveChildSubclassing(Control: TWinControl);

// Child controls may be released without further notice if their parent control is destroyed.
// One can use the WM_DESTORY message to get notified but if the control haven't even created their window handle
// then also this possibility does not exist anymore.
// Hence when we get notice of a control which is being destroyed then we implicitely remove all subclassed child
// controls from our lists too.

var
  I: Integer;

begin
  for I := 0 to Control.ControlCount - 1 do
    if Control.Controls[I] is TWinControl then
    begin
      RemoveChildSubclassing(TWinControl(Control.Controls[I]));
      HandleControlChange(Control.Controls[I], False);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.RemoveRecreationCandidate(Control: TControl);

begin
  FPendingRecreationList.Remove(Control);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.UpdateThemes;

var
  Flags: Cardinal;

begin
  ThemeServices.UpdateThemes;

  if ThemeServices.ThemesAvailable and not (csDesigning in ComponentState) then
  begin
    Flags := GetThemeAppProperties;
    if (Flags and STAP_ALLOW_NONCLIENT) <> 0 then
      Include(FOptions, toAllowNonClientArea)
    else
      Exclude(FOptions, toAllowNonClientArea);
    if (Flags and STAP_ALLOW_CONTROLS) <> 0 then
      Include(FOptions, toAllowControls)
    else
      Exclude(FOptions, toAllowControls);
    if (Flags and STAP_ALLOW_WEBCONTENT) <> 0 then
      Include(FOptions, toAllowWebContent)
    else
      Exclude(FOptions, toAllowWebContent);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.UpdateUIState(Control: TControl; CharCode: Word);

// Beginning with Windows 2000 the UI in an application may hide focus rectangles and accelerator key indication.
// We have to take care to show them if the user starts navigating using the keyboard.

var
  Form: TCustomForm;

  //--------------- Local functions --------------------------------------------

  procedure InvalidateStaticText(Control: TWinControl);

  var
    I: Integer;

  begin
    if Control is TCustomStaticText then
      Control.Invalidate;
    for I := 0 to Control.ControlCount - 1 do
      if Control.Controls[I] is TWinControl then
        InvalidateStaticText(Control.Controls[I] as TWinControl);
  end;

  //--------------- End local functions ----------------------------------------

begin
  Form := GetParentForm(Control);
  if Assigned(Form) then
    case CharCode of
      VK_LEFT..VK_DOWN,
      VK_TAB:
        Form.Perform(WM_CHANGEUISTATE, MakeLong(UIS_CLEAR, UISF_HIDEFOCUS), 0);
      VK_MENU:
        begin
          Form.Perform(WM_CHANGEUISTATE, MakeLong(UIS_CLEAR, UISF_HIDEACCEL), 0);

          // For no appearent reason does TCustomStaticText not correctly redraw when the accelerator underline
          // is enabled. So we have manually invalide all instances of TCustomStaticText.
          InvalidateStaticText(Form);
        end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.ClearLists;

begin
  // Listview controls must always be subclassed, otherwise they produce trouble on XP with
  // classic themes.
  FListViewList.Clear;
  if ThemeServices.ThemesEnabled then
  begin
    {$ifdef CheckListSupport}
      FCheckListBoxList.Clear;
    {$endif CheckListSupport}
    FStatusBarList.Clear;
    FAnimateList.Clear;
    FTrackBarList.Clear;
    FSpeedButtonList.Clear;
    FButtonControlList.Clear;
    FTabSheetList.Clear;
    FWinControlList.Clear;
    FGroupBoxList.Clear;
    FFormList.Clear;
    FPanelList.Clear;
    {$ifdef COMPILER_5_UP}
      FFrameList.Clear;
    {$endif COMPILER_5_UP}
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.CollectForms(Form: TCustomForm = nil);

// (Re)initiates collecting all controls which need to be subclassed to fixed one or more problems.

var
  I: Integer;

begin
  if not FSubclassingDisabled and not (csDesigning in ComponentState) then
  begin
    if Form = nil then
    begin
      ClearLists;
      for I := 0 to Screen.FormCount - 1 do
      begin
        FFormList.Add(Screen.Forms[I]);
        CollectControls(Screen.Forms[I]);
      end;
    end
    else
    begin
      FFormList.Add(Form);
      CollectControls(Form);
    end;

    if ([toResetMouseCapture, toSetTransparency] * FOptions) <> [] then
      FixControls(Form);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.CollectControls(Parent: TWinControl);

var
  I: Integer;

begin 
  Assert(Assigned(Parent), 'Parent of controls to be collected must be valid.');
  if not FSubclassingDisabled and not (csDesigning in ComponentState) then
  begin
    for I := 0 to Parent.ControlCount - 1 do
    begin
      HandleControlChange(Parent.Controls[I], True);
      if (Parent.Controls[I] is TWinControl) and (TWinControl(Parent.Controls[I]).ControlCount > 0) then
        CollectControls(Parent.Controls[I] as TWinControl);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.PerformEraseBackground(Control: TControl; DC: HDC);

// Repainting the background of a control using theme services relies on the ability of the parent to handle
// WM_PRINT messages. Usually the default behavior of a window is enough to make this possible. However
// double buffered and non-windowed controls are quite different and need so special handling.
// This method uses the WM_ERASEBKGND message to achieve the same effect.

var
  LastOrigin: TPoint;

begin
  GetWindowOrgEx(DC, LastOrigin);
  SetWindowOrgEx(DC, LastOrigin.X + Control.Left, LastOrigin.Y + Control.Top, nil);
  Control.Parent.Perform(WM_ERASEBKGND, Integer(DC), Integer(DC));
  SetWindowOrgEx(DC, LastOrigin.X, LastOrigin.Y, nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.RegisterListener(AControlMessage: TControlMessageEvent);

var
  I: Integer;
  Ptr: PControlMessageEvent;

begin
  if not FindListener(AControlMessage, I) then
  begin
    New(Ptr);
    Ptr^ := AControlMessage;
    FListeners.Add(Ptr);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThemeManager.UnregisterListener(AControlMessage: TControlMessageEvent);

var
  I: Integer;

begin
  if FindListener(AControlMessage, I) then
  begin
    Dispose(PControlMessageEvent(FListeners[I]));
    FListeners.Delete(I);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  Lock := TCriticalSection.Create;
  GetCheckSize;
  IsWindowsXP := (Win32MajorVersion > 5) or ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1));
finalization
  Lock.Free;
  Lock := nil;
end.

