
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntControls;

{$INCLUDE compilers.inc}

{
  Windows NT provides support for native Unicode windows.  To add Unicode support to a
    TWinControl descendant, override CreateWindowHandle() and call CreateUnicodeHandle().

  One major reason this works is because the VCL only uses the ANSI version of
    SendMessage() -- SendMessageA().  If you call SendMessageA() on a UNICODE
    window, Windows deals with the ANSI/UNICODE conversion automatically.  So
    for example, if the VCL sends WM_SETTEXT to a window using SendMessageA,
    Windows actually *expects* a PAnsiChar even if the target window is a UNICODE
    window.  So caling SendMessageA with PChars causes no problems.

  A problem in the VCL has to do with the TControl.Perform() method. Perform()
    calls the window procedure directly and assumes an ANSI window.  This is a
    problem if, for example, the VCL calls Perform(WM_SETTEXT, ...) passing in a
    PAnsiChar which eventually gets passed downto DefWindowProcW() which expects a PWideChar.

  This is the reason for SubClassUnicodeControl().  This procedure will subclass the
    Windows WndProc, and the TWinControl.WindowProc pointer.  It will determine if the
    message came from Windows or if the WindowProc was called directly.  It will then
    call SendMessageA() for Windows to perform proper conversion on certain text messages.

  Another problem has to do with TWinControl.DoKeyPress().  It is called from the WM_CHAR
    message.  It casts the WideChar to an AnsiChar, and sends the resulting character to
    DefWindowProc.  In order to avoid this, the DefWindowProc is subclassed as well.  WindowProc
    will make a WM_CHAR message safe for ANSI handling code by converting the char code to
    #FF before passing it on.  It stores the original WideChar in the .Unused field of TWMChar.
    The code #FF is converted back to the WideChar before passing onto DefWindowProc.
}

{
  Things to consider when designing new controls:
    1)  Check that a WideString Hint property is published.
    2)  If descending from TWinControl, override CreateWindowHandle().
    3)  If not descending from TWinControl, handle CM_HINTSHOW message.
    4)  Check to make sure that CN_CHAR, CN_SYSCHAR and CM_DIALOGCHAR are handled properly.
    5)  If descending from TWinControl, verify Unicode chars are preserved after RecreateWnd.
    6)  Consider using storage specifiers for Hint and Caption properties.
    7)  If any class could possibly have published WideString properties,
          override DefineProperties and call TntPersistent_AfterInherited_DefineProperties.
    8)  Check if TTntThemeManager needs to be updated.
    9)  Override GetActionLinkClass() and ActionChange().
    10) If class updates Application.Hint then update TntApplication.Hint instead.
}

interface

{ TODO: Unicode enable .OnKeyPress event }

uses
  Classes, Windows, Messages, Controls, Menus;


{TNT-WARN TCaption}
type TWideCaption = type WideString;

// caption/text management
function TntControl_IsCaptionStored(Control: TControl): Boolean;
function TntControl_GetStoredText(Control: TControl; const Default: WideString): WideString;
procedure TntControl_SetStoredText(Control: TControl; const Value: WideString);
function TntControl_GetText(Control: TControl): WideString;
procedure TntControl_SetText(Control: TControl; const Text: WideString);

// hint management
function TntControl_IsHintStored(Control: TControl): Boolean;
function TntControl_GetHint(Control: TControl): WideString;
procedure TntControl_SetHint(Control: TControl; const Value: WideString);

function WideGetHint(Control: TControl): WideString;
function WideGetShortHint(const Hint: WideString): WideString;
function WideGetLongHint(const Hint: WideString): WideString;
procedure ProcessCMHintShowMsg(var Message: TMessage);

type
  TTntCustomHintWindow = class(THintWindow)
  private
    FActivating: Boolean;
    FBlockPaint: Boolean;
    function GetCaption: TWideCaption;
    procedure SetCaption(const Value: TWideCaption);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
{$IFNDEF COMPILER_7_UP}
    procedure CreateParams(var Params: TCreateParams); override;
{$ENDIF}
    procedure Paint; override;
  public
    procedure ActivateHint(Rect: TRect; const AHint: AnsiString); override;
    procedure ActivateHintData(Rect: TRect; const AHint: AnsiString; AData: Pointer); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: AnsiString; AData: Pointer): TRect; override;
    property Caption: TWideCaption read GetCaption write SetCaption;
  end;

  TTntHintWindow = class(TTntCustomHintWindow)
  public
    procedure ActivateHint(Rect: TRect; const AHint: WideString); reintroduce;
    procedure ActivateHintData(Rect: TRect; const AHint: WideString; AData: Pointer); reintroduce;
    function CalcHintRect(MaxWidth: Integer; const AHint: WideString; AData: Pointer): TRect; reintroduce;
  end;

// text/char message
function IsTextMessage(Msg: UINT): Boolean;
procedure MakeWMCharMsgSafeForAnsi(var Message: TMessage);
procedure RestoreWMCharMsg(var Message: TMessage);
function GetWideCharFromWMCharMsg(Message: TWMChar): WideChar;
procedure SetWideCharForWMCharMsg(var Message: TWMChar; Ch: WideChar);

// register/create window
procedure SubClassUnicodeControl(Control: TWinControl; Params_Caption: PAnsiChar; IDEWindow: Boolean = False);
procedure RegisterUnicodeClass(Params: TCreateParams; out WideWinClassName: WideString; IDEWindow: Boolean = False);
procedure CreateUnicodeHandle(Control: TWinControl; const Params: TCreateParams;
                                        const SubClass: WideString; IDEWindow: Boolean = False);
procedure ReCreateUnicodeWnd(Control: TWinControl; Subclass: WideString; IDEWindow: Boolean = False);

type
  IWideCustomListControl = interface
    ['{C1801F41-51E9-4DB5-8DB8-58AC86698C2E}']
    procedure AddItem(const Item: WideString; AObject: TObject);
  end;

procedure WideListControl_AddItem(Control: TCustomListControl; const Item: WideString; AObject: TObject);

var
  _IsShellProgramming: Boolean = False;

var
  TNT_WM_DESTROY: Cardinal;

implementation

uses
  ActnList, Forms, SysUtils, Contnrs, 
  TntGraphics, TntWindows, TntClasses, TntMenus, TntSysUtils;

type
  TAccessControl = class(TControl);
  TAccessWinControl = class(TWinControl);
  TAccessControlActionLink = class(TControlActionLink{TNT-ALLOW TControlActionLink});

//----------------------------------------------- WIDE CAPTION HOLDERS --------

{ TWideControlHelper }

var
  WideControlHelpers: TComponentList = nil;

type
  TWideControlHelper = class(TWideComponentHelper)
  private
    FControl: TControl;
    FWideCaption: WideString;
    FWideHint: WideString;
    procedure SetAnsiText(const Value: AnsiString);
    procedure SetAnsiHint(const Value: AnsiString);
  public
    constructor Create(AOwner: TControl); reintroduce;
    property WideCaption: WideString read FWideCaption;
    property WideHint: WideString read FWideHint;
  end;

constructor TWideControlHelper.Create(AOwner: TControl);
begin
  inherited CreateHelper(AOwner, WideControlHelpers);
  FControl := AOwner;
end;

procedure TWideControlHelper.SetAnsiText(const Value: AnsiString);
begin
  TAccessControl(FControl).Text := Value;
end;

procedure TWideControlHelper.SetAnsiHint(const Value: AnsiString);
begin
  FControl.Hint := Value;
end;

function FindWideControlHelper(Control: TControl; CreateIfNotFound: Boolean = True): TWideControlHelper;
begin
  Result := TWideControlHelper(FindWideComponentHelper(WideControlHelpers, Control));
  if (Result = nil) and CreateIfNotFound then
  	Result := TWideControlHelper.Create(Control);
end;

//----------------------------------------------- GET/SET WINDOW CAPTION/HINT -------------

function TntControl_IsCaptionStored(Control: TControl): Boolean;
begin
  with TAccessControl(Control) do
    Result := (ActionLink = nil) or not TAccessControlActionLink(ActionLink).IsCaptionLinked;
end;

function TntControl_GetStoredText(Control: TControl; const Default: WideString): WideString;
var
  WideControlHelper: TWideControlHelper;
begin
  WideControlHelper := FindWideControlHelper(Control, False);
  if WideControlHelper <> nil then
    Result := WideControlHelper.WideCaption
  else
    Result := Default;
end;

procedure TntControl_SetStoredText(Control: TControl; const Value: WideString);
begin
  FindWideControlHelper(Control).FWideCaption := Value;
  TAccessControl(Control).Text := Value;
end;

function TntControl_GetText(Control: TControl): WideString;
var
  WideControlHelper: TWideControlHelper;
begin
  if (not Win32PlatformIsUnicode)
  or ((Control is TWinControl) and TWinControl(Control).HandleAllocated and (not IsWindowUnicode(TWinControl(Control).Handle))) then
    // Win9x / non-unicode handle
    Result := TAccessControl(Control).Text
  else if (not (Control is TWinControl)) then begin
    // non-windowed TControl
    WideControlHelper := FindWideControlHelper(Control, False);
    if WideControlHelper = nil then
      Result := TAccessControl(Control).Text
    else
      Result := GetSyncedWideString(WideControlHelper.FWideCaption, TAccessControl(Control).Text);
  end else if (not TWinControl(Control).HandleAllocated) then begin
    // NO HANDLE
    Result := TntControl_GetStoredText(Control, TAccessControl(Control).Text)
  end else begin
    // UNICODE & HANDLE
    SetLength(Result, GetWindowTextLengthW(TWinControl(Control).Handle) + 1);
    GetWindowTextW(TWinControl(Control).Handle, PWideChar(Result), Length(Result));
    SetLength(Result, Length(Result) - 1);
  end;
end;

procedure TntControl_SetText(Control: TControl; const Text: WideString);
begin
  if (not Win32PlatformIsUnicode)
  or ((Control is TWinControl) and TWinControl(Control).HandleAllocated and (not IsWindowUnicode(TWinControl(Control).Handle))) then
    // Win9x / non-unicode handle
    TAccessControl(Control).Text := Text
  else if (not (Control is TWinControl)) then begin
    // non-windowed TControl
    with FindWideControlHelper(Control) do
      SetSyncedWideString(Text, FWideCaption, TAccessControl(Control).Text, SetAnsiText)
  end else if (not TWinControl(Control).HandleAllocated) then begin
    // NO HANDLE
    TntControl_SetStoredText(Control, Text);
  end else if TntControl_GetText(Control) <> Text then begin
    // UNICODE & HANDLE
    Tnt_SetWindowTextW(TWinControl(Control).Handle, PWideChar(Text));
    Control.Perform(CM_TEXTCHANGED, 0, 0);
  end;
end;

// hint management -----------------------------------------------------------------------

function TntControl_IsHintStored(Control: TControl): Boolean;
begin
  with TAccessControl(Control) do
    Result := (ActionLink = nil) or not TAccessControlActionLink(ActionLink).IsHintLinked;
end;

function TntControl_GetHint(Control: TControl): WideString;
var
  WideControlHelper: TWideControlHelper;
begin
  if (not Win32PlatformIsUnicode) then
    Result := Control.Hint
  else begin
    WideControlHelper := FindWideControlHelper(Control, False);
    if WideControlHelper <> nil then
      Result := GetSyncedWideString(WideControlHelper.FWideHint, Control.Hint)
    else
      Result := Control.Hint;
  end;
end;

procedure TntControl_SetHint(Control: TControl; const Value: WideString);
begin
  if (not Win32PlatformIsUnicode) then
    Control.Hint := Value
  else
    with FindWideControlHelper(Control) do
      SetSyncedWideString(Value, FWideHint, Control.Hint, SetAnsiHint);
end;

function WideGetHint(Control: TControl): WideString;
begin
  while Control <> nil do
    if TntControl_GetHint(Control) = '' then
      Control := Control.Parent
    else
    begin
      Result := TntControl_GetHint(Control);
      Exit;
    end;
  Result := '';
end;

function WideGetShortHint(const Hint: WideString): WideString;
var
  I: Integer;
begin
  I := Pos('|', Hint);
  if I = 0 then
    Result := Hint else
    Result := Copy(Hint, 1, I - 1);
end;

function WideGetLongHint(const Hint: WideString): WideString;
var
  I: Integer;
begin
  I := Pos('|', Hint);
  if I = 0 then
    Result := Hint else
    Result := Copy(Hint, I + 1, Maxint);
end;

//----------------------------------------------------------------------------------------

var UnicodeCreationControl: TWinControl = nil;

function IsUnicodeCreationControl(Handle: HWND): Boolean;
begin
  Result := (UnicodeCreationControl <> nil)
        and (UnicodeCreationControl.HandleAllocated)
        and (UnicodeCreationControl.Handle = Handle);
end;

function WMNotifyFormatResult(FromHandle: HWND): Integer;
begin
  if Win32PlatformIsUnicode
  and (IsWindowUnicode(FromHandle) or IsUnicodeCreationControl(FromHandle)) then
    Result := NFR_UNICODE
  else
    Result := NFR_ANSI;
end;

function IsTextMessage(Msg: UINT): Boolean;
begin
  // WM_CHAR is omitted because of the special handling it receives
  Result := (Msg = WM_SETTEXT)
         or (Msg = WM_GETTEXT)
         or (Msg = WM_GETTEXTLENGTH);
end;

const
  ANSI_UNICODE_HOLDER = $FF;

procedure MakeWMCharMsgSafeForAnsi(var Message: TMessage);
begin
  with TWMChar(Message) do begin
    Assert(Msg = WM_CHAR);
    if not _IsShellProgramming then
      Assert(Unused = 0)
    else begin
      Assert((Unused = 0) or (CharCode <= Word(High(AnsiChar))));
      // When a Unicode control is embedded under non-Delphi Unicode
      //   window something strange happens
      if (Unused <> 0) then begin
        CharCode := (Unused shl 8) or CharCode;
      end;
    end;
    if (CharCode > Word(High(AnsiChar))) then begin
      Unused := CharCode;
      CharCode := ANSI_UNICODE_HOLDER;
    end;
  end;
end;

procedure RestoreWMCharMsg(var Message: TMessage);
begin
  with TWMChar(Message) do begin
    Assert(Message.Msg = WM_CHAR);
    if (Unused > 0)
    and (CharCode = ANSI_UNICODE_HOLDER) then
      CharCode := Unused;
    Unused := 0;
  end;
end;

function GetWideCharFromWMCharMsg(Message: TWMChar): WideChar;
begin
  if (Message.CharCode = ANSI_UNICODE_HOLDER)
  and (Message.Unused <> 0) then
    Result := WideChar(Message.Unused)
  else
    Result := WideChar(Message.CharCode);
end;

procedure SetWideCharForWMCharMsg(var Message: TWMChar; Ch: WideChar);
begin
  Message.CharCode := Word(Ch);
  Message.Unused := 0;
  MakeWMCharMsgSafeForAnsi(TMessage(Message));
end;

//-----------------------------------------------------------------------------------
type
  TWinControlTrap = class(TComponent)
  private
    WinControl_ObjectInstance: Pointer;
    ObjectInstance: Pointer;
    DefObjectInstance: Pointer;
    function IsInSubclassChain(Control: TWinControl): Boolean;
    procedure SubClassWindowProc;
  private
    FControl: TAccessWinControl;
    Handle: THandle;
    PrevWin32Proc: Pointer;
    PrevDefWin32Proc: Pointer;
    PrevWindowProc: TWndMethod;
  private
    LastWin32Msg: UINT;
    Win32ProcLevel: Integer;
    IDEWindow: Boolean;
    DestroyTrap: Boolean;
    TestForNull: Boolean;
    FoundNull: Boolean;
    {$IFDEF TNT_VERIFY_WINDOWPROC}
    LastVerifiedWindowProc: TWndMethod;
    {$ENDIF}
    procedure Win32Proc(var Message: TMessage);
    procedure DefWin32Proc(var Message: TMessage);
    procedure WindowProc(var Message: TMessage);
  private
    procedure SubClassControl(Params_Caption: PAnsiChar);
    procedure UnSubClassUnicodeControl;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TWinControlTrap.Create(AOwner: TComponent);
begin
  FControl := TAccessWinControl(AOwner as TWinControl);
  inherited Create(nil);
  FControl.FreeNotification(Self);

  WinControl_ObjectInstance := Classes.MakeObjectInstance(FControl.MainWndProc);
  ObjectInstance := Classes.MakeObjectInstance(Win32Proc);
  DefObjectInstance := Classes.MakeObjectInstance(DefWin32Proc);
end;

destructor TWinControlTrap.Destroy;
begin
  Classes.FreeObjectInstance(ObjectInstance);
  Classes.FreeObjectInstance(DefObjectInstance);
  Classes.FreeObjectInstance(WinControl_ObjectInstance);
  inherited;
end;

procedure TWinControlTrap.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FControl) and (Operation = opRemove) then begin
    FControl := nil;
    if Win32ProcLevel = 0 then
      Free
    else
      DestroyTrap := True;
  end;
end;

procedure TWinControlTrap.SubClassWindowProc;
begin
  if not IsInSubclassChain(FControl) then begin
    PrevWindowProc := FControl.WindowProc;
    FControl.WindowProc := Self.WindowProc;
  end;
  {$IFDEF TNT_VERIFY_WINDOWPROC}
  LastVerifiedWindowProc := FControl.WindowProc;
  {$ENDIF}
end;

procedure TWinControlTrap.SubClassControl(Params_Caption: PAnsiChar);
begin
  // initialize trap object
  Handle := FControl.Handle;
  PrevWin32Proc := Pointer(GetWindowLongW(FControl.Handle, GWL_WNDPROC));
  PrevDefWin32Proc := FControl.DefWndProc;

  // subclass Window Procedures
  SetWindowLongW(FControl.Handle, GWL_WNDPROC, Integer(ObjectInstance));
  FControl.DefWndProc := DefObjectInstance;
  SubClassWindowProc;

  // For some reason, caption gets garbled after calling SetWindowLongW(.., GWL_WNDPROC).
  TntControl_SetText(FControl, TntControl_GetStoredText(FControl, Params_Caption));
end;

function SameWndMethod(A, B: TWndMethod): Boolean;
begin
  Result := @A = @B;
end;

var
  PendingRecreateWndTrapList: TComponentList = nil;

procedure TWinControlTrap.UnSubClassUnicodeControl;
begin
  // remember caption for future window creation
  if not (csDestroying in FControl.ComponentState) then
    TntControl_SetStoredText(FControl, TntControl_GetText(FControl));

  // restore window procs (restore WindowProc only if we are still the direct subclass)
  if SameWndMethod(FControl.WindowProc, Self.WindowProc) then
    FControl.WindowProc := PrevWindowProc;
  TAccessWinControl(FControl).DefWndProc := PrevDefWin32Proc;
  SetWindowLongW(FControl.Handle, GWL_WNDPROC, Integer(PrevWin32Proc));

  if IDEWindow then
    DestroyTrap := True
  else if not (csDestroying in FControl.ComponentState) then
    // control not being destroyed, probably recreating window
    PendingRecreateWndTrapList.Add(Self);
end;

var
  Finalized: Boolean; { If any tnt controls are still around after finalization it must be due to a memory leak.
                        Windows will still try to send a WM_DESTROY, but we will just ignore it if we're finalized. }

procedure TWinControlTrap.Win32Proc(var Message: TMessage);
begin
  if (not Finalized) then begin
    Inc(Win32ProcLevel);
    try
      with Message do begin
        {$IFDEF TNT_VERIFY_WINDOWPROC}
        if not SameWndMethod(FControl.WindowProc, LastVerifiedWindowProc) then begin
          SubClassWindowProc;
          LastVerifiedWindowProc := FControl.WindowProc;
        end;
        {$ENDIF}
        LastWin32Msg := Msg;
        Result := CallWindowProcW(PrevWin32Proc, Handle, Msg, wParam, lParam);
      end;
    finally
      Dec(Win32ProcLevel);
    end;
    if (Win32ProcLevel = 0) and (DestroyTrap) then
      Free;
  end else if (Message.Msg = WM_DESTROY) or (Message.Msg = TNT_WM_DESTROY) then
    FControl.WindowHandle := 0
end;

procedure TWinControlTrap.DefWin32Proc(var Message: TMessage);

  function IsChildEdit(AHandle: HWND): Boolean;
  var
    AHandleClass: WideString;
  begin
    Result := False;
    if (FControl.Handle = GetParent(Handle)) then begin
      // child control
      SetLength(AHandleClass, 255);
      SetLength(AHandleClass, GetClassNameW(AHandle, PWideChar(AHandleClass), Length(AHandleClass)));
      Result := WideSameText(AHandleClass, 'EDIT');
    end;
  end;

begin
  with Message do begin
    if Msg = WM_NOTIFYFORMAT then
      Result := WMNotifyFormatResult(HWND(Message.wParam))
    else begin
      if (Msg = WM_CHAR) then begin
        RestoreWMCharMsg(Message)
      end;
      if (Msg = WM_IME_CHAR) and (not _IsShellProgramming) and (not Win32PlatformIsXP) then
      begin
        { In Windows XP, DefWindowProc handles WM_IME_CHAR fine for VCL windows. }
        { Before XP, DefWindowProc will sometimes produce incorrect, non-Unicode WM_CHAR. }
        { Also, using PostMessageW on Windows 2000 didn't always produce the correct results. }
        Message.Result := SendMessageW(Handle, WM_CHAR, wParam, lParam)
      end else if (Msg = WM_IME_CHAR) and (_IsShellProgramming) then begin
        { When a Tnt control is hosted by a non-delphi control, DefWindowProc doesn't always work even on XP. }
        if IsChildEdit(Handle) then
          Message.Result := Integer(PostMessageW(Handle, WM_CHAR, wParam, lParam)) // native edit child control
        else
          Message.Result := SendMessageW(Handle, WM_CHAR, wParam, lParam);
      end else begin
        if (Msg = WM_DESTROY) then begin
          UnSubClassUnicodeControl; {The reason for doing this in DefWin32Proc is because in D9, TWinControl.WMDestroy() does a perform(WM_TEXT) operation. }
        end;
        { Normal DefWindowProc }
        Result := CallWindowProcW(PrevDefWin32Proc, Handle, Msg, wParam, lParam);
      end;
    end;
  end;
end;

procedure ProcessCMHintShowMsg(var Message: TMessage);
begin
  if Win32PlatformIsUnicode then begin
    with TCMHintShow(Message) do begin
      if (HintInfo.HintWindowClass = THintWindow)
      or (HintInfo.HintWindowClass.InheritsFrom(TTntCustomHintWindow)) then begin
        if (HintInfo.HintWindowClass = THintWindow) then
          HintInfo.HintWindowClass := TTntCustomHintWindow;
        HintInfo.HintData := HintInfo;
        HintInfo.HintStr := WideGetShortHint(WideGetHint(HintInfo.HintControl));
      end;
    end;
  end;
end;

function TWinControlTrap.IsInSubclassChain(Control: TWinControl): Boolean;
var
  Message: TMessage;
begin
  if SameWndMethod(Control.WindowProc, TAccessWinControl(Control).WndProc) then
    Result := False { no subclassing }
  else if SameWndMethod(Control.WindowProc, Self.WindowProc) then
    Result := True { directly subclassed }
  else begin
    TestForNull := True;
    FoundNull := False;
    ZeroMemory(@Message, SizeOf(Message));
    Message.Msg := WM_NULL;
    Control.WindowProc(Message);
    Result := FoundNull; { indirectly subclassed }
  end;
end;

procedure TWinControlTrap.WindowProc(var Message: TMessage);
var
  CameFromWindows: Boolean;
begin
  if TestForNull and (Message.Msg = WM_NULL) then
    FoundNull := True;

  if (not FControl.HandleAllocated) then
    FControl.WndProc(Message)
  else begin
    CameFromWindows := LastWin32Msg <> WM_NULL;
    LastWin32Msg := WM_NULL;
    with Message do begin
      if Msg = CM_HINTSHOW then
        ProcessCMHintShowMsg(Message);
      if (not CameFromWindows)
      and (IsTextMessage(Msg)) then
        Result := SendMessageA(Handle, Msg, wParam, lParam)
      else begin
        if (Msg = WM_CHAR) then begin
          MakeWMCharMsgSafeForAnsi(Message);
        end;
        PrevWindowProc(Message)
      end;
      if (Msg = TNT_WM_DESTROY) then 
        UnSubClassUnicodeControl; {The reason for doing this in DefWin32Proc is because in D9, TWinControl.WMDestroy() does a perform(WM_TEXT) operation. }
    end;
  end;
end;

//----------------------------------------------------------------------------------

function FindOrCreateWinControlTrap(Control: TWinControl): TWinControlTrap;
var
  i: integer;
begin
  // find or create trap object
  Result := nil;
  for i := PendingRecreateWndTrapList.Count - 1 downto 0 do begin
    if TWinControlTrap(PendingRecreateWndTrapList[i]).FControl = Control then begin
      Result := TWinControlTrap(PendingRecreateWndTrapList[i]);
      PendingRecreateWndTrapList.Delete(i);
      break; { found it }
    end;
  end;
  if Result = nil then
    Result := TWinControlTrap.Create(Control);
end;

procedure SubClassUnicodeControl(Control: TWinControl; Params_Caption: PAnsiChar; IDEWindow: Boolean = False);
var
  WinControlTrap: TWinControlTrap;
begin
  if not IsWindowUnicode(Control.Handle) then
    raise ETntInternalError.Create('Internal Error: SubClassUnicodeControl.Control is not Unicode.');

  WinControlTrap := FindOrCreateWinControlTrap(Control);
  WinControlTrap.SubClassControl(Params_Caption);
  WinControlTrap.IDEWindow := IDEWindow;
end;


//----------------------------------------------- CREATE/DESTROY UNICODE HANDLE

var
  WindowAtom: TAtom;
  ControlAtom: TAtom;
  WindowAtomString: AnsiString;
  ControlAtomString: AnsiString;

type
  TWndProc = function(HWindow: HWnd; Message, WParam, LParam: Longint): Longint; stdcall;

function InitWndProcW(HWindow: HWnd; Message, WParam, LParam: Longint): Longint; stdcall;

    function GetObjectInstance(Control: TWinControl): Pointer;
    var
      WinControlTrap: TWinControlTrap;
    begin
      WinControlTrap := FindOrCreateWinControlTrap(Control);
      PendingRecreateWndTrapList.Add(WinControlTrap);
      Result := WinControlTrap.WinControl_ObjectInstance;
    end;

var
  ObjectInstance: Pointer;
begin
  TAccessWinControl(CreationControl).WindowHandle := HWindow;
  ObjectInstance := GetObjectInstance(CreationControl);
  {Controls.InitWndProc converts control to ANSI here by calling SetWindowLongA()!}
  SetWindowLongW(HWindow, GWL_WNDPROC, Integer(ObjectInstance));
  if  (GetWindowLongW(HWindow, GWL_STYLE) and WS_CHILD <> 0)
  and (GetWindowLongW(HWindow, GWL_ID) = 0) then
    SetWindowLongW(HWindow, GWL_ID, Integer(HWindow));
  SetProp(HWindow, MakeIntAtom(ControlAtom), THandle(CreationControl));
  SetProp(HWindow, MakeIntAtom(WindowAtom), THandle(CreationControl));
  CreationControl := nil;
  Result := TWndProc(ObjectInstance)(HWindow, Message, WParam, lParam);
end;

procedure RegisterUnicodeClass(Params: TCreateParams; out WideWinClassName: WideString; IDEWindow: Boolean = False);
const
  UNICODE_CLASS_EXT = '.UnicodeClass';
var
  TempClass: TWndClassW;
  WideClass: TWndClassW;
  ClassRegistered: Boolean;
  InitialProc: TFNWndProc;
begin
  if IDEWindow then
    InitialProc := @InitWndProc
  else
    InitialProc := @InitWndProcW;

  with Params do begin
    WideWinClassName := WinClassName + UNICODE_CLASS_EXT;
    ClassRegistered := GetClassInfoW(hInstance, PWideChar(WideWinClassName), TempClass);
    if (not ClassRegistered) or (TempClass.lpfnWndProc <> InitialProc)
    then begin
      if ClassRegistered then Win32Check(Windows.UnregisterClassW(PWideChar(WideWinClassName), hInstance));
      // Prepare a TWndClassW record
      WideClass := TWndClassW(WindowClass);
      WideClass.hInstance := hInstance;
      WideClass.lpfnWndProc := InitialProc;
      if not Tnt_Is_IntResource(PWideChar(WindowClass.lpszMenuName)) then begin
        WideClass.lpszMenuName := PWideChar(WideString(WindowClass.lpszMenuName));
      end;
      WideClass.lpszClassName := PWideChar(WideWinClassName);

      // Register the UNICODE class
      if RegisterClassW(WideClass) = 0 then RaiseLastOSError;
    end;
  end;
end;

procedure CreateUnicodeHandle(Control: TWinControl; const Params: TCreateParams;
                                        const SubClass: WideString; IDEWindow: Boolean = False);
var
  TempSubClass: TWndClassW;
  WideWinClassName: WideString;
  Handle: THandle;
begin
  if (not Win32PlatformIsUnicode) then begin
    with Params do
      TAccessWinControl(Control).WindowHandle := CreateWindowEx(ExStyle, WinClassName,
        Caption, Style, X, Y, Width, Height, WndParent, 0, WindowClass.hInstance, Param);
  end else begin
    // SubClass the unicode version of this control by getting the correct DefWndProc
    if (SubClass <> '')
    and GetClassInfoW(Params.WindowClass.hInstance, PWideChar(SubClass), TempSubClass) then
      TAccessWinControl(Control).DefWndProc := TempSubClass.lpfnWndProc
    else
      TAccessWinControl(Control).DefWndProc := @DefWindowProcW;

    // make sure Unicode window class is registered
    RegisterUnicodeClass(Params, WideWinClassName, IDEWindow);

    // Create UNICODE window handle
    UnicodeCreationControl := Control;
    try
      with Params do
        Handle := CreateWindowExW(ExStyle, PWideChar(WideWinClassName), nil,
          Style, X, Y, Width, Height, WndParent, 0, hInstance, Param);
      if Handle = 0 then
        RaiseLastOSError;
      TAccessWinControl(Control).WindowHandle := Handle;
      if IDEWindow then
        SetWindowLongW(Handle, GWL_WNDPROC, GetWindowLong(Handle, GWL_WNDPROC));
    finally
      UnicodeCreationControl := nil;
    end;

    SubClassUnicodeControl(Control, Params.Caption, IDEWindow);
  end;
end;

procedure ReCreateUnicodeWnd(Control: TWinControl; Subclass: WideString; IDEWindow: Boolean = False);
var
  WasFocused: Boolean;
  Params: TCreateParams;
begin
  with TAccessWinControl(Control) do begin
    WasFocused := Focused;
    DestroyHandle;
    CreateParams(Params);
    CreationControl := Control;
    CreateUnicodeHandle(Control, Params, SubClass, IDEWindow);
    StrDispose{TNT-ALLOW StrDispose}(WindowText);
    WindowText := nil;
    Perform(WM_SETFONT, Integer(Font.Handle), 1);
    if AutoSize then AdjustSize;
    UpdateControlState;
    if WasFocused and (WindowHandle <> 0) then Windows.SetFocus(WindowHandle);
  end;
end;

{ TTntCustomHintWindow procs }

function DataPointsToHintInfoForTnt(AData: Pointer): Boolean;
begin
  try
    Result := (AData <> nil)
          and (PHintInfo(AData).HintData = AData) {points to self}
          and (PHintInfo(AData).HintWindowClass.InheritsFrom(TTntCustomHintWindow));
  except
    Result := False;
  end;
end;

function ExtractTntHintCaption(AData: Pointer): WideString;
var
  Control: TControl;
  WideHint: WideString;
  AnsiHintWithShortCut: AnsiString;
  ShortCut: TShortCut;
begin
  Result := PHintInfo(AData).HintStr;
  if Result <> '' then begin
    Control := PHintInfo(AData).HintControl;
    WideHint := WideGetShortHint(WideGetHint(Control));
    if (AnsiString(WideHint) = PHintInfo(AData).HintStr) then
      Result := WideHint
    else if Application.HintShortCuts and (Control <> nil)
    and (Control.Action is TCustomAction{TNT-ALLOW TCustomAction}) then begin
      ShortCut := TCustomAction{TNT-ALLOW TCustomAction}(Control.Action).ShortCut;
      if (ShortCut <> scNone) then
      begin
        AnsiHintWithShortCut := Format{TNT-ALLOW Format}('%s (%s)', [WideHint, ShortCutToText{TNT-ALLOW ShortCutToText}(ShortCut)]);
        if AnsiHintWithShortCut = PHintInfo(AData).HintStr then
          Result := WideFormat('%s (%s)', [WideHint, WideShortCutToText(ShortCut)]);
      end;
    end;
  end;
end;

{ TTntCustomHintWindow }

procedure TTntCustomHintWindow.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, '');
end;

{$IFNDEF COMPILER_7_UP}
procedure TTntCustomHintWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited;
  if Win32PlatformIsXP then { Enable drop shadow effect on Windows XP and later. }
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;
{$ENDIF}

function TTntCustomHintWindow.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self)
end;

procedure TTntCustomHintWindow.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

procedure TTntCustomHintWindow.Paint;
var
  R: TRect;
begin
  if FBlockPaint then
    exit;
  if (not Win32PlatformIsUnicode) then
    inherited
  else begin
    R := ClientRect;
    Inc(R.Left, 2);
    Inc(R.Top, 2);
    Canvas.Font.Color := Screen.HintFont.Color;
    Tnt_DrawTextW(Canvas.Handle, PWideChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX or
      DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
  end;
end;

procedure TTntCustomHintWindow.CMTextChanged(var Message: TMessage);
begin
  { Avoid flicker when calling ActivateHint }
  if FActivating then Exit;
  Width := WideCanvasTextWidth(Canvas, Caption) + 6;
  Height := WideCanvasTextHeight(Canvas, Caption) + 6;
end;

procedure TTntCustomHintWindow.ActivateHint(Rect: TRect; const AHint: AnsiString);
var
  SaveActivating: Boolean;
begin
  SaveActivating := FActivating;
  try
    FActivating := True;
    inherited;
  finally
    FActivating := SaveActivating;
  end;
end;

procedure TTntCustomHintWindow.ActivateHintData(Rect: TRect; const AHint: AnsiString; AData: Pointer);
var
  SaveActivating: Boolean;
begin
  if (not Win32PlatformIsUnicode)
  or (not DataPointsToHintInfoForTnt(AData)) then
    inherited
  else begin
    FBlockPaint := True;
    try
      SaveActivating := FActivating;
      try
        FActivating := True;
        inherited;
        Caption := ExtractTntHintCaption(AData);
      finally
        FActivating := SaveActivating;
      end;
    finally
      FBlockPaint := False;
    end;
    Invalidate;
  end;
end;

function TntHintWindow_CalcHintRect(HintWindow: TTntCustomHintWindow; MaxWidth: Integer; const AHint: WideString): TRect;
begin
  Result := Rect(0, 0, MaxWidth, 0);
  Tnt_DrawTextW(HintWindow.Canvas.Handle, PWideChar(AHint), -1, Result, DT_CALCRECT or DT_LEFT or
    DT_WORDBREAK or DT_NOPREFIX or HintWindow.DrawTextBiDiModeFlagsReadingOnly);
  Inc(Result.Right, 6);
  Inc(Result.Bottom, 2);
end;

function TTntCustomHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: AnsiString; AData: Pointer): TRect;
var
  WideHintStr: WideString;
begin
  if (not Win32PlatformIsUnicode)
  or (not DataPointsToHintInfoForTnt(AData)) then
    Result := inherited CalcHintRect(MaxWidth, AHint, AData)
  else begin
    WideHintStr := ExtractTntHintCaption(AData);
    Result := TntHintWindow_CalcHintRect(Self, MaxWidth, WideHintStr);
  end;
end;

{ TTntHintWindow }

procedure TTntHintWindow.ActivateHint(Rect: TRect; const AHint: WideString);
var
  SaveActivating: Boolean;
begin
  SaveActivating := FActivating;
  try
    FActivating := True;
    Caption := AHint;
    inherited ActivateHint(Rect, AHint);
  finally
    FActivating := SaveActivating;
  end;
end;

procedure TTntHintWindow.ActivateHintData(Rect: TRect; const AHint: WideString; AData: Pointer);
var
  SaveActivating: Boolean;
begin
  FBlockPaint := True;
  try
    SaveActivating := FActivating;
    try
      FActivating := True;
      Caption := AHint;
      inherited ActivateHintData(Rect, AHint, AData);
    finally
      FActivating := SaveActivating;
    end;
  finally
    FBlockPaint := False;
  end;
  Invalidate;
end;

function TTntHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: WideString; AData: Pointer): TRect;
begin
  Result := TntHintWindow_CalcHintRect(Self, MaxWidth, AHint);
end;

procedure WideListControl_AddItem(Control: TCustomListControl; const Item: WideString; AObject: TObject);
var
  WideControl: IWideCustomListControl;
begin
  if Control.GetInterface(IWideCustomListControl, WideControl) then
    WideControl.AddItem(Item, AObject)
  else
    Control.AddItem(Item, AObject);
end;

procedure InitControls;

  procedure InitAtomStrings_D6_D7_D9;
  var
    Controls_HInstance: Cardinal;
  begin
    Controls_HInstance := FindClassHInstance(TWinControl);
    WindowAtomString := Format{TNT-ALLOW Format}('Delphi%.8X',[GetCurrentProcessID]);
    ControlAtomString := Format{TNT-ALLOW Format}('ControlOfs%.8X%.8X', [Controls_HInstance, GetCurrentThreadID]);
  end;

  {$IFDEF COMPILER_6} // verified against VCL source in Delphi 6 and BCB 6
  procedure InitAtomStrings;
  begin
    InitAtomStrings_D6_D7_D9;
  end;
  {$ENDIF}
  {$IFDEF DELPHI_7} // verified against VCL source in Delphi 7
  procedure InitAtomStrings;
  begin
    InitAtomStrings_D6_D7_D9;
  end;
  {$ENDIF}
  {$IFDEF DELPHI_9} // verified against VCL source in Delphi 9
  procedure InitAtomStrings;
  begin
    InitAtomStrings_D6_D7_D9;
  end;
  {$ENDIF}
  {$IFDEF DELPHI_10} // verified against VCL source in Delphi 10
  procedure InitAtomStrings;
  begin
    InitAtomStrings_D6_D7_D9;
  end;
  {$ENDIF}

begin
  InitAtomStrings;
  WindowAtom := WinCheckH(GlobalAddAtom(PAnsiChar(WindowAtomString)));
  ControlAtom := WinCheckH(GlobalAddAtom(PAnsiChar(ControlAtomString)));
end;

initialization
  TNT_WM_DESTROY := RegisterWindowMessage('TntUnicodeVcl.DestroyWindow');
  WideControlHelpers := TComponentList.Create(True);
  PendingRecreateWndTrapList := TComponentList.Create(False);
  InitControls;

finalization
  GlobalDeleteAtom(ControlAtom);
  GlobalDeleteAtom(WindowAtom);
  FreeAndNil(WideControlHelpers);
  FreeAndNil(PendingRecreateWndTrapList);
  Finalized := True;

end.
