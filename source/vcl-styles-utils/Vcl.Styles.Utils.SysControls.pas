// ***************************************************************************************************
//
// Unit Vcl.Styles.Utils.SysControls
// unit for the VCL Styles Utils
// https://github.com/RRUZ/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License")
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
unit Vcl.Styles.Utils.SysControls;


interface

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Themes,
  Vcl.Styles.Utils.SysStyleHook;

type
  PChildControlInfo = ^TChildControlInfo;

  TChildControlInfo = record
    Parent: HWND;
    ParentStyle: NativeInt;
    StyleHookClass: TSysStyleHookClass;
  end;

  PControlInfo = ^TControlInfo;

  TControlInfo = record
    Handle: HWND;
    Parent: HWND;
    Style: NativeInt;
    ParentStyle: NativeInt;
    ExStyle: NativeInt;
    ParentExStyle: NativeInt;
    ClassName: PChar;
    ParentClassName: PChar;
  end;

type
  TSysHookAction = (cAdded, cRemoved);
  TBeforeHookingControl = function(Info: PControlInfo): Boolean;
  TSysHookNotification = procedure(Action: TSysHookAction; Info: PControlInfo);

  TSysStyleManager = class(TComponent)
  private
  class var
    FEnabled: Boolean;
    FHook_WH_CBT: HHook;
    FBeforeHookingControlProc: TBeforeHookingControl;
    FSysHookNotificationProc: TSysHookNotification;
    FRegSysStylesList: TObjectDictionary<String, TSysStyleHookClass>;
    FSysStyleHookList: TObjectDictionary<HWND, TSysStyleHook>;
    FChildRegSysStylesList: TObjectDictionary<HWND, TChildControlInfo>;
    FHookVclControls: Boolean;
    FUseStyleColorsChildControls: Boolean;
    class var FHookDialogIcons: Boolean;
  protected
    /// <summary>
    /// Install the Hook
    /// </summary>
    class procedure InstallHook_WH_CBT;
    /// <summary>
    /// Remove the Hook
    /// </summary>
    class procedure RemoveHook_WH_CBT;
    /// <summary>
    /// Hook Callback
    /// </summary>
    class function HookActionCallBackCBT(nCode: Integer; wParam: wParam; lParam: lParam): LRESULT; stdcall; static;
  public
    /// <summary>
    /// Register a Sys Style Hook for an specified class.
    /// </summary>
    class procedure RegisterSysStyleHook(const SysControlClass: String; SysStyleHookClass: TSysStyleHookClass);
    /// <summary>
    /// UnRegister a Sys Style Hook for an specified class.
    /// </summary>
    class procedure UnRegisterSysStyleHook(const SysControlClass: String; SysStyleHookClass: TSysStyleHookClass);
    class constructor Create;
    class destructor Destroy;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    /// Event to preventvor allow hook a control.
    /// </summary>
    class Property OnBeforeHookingControl: TBeforeHookingControl read FBeforeHookingControlProc write FBeforeHookingControlProc;
    /// <summary>
    /// Notify when a hook foir control is added or removed
    /// </summary>
    class Property OnHookNotification: TSysHookNotification read FSysHookNotificationProc write FSysHookNotificationProc;
    /// <summary>
    /// Enable or disable the style of the controls
    /// </summary>
    class property Enabled: Boolean read FEnabled write FEnabled;
    /// <summary>
    /// Allow set the current VCL Style font and background color in  child
    /// controls.
    /// </summary>
    class property UseStyleColorsChildControls: Boolean read FUseStyleColorsChildControls write FUseStyleColorsChildControls;
    /// <summary>
    /// Allow disable or enable the hook of VCL Controls
    /// </summary>
    class property HookVclControls: Boolean read FHookVclControls write FHookVclControls;
    /// <summary>
    /// Allow disable or enable the hook of the icons dialogs
    /// </summary>
    class property HookDialogIcons: Boolean read FHookDialogIcons write FHookDialogIcons;
    /// <summary>
    /// Collection of Styled (Hooked) Controls
    /// </summary>
    class property SysStyleHookList: TObjectDictionary<HWND, TSysStyleHook> read FSysStyleHookList;
    /// <summary>
    /// Collection of Styled Child Controls
    /// </summary>
    class property ChildRegSysStylesList: TObjectDictionary<HWND, TChildControlInfo> read FChildRegSysStylesList;
    class procedure AddControlDirectly(Handle: HWND; const sClassName : string; IncludeChildControls : Boolean = False);
  end;


implementation

uses
  WinApi.CommCtrl,
  Vcl.Styles.Utils.Misc;

function FindWinFromRoot(Root: HWND; ClassName: PChar): HWND;
var
  Next, Child: HWND;
  S: String;
begin
  Result := 0;
  Next := GetWindow(Root, GW_CHILD or GW_HWNDFIRST);
  while (Next > 0) do
  begin
    S := GetWindowClassName(Next);
    if S = String(ClassName) then
      Exit(Next);
    Next := GetWindow(Next, GW_HWNDNEXT);
    Child := GetWindow(Next, GW_CHILD or GW_HWNDFIRST);
    if Child > 0 then
      Result := FindWinFromRoot(Next, ClassName);
    if Result > 0 then
      Exit;
  end;
end;

{ -------------------------------------------------------------------------------------- }
{ TSysStyleManager }
function BeforeHookingControl(Info: PControlInfo): Boolean;
var
  LInfo: TControlInfo;
  Root, C: HWND;
begin
  {
    Return true to allow control hooking !
    Return false to prevent control hooking !
  }
  { NB: The ClassName is always in lowercase . }
  LInfo := Info^;
  Result := True;
  Root := GetAncestor(LInfo.Parent, GA_ROOT);
  if FindWinFromRoot(Root, 'DirectUIHWND') > 0 then
  begin
    Result := False;
    Exit;
  end;

  if SameText(LInfo.ClassName,  WC_LISTVIEW) then
  begin
    if SameText(LInfo.ParentClassName, 'listviewpopup') then
     Result:=False;
  end
  else
  if SameText(LInfo.ClassName, TRACKBAR_CLASS) then
  begin
    if SameText(LInfo.ParentClassName, 'ViewControlClass') then
     Result:=False;
  end
  else
  //Prevent hook Toolbars on DirectUIHWND
  if SameText(LInfo.ClassName, TOOLBARCLASSNAME) then
  begin
    if SameText(LInfo.ParentClassName, 'ViewControlClass') then
     Result:=False
    else
    if Root > 0 then
    begin
      C := FindWinFromRoot(Root, REBARCLASSNAME);
      Result := not(C > 0);
    end;
  end;
end;

procedure HookNotification(Action: TSysHookAction; Info: PControlInfo);
begin

end;

class constructor TSysStyleManager.Create;
begin
  FHook_WH_CBT:=0;
  FBeforeHookingControlProc := @BeforeHookingControl;
  FSysHookNotificationProc := @HookNotification;
  FUseStyleColorsChildControls := True;
  FEnabled := True;
  FHookDialogIcons := False;
  FHookVclControls := False;
  FSysStyleHookList := TObjectDictionary<HWND, TSysStyleHook>.Create([doOwnsValues]);
  FRegSysStylesList := TObjectDictionary<String, TSysStyleHookClass>.Create;
  FChildRegSysStylesList := TObjectDictionary<HWND, TChildControlInfo>.Create;
  //FSysStyleHookList := TObjectDictionary<HWND, TSysStyleHook>.Create([]);
  InstallHook_WH_CBT;
end;

class destructor TSysStyleManager.Destroy;
begin
  RemoveHook_WH_CBT;
  FRegSysStylesList.Free;
  FSysStyleHookList.Free; // remove the childs too because doOwnsValues
  FChildRegSysStylesList.Free;
  inherited;
end;

class procedure TSysStyleManager.AddControlDirectly(Handle: HWND; const sClassName : string; IncludeChildControls : Boolean = False);
var
 LStyleHook  : TSysStyleHook;
 ParentStyle : DWORD;

  procedure AddChildControl(ChildHandle: HWND);
  var
    Info: TChildControlInfo;
    sChildClassName : string;
    LStyleHook  : TSysStyleHook;
  begin
   { Hook the control directly ! }
    ZeroMemory(@Info, sizeof(TChildControlInfo));
    Info.Parent := Handle;
    Info.ParentStyle := ParentStyle;
    sChildClassName := LowerCase(GetWindowClassName(ChildHandle));
    if FRegSysStylesList.ContainsKey(sChildClassName) then
    begin
      LStyleHook:=FRegSysStylesList[LowerCase(sChildClassName)].Create(ChildHandle);
      FSysStyleHookList.Add(ChildHandle, LStyleHook);
      SendMessage(ChildHandle, CM_CONTROLHOOKEDDIRECTLY, 0, 0);
      InvalidateRect(ChildHandle, nil, False);
//    if Assigned(FSysHookNotificationProc) then
//      FSysHookNotificationProc(cAdded, @Info);
    end;
  end;

  function EnumChildProc(const hWindow: hWnd; const LParam : LParam): boolean; stdcall;
  begin
    AddChildControl(hWindow);
    Result:= True;
  end;

begin
  if not FRegSysStylesList.ContainsKey(LowerCase(sClassName)) then
   Exit;
  { Hook the control directly ! }
  if FSysStyleHookList.ContainsKey(Handle) then
    FSysStyleHookList.Remove(Handle);
  LStyleHook:=FRegSysStylesList[LowerCase(sClassName)].Create(Handle);
  FSysStyleHookList.Add(Handle, LStyleHook);
  SendMessage(Handle, CM_CONTROLHOOKEDDIRECTLY, 0, 0);
//    if Assigned(FSysHookNotificationProc) then
//      FSysHookNotificationProc(cAdded, @Info);

  if IncludeChildControls then
  begin
   ParentStyle:=GetWindowLongPtr(Handle, GWL_STYLE);
   EnumChildWindows(Handle, @EnumChildProc, 0);
  end;
end;


constructor TSysStyleManager.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TSysStyleManager.Destroy;
begin
  inherited;
end;

type
 TSysStyleClass = class(TSysStyleHook);

class function TSysStyleManager.HookActionCallBackCBT(nCode: Integer; wParam: wParam; lParam: lParam): LRESULT;
var
  CBTSturct: TCBTCreateWnd;
  sClassName, Tmp: string;
  {LHWND,} Parent: HWND;
  Style, ParentStyle, ExStyle, ParentExStyle: NativeInt;
  Info: TControlInfo;

  procedure RemoveUnusedHooks;
  var
    LHandle : THandle;
  begin
   for LHandle in TSysStyleManager.SysStyleHookList.Keys do
    if TSysStyleClass(TSysStyleManager.SysStyleHookList.Items[LHandle]).MustRemove then
      TSysStyleManager.SysStyleHookList.Remove(LHandle);
  end;

  procedure AddChildControl(Handle: HWND);
  var
    Info: TChildControlInfo;
  begin
    { The child control will be hooked inside it's parent control. }
    ZeroMemory(@Info, sizeof(TChildControlInfo));
    Info.Parent := Parent;
    Info.ParentStyle := ParentStyle;
    Info.StyleHookClass := FRegSysStylesList[sClassName];
    if FChildRegSysStylesList.ContainsKey(Handle) then
      FChildRegSysStylesList.Remove(Handle);
    FChildRegSysStylesList.Add(Handle, Info);
    if Assigned(FSysHookNotificationProc) then
      FSysHookNotificationProc(cAdded, @Info);
  end;

  procedure AddControl(Handle: HWND);
  var
   LStyleHook : TSysStyleHook;
  begin
    { Hook the control directly ! }
    RemoveUnusedHooks;
    if FSysStyleHookList.ContainsKey(Handle) then
      FSysStyleHookList.Remove(Handle);
    LStyleHook:=FRegSysStylesList[sClassName].Create(Handle);
    FSysStyleHookList.Add(Handle, LStyleHook);
    SendMessage(Handle, CM_CONTROLHOOKEDDIRECTLY, 0, 0);
    if Assigned(FSysHookNotificationProc) then
      FSysHookNotificationProc(cAdded, @Info);
  end;

begin
  Result := CallNextHookEx(FHook_WH_CBT, nCode, wParam, lParam);
  if not FEnabled then
    Exit;

//  if (nCode = HCBT_ACTIVATE) and not(StyleServices.IsSystemStyle) then
//     begin
//       LHWND := HWND(wParam);
//       if(LHWND>0) then
//       begin
//          sClassName:= GetWindowClassName(LHWND);
//          if (sClassName<>'') and  (not TSysStyleManager.SysStyleHookList.ContainsKey(LHWND)) and (SameText(sClassName,'#32770'))  then
//          begin
//            TSysStyleManager.AddControlDirectly(LHWND, sClassName);
//            InvalidateRect(LHWND, nil, False);
//          end;
//       end;
//     end;


  if (nCode = HCBT_CREATEWND) and not(StyleServices.IsSystemStyle) then
  begin

    CBTSturct := PCBTCreateWnd(lParam)^;
    sClassName := GetWindowClassName(wParam);
    sClassName := LowerCase(sClassName);

//    if SameText(sClassName, '#32770') then
//      OutputDebugString(PChar('Class '+sclassName+' '+IntToHex(wParam, 8)));

    Parent := CBTSturct.lpcs.hwndParent;
    Style := CBTSturct.lpcs.Style;
    ExStyle := CBTSturct.lpcs.dwExStyle;
    ParentExStyle := 0;
    ParentStyle := 0;

    if Parent > 0 then
    begin
      ParentStyle := GetWindowLongPtr(Parent, GWL_STYLE);
      ParentExStyle := GetWindowLongPtr(Parent, GWL_EXSTYLE);
    end;

    if FRegSysStylesList.ContainsKey(sClassName) then
    begin
      Info.Handle := wParam;
      Info.Parent := Parent;
      Info.Style := Style;
      Info.ParentStyle := ParentStyle;
      Info.ExStyle := ExStyle;
      Info.ParentExStyle := ParentExStyle;
      Tmp := sClassName;
      Info.ClassName := PChar(Tmp);
      Tmp := LowerCase(GetWindowClassName(Parent));
      Info.ParentClassName := PChar(Tmp);

      if not HookVclControls then
        if IsVCLControl(wParam) then
          Exit;

      if Assigned(FBeforeHookingControlProc) then
        if not FBeforeHookingControlProc(@Info) then
          Exit;

      if (Style and DS_CONTROL = DS_CONTROL) then
      begin
        { TabSheet ! }
        AddControl(wParam);
        PostMessage(wParam, CM_INITCHILDS, 0, 0);
      end
      else if (Style and WS_POPUP = WS_POPUP) then
      begin
        { Parent Control ! }
        AddControl(wParam);
      end
      else if (Style and WS_CHILD = WS_CHILD) then
      begin
        { Child Control ! }
        if FSysStyleHookList.ContainsKey(Parent) then
        begin
          { Parent is already hooked . }
          if IsVCLControl(Parent) then
            { Parent is a VCL control . }
            AddControl(wParam)
          else
            AddChildControl(wParam)
        end
        else
          { Parent not registered (not hooked). }
          AddControl(wParam);
      end
      else
        { Not (WS_CHILD or WS_POPUP) !! }
        AddControl(wParam);
    end;

   // if FSysStyleHookList.ContainsKey(wParam) or  FChildRegSysStylesList.ContainsKey(wParam) then
   //  OutputDebugString(PChar('Hooked '+IntToHex(wParam, 8)));

  end;


  if nCode = HCBT_DESTROYWND then
  begin
    //OutputDebugString(PChar('HCBT_DESTROYWND Handle '+IntToHex(wParam, 8)));
    if FSysStyleHookList.ContainsKey(wParam) then
    begin
      ZeroMemory(@Info, sizeof(TControlInfo));
      Info.Handle := wParam;
      if Assigned(FSysHookNotificationProc) then
        OnHookNotification(cRemoved, @Info);
      // FSysStyleHookList.Remove(wParam); -> removed in WM_NCDESTROY
    end;
  end;
end;

class procedure TSysStyleManager.InstallHook_WH_CBT;
begin
  FHook_WH_CBT := SetWindowsHookEx(WH_CBT, @HookActionCallBackCBT, 0, GetCurrentThreadId);
end;

class procedure TSysStyleManager.RegisterSysStyleHook(const SysControlClass: String; SysStyleHookClass: TSysStyleHookClass);
begin
  if FRegSysStylesList.ContainsKey(LowerCase(SysControlClass)) then
    FRegSysStylesList.Remove(LowerCase(SysControlClass));
  FRegSysStylesList.Add(LowerCase(SysControlClass), SysStyleHookClass);
end;

class procedure TSysStyleManager.RemoveHook_WH_CBT;
begin
  if FHook_WH_CBT <> 0 then
    UnhookWindowsHookEx(FHook_WH_CBT);
end;

class procedure TSysStyleManager.UnRegisterSysStyleHook(const SysControlClass: String; SysStyleHookClass: TSysStyleHookClass);
begin
  if FRegSysStylesList.ContainsKey(LowerCase(SysControlClass)) then
    FRegSysStylesList.Remove(LowerCase(SysControlClass));
end;

end.
