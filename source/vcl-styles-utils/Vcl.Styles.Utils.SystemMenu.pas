// **************************************************************************************************
//
// Unit Vcl.Styles.Utils.SystemMenu
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
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2014-2019 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.Utils.SystemMenu;

interface

uses
  System.Rtti,
  System.Classes,
  System.Generics.Collections,
  WinApi.Windows,
  WinApi.Messages,
  Vcl.Themes,
  Vcl.Forms;

type
  TMethodInfo = class;

  TProcCallback = reference to procedure(Info: TMethodInfo);

  TMethodInfo = class
    Value1: TValue;
    Value2: TValue;
    Method: TProcCallback;
  end;

  TVclStylesSystemMenu = class(TComponent)
  strict private
    FVCLStylesMenu: HMenu;
    FOrgWndProc: TWndMethod;
    FForm: TForm;
    FMethodsDict: TObjectDictionary<NativeUInt, TMethodInfo>;
    procedure CreateMenus;
    procedure DeleteMenus;
    procedure CreateMenuStyles;
    procedure WndProc(var Message: TMessage);
  private
    FMenuCaption: string;
    FShowNativeStyle: Boolean;
    procedure SetMenuCaption(const Value: string);
    procedure SetShowNativeStyle(const Value: Boolean);
  public
    property ShowNativeStyle: Boolean read FShowNativeStyle write SetShowNativeStyle;
    property MenuCaption: string read FMenuCaption write SetMenuCaption;
    constructor Create(AOwner: TForm); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Vcl.Controls,
  System.SysUtils;

const
  VCLStylesMenu = WM_USER + 666;

function InsertMenuHelper(HMenu: HMenu; uPosition: UINT; uIDNewItem: UINT_PTR; lpNewItem, IconName: LPCWSTR): BOOL;
var
  LMenuItem: TMenuItemInfo;
begin
  ZeroMemory(@LMenuItem, SizeOf(TMenuItemInfo));
  LMenuItem.cbSize := SizeOf(TMenuItemInfo);
  LMenuItem.fMask := MIIM_FTYPE or MIIM_ID or MIIM_BITMAP or MIIM_STRING;
  LMenuItem.fType := MFT_STRING;
  LMenuItem.wID := uIDNewItem;
  LMenuItem.dwTypeData := lpNewItem;
  Result := InsertMenuItem(HMenu, uPosition, True, LMenuItem);
end;

procedure AddMenuSeparatorHelper(HMenu: HMenu; var MenuIndex: Integer);
var
  LMenuInfo: TMenuItemInfo;
  Buffer: array [0 .. 79] of char;
begin
  ZeroMemory(@LMenuInfo, SizeOf(TMenuItemInfo));
  LMenuInfo.cbSize := SizeOf(LMenuInfo);
  LMenuInfo.fMask := MIIM_TYPE;
  LMenuInfo.dwTypeData := Buffer;
  LMenuInfo.cch := SizeOf(Buffer);
  if GetMenuItemInfo(HMenu, MenuIndex - 1, True, LMenuInfo) then
  begin
    if (LMenuInfo.fType and MFT_SEPARATOR) = MFT_SEPARATOR then
    else
    begin
      InsertMenu(HMenu, MenuIndex, MF_BYPOSITION or MF_SEPARATOR, 0, nil);
      inc(MenuIndex);
    end;
  end;
end;

{ TVclStylesSystemMenu }

constructor TVclStylesSystemMenu.Create(AOwner: TForm);
begin
  inherited Create(AOwner);
  FShowNativeStyle := True;
  FMenuCaption := 'VCL Styles';
  FForm := AOwner;
  FMethodsDict := TObjectDictionary<NativeUInt, TMethodInfo>.Create([doOwnsValues]);
  FOrgWndProc := FForm.WindowProc;
  FForm.WindowProc := WndProc;
  CreateMenus;
end;

destructor TVclStylesSystemMenu.Destroy;
begin
  DeleteMenus;
  FForm.WindowProc := FOrgWndProc;
  FMethodsDict.Free;
  inherited;
end;

procedure TVclStylesSystemMenu.SetMenuCaption(const Value: string);
begin
  DeleteMenus;
  FMenuCaption := Value;
  CreateMenus;
end;

procedure TVclStylesSystemMenu.SetShowNativeStyle(const Value: Boolean);
begin
  DeleteMenus;
  FShowNativeStyle := Value;
  CreateMenus;
end;

procedure TVclStylesSystemMenu.CreateMenus;
begin
  CreateMenuStyles;
end;

procedure TVclStylesSystemMenu.DeleteMenus;
var
  LSysMenu: HMenu;
begin
  if IsMenu(FVCLStylesMenu) then
    while GetMenuItemCount(FVCLStylesMenu) > 0 do
      DeleteMenu(FVCLStylesMenu, 0, MF_BYPOSITION);

  if FForm.HandleAllocated then
  begin
    LSysMenu := GetSystemMenu(FForm.Handle, False);
    if IsMenu(LSysMenu) then
      DeleteMenu(LSysMenu, VCLStylesMenu, MF_BYCOMMAND);
  end;

  FMethodsDict.Clear;
end;

procedure TVclStylesSystemMenu.CreateMenuStyles;
var
  LSysMenu: HMenu;
  LMenuItem: TMenuItemInfo;
  uIDNewItem, LSubMenuIndex: Integer;
  LMethodInfo: TMethodInfo;
  s: string;
  LStyleNames: TArray<string>;

begin
  LSysMenu := GetSystemMenu(FForm.Handle, False);

  LSubMenuIndex := GetMenuItemCount(LSysMenu);
  AddMenuSeparatorHelper(LSysMenu, LSubMenuIndex);

  FVCLStylesMenu := CreatePopupMenu();

  uIDNewItem := VCLStylesMenu;
  ZeroMemory(@LMenuItem, SizeOf(TMenuItemInfo));
  LMenuItem.cbSize := SizeOf(TMenuItemInfo);
  LMenuItem.fMask := MIIM_SUBMENU or MIIM_FTYPE or MIIM_ID or MIIM_BITMAP or MIIM_STRING;
  LMenuItem.fType := MFT_STRING;
  LMenuItem.wID := VCLStylesMenu;
  LMenuItem.hSubMenu := FVCLStylesMenu;
  LMenuItem.dwTypeData := PWideChar(FMenuCaption);
  LMenuItem.cch := Length(FMenuCaption);

  InsertMenuItem(LSysMenu, GetMenuItemCount(LSysMenu), True, LMenuItem);
  inc(uIDNewItem);
  LSubMenuIndex := 0;

  LStyleNames := TStyleManager.StyleNames;
  TArray.Sort<String>(LStyleNames);

  for s in LStyleNames do
  begin

    if not FShowNativeStyle and SameText('Windows', s) then
      Continue;

    InsertMenuHelper(FVCLStylesMenu, LSubMenuIndex, uIDNewItem, PChar(s), nil);
    if SameText(TStyleManager.ActiveStyle.Name, s) then
      CheckMenuItem(FVCLStylesMenu, LSubMenuIndex, MF_BYPOSITION or MF_CHECKED);

    if SameText('Windows', s) then
      AddMenuSeparatorHelper(FVCLStylesMenu, LSubMenuIndex);

    inc(LSubMenuIndex);
    inc(uIDNewItem);
    LMethodInfo := TMethodInfo.Create;
    LMethodInfo.Value1 := s;
    LMethodInfo.Method := procedure(Info: TMethodInfo)
      begin
        TStyleManager.SetStyle(Info.Value1.AsString);
      end;
    FMethodsDict.Add(uIDNewItem - 1, LMethodInfo);
  end;
end;

procedure TVclStylesSystemMenu.WndProc(var Message: TMessage);
var
  LVerb: NativeUInt;
begin
  case Message.Msg of
    CM_RECREATEWND:
      begin
        DeleteMenus;
        FOrgWndProc(Message);
        CreateMenus;
      end;

    WM_SYSCOMMAND:
      begin
        if FMethodsDict.ContainsKey(TWMSysCommand(Message).CmdType) then
        begin
          LVerb := TWMSysCommand(Message).CmdType;
          FMethodsDict.Items[LVerb].Method(FMethodsDict.Items[LVerb]);
        end
        else
          FOrgWndProc(Message);
      end
  else
    FOrgWndProc(Message);
  end;
end;

end.
