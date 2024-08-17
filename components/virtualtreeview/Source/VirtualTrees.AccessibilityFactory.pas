unit VirtualTrees.AccessibilityFactory;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is VirtualTrees.pas, released September 30, 2000.
//
// The initial developer of the original code is digital publishing AG (Munich, Germany, www.digitalpublishing.de),
// written by Mike Lischke (public@soft-gems.net, www.soft-gems.net).
//
// Portions created by digital publishing AG are Copyright
// (C) 1999-2001 digital publishing AG. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------


// class to create IAccessibles for the tree passed into it.
// If not already assigned, creates IAccessibles for the tree itself
// and the focused item
// the tree accessible is returned when the tree receives an WM_GETOBJECT message
// the AccessibleItem is returned when the Accessible is being asked for the first child
// To create your own IAccessibles, use the VTStandardAccessible unit as a reference,
// and assign your Accessibles to the variables in the unit's initialization.
// You only need to add the unit to your project, and voilá, you have an accessible string tree!
//
// Written by Marco Zehe. (c) 2007

interface

uses
  Winapi.oleacc,
  System.Classes,
  Vcl.Controls,
  VirtualTrees.BaseTree;

type
  IVTAccessibleProvider = interface
    function CreateIAccessible(ATree: TBaseVirtualTree): IAccessible;
  end;

  TVTAccessibilityFactory = class(TObject)
  strict private class var
    FAccessibilityAvailable: Boolean;
    FVTAccessibleFactory: TVTAccessibilityFactory;
  strict private
    FAccessibleProviders: TInterfaceList;
  private
    class procedure FreeFactory;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateIAccessible(ATree: TCustomControl): IAccessible;
    class function GetAccessibilityFactory: TVTAccessibilityFactory; static;
    procedure RegisterAccessibleProvider(const AProvider: IVTAccessibleProvider);
    procedure UnRegisterAccessibleProvider(const AProvider: IVTAccessibleProvider);
  end;


implementation

{ TVTAccessibilityFactory }

constructor TVTAccessibilityFactory.Create;
begin
  inherited Create;
  FAccessibleProviders := TInterfaceList.Create;
  FAccessibleProviders.Clear;
end;

function TVTAccessibilityFactory.CreateIAccessible(ATree: TCustomControl): IAccessible;
var
  I: Integer;
  TmpIAccessible: IAccessible;
  lTree: TBaseVirtualTree;
// returns an IAccessible.
// 1. If the Accessible property of the passed-in tree is nil,
// the first registered element will be returned.
// Usually, this is the IAccessible that provides information about the tree itself.
// If it is not nil, we'll check whether the AccessibleItem is nil.
// If it is, we'll look in the registered IAccessibles for the appropriate one.
// Each IAccessibleProvider will check the tree for properties to determine whether it is responsible.
// We'll work top to bottom, from the most complicated to the most simple.
// The index for these should all be greater than 0, e g the IAccessible for the tree itself should always be registered first, then any IAccessible items.
begin
  Result := nil;
  lTree := (ATree as TBaseVirtualTree);
  if lTree <> nil then
  begin
    if lTree.Accessible = nil then
    begin
      if FAccessibleProviders.Count > 0 then
      begin
        Result := IVTAccessibleProvider(FAccessibleProviders.Items[0]).CreateIAccessible(lTree);
        Exit;
      end;
    end;
    if lTree.AccessibleItem = nil then
    begin
      if FAccessibleProviders.Count > 0 then
      begin
        for I := FAccessibleProviders.Count - 1 downto 1 do
        begin
          TmpIAccessible := IVTAccessibleProvider(FAccessibleProviders.Items[I]).CreateIAccessible(lTree);
          if TmpIAccessible <> nil then
          begin
            Result := TmpIAccessible;
            Break;
          end;
        end;
        if TmpIAccessible = nil then
        begin
          Result := IVTAccessibleProvider(FAccessibleProviders.Items[0]).CreateIAccessible(lTree);
        end;
      end;
    end
    else
      Result := lTree.AccessibleItem;
  end;
end;

destructor TVTAccessibilityFactory.Destroy;
begin
  FAccessibleProviders.Free;
  FAccessibleProviders := nil;
  inherited Destroy;
end;

class procedure TVTAccessibilityFactory.FreeFactory;
begin
  FVTAccessibleFactory.Free;
end;

procedure TVTAccessibilityFactory.RegisterAccessibleProvider(const AProvider: IVTAccessibleProvider);
// Ads a provider if it is not already registered
begin
  if FAccessibleProviders.IndexOf(AProvider) < 0 then
    FAccessibleProviders.Add(AProvider)
end;

procedure TVTAccessibilityFactory.UnRegisterAccessibleProvider(const AProvider: IVTAccessibleProvider);
// Unregisters/removes an IAccessible provider if it is present
begin
  if FAccessibleProviders.IndexOf(AProvider) >= 0 then
    FAccessibleProviders.Remove(AProvider);
end;

class function TVTAccessibilityFactory.GetAccessibilityFactory: TVTAccessibilityFactory;
// Accessibility helper function to create a singleton class that will create or return
// the IAccessible interface for the tree and the focused node.

begin
  // first, check if we've loaded the library already
  if not FAccessibilityAvailable then
    FAccessibilityAvailable := True;
  if FAccessibilityAvailable then
  begin
    // Check to see if the class has already been created.
    if FVTAccessibleFactory = nil then
      FVTAccessibleFactory := TVTAccessibilityFactory.Create;
    Result := FVTAccessibleFactory;
  end
  else
    Result := nil;
end;

initialization

finalization
  TVTAccessibilityFactory.FreeFactory;

end.
