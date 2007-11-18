
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntMenus_Design;

{$INCLUDE ..\Source\TntCompilers.inc}

{*******************************************************}
{  Special Thanks to Francisco Leong for getting these  }
{    menu designer enhancements to work w/o MnuBuild.   }
{*******************************************************}

interface

{$IFDEF COMPILER_6}     // Delphi 6 and BCB 6 have MnuBuild available
  {$DEFINE MNUBUILD_AVAILABLE}
{$ENDIF}

{$IFDEF COMPILER_7}     // Delphi 7 has MnuBuild available
  {$DEFINE MNUBUILD_AVAILABLE}
{$ENDIF}

uses
  Windows, Classes, Menus, Messages,
  {$IFDEF MNUBUILD_AVAILABLE} MnuBuild, {$ENDIF}
  DesignEditors, DesignIntf;

type
  TTntMenuEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string{TNT-ALLOW string}; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  {$IFDEF MNUBUILD_AVAILABLE} MnuConst, {$ELSE} DesignWindows, {$ENDIF} SysUtils, Graphics, ActnList,
  Controls, Forms, TntDesignEditors_Design, TntActnList, TntMenus;

procedure Register;
begin
  RegisterComponentEditor(TTntMainMenu, TTntMenuEditor);
  RegisterComponentEditor(TTntPopupMenu, TTntMenuEditor);
end;

function GetMenuBuilder: TForm{TNT-ALLOW TForm};
{$IFDEF MNUBUILD_AVAILABLE}
begin
  Result := MenuEditor;
{$ELSE}
var
  Comp: TComponent;
begin
  Result := nil;
  if Application <> nil then
  begin
    Comp := Application.FindComponent('MenuBuilder');
    if Comp is TForm{TNT-ALLOW TForm} then
      Result := TForm{TNT-ALLOW TForm}(Comp);
  end;
{$ENDIF}
end;

{$IFDEF DELPHI_9} // verified against Delphi 9
type
  THackMenuBuilder = class(TDesignWindow)
  protected
    Fields: array[1..26] of TObject;
    FWorkMenu: TMenuItem{TNT-ALLOW TMenuItem};
  end;
{$ENDIF}

{$IFDEF COMPILER_10_UP}
{$IFDEF DELPHI_10} // NOT verified against Delphi 10
type
  THackMenuBuilder = class(TDesignWindow)
  protected
    Fields: array[1..26] of TObject;
    FWorkMenu: TMenuItem{TNT-ALLOW TMenuItem};
  end;
{$ENDIF}
{$ENDIF}

function GetMenuBuilder_WorkMenu(MenuBuilder: TForm{TNT-ALLOW TForm}): TMenuItem{TNT-ALLOW TMenuItem};
begin
  if MenuBuilder = nil then
    Result := nil
  else begin
    {$IFDEF MNUBUILD_AVAILABLE}
    Result := MenuEditor.WorkMenu;
    {$ELSE}
    Result := THackMenuBuilder(MenuBuilder).FWorkMenu;
    Assert((Result = nil) or (Result is TMenuItem{TNT-ALLOW TMenuItem}),
      'TNT Internal Error: THackMenuBuilder has incorrect internal layout.');
    {$ENDIF}
  end;
end;

{$IFDEF DELPHI_9} // verified against Delphi 9
type
  THackMenuItemWin = class(TCustomControl)
  protected
    FxxxxCaptionExtent: Integer;
    FMenuItem: TMenuItem{TNT-ALLOW TMenuItem};
  end;
{$ENDIF}

{$IFDEF DELPHI_10} // beta: NOT verified against Delphi 10
type
  THackMenuItemWin = class(TCustomControl)
  protected
    FxxxxCaptionExtent: Integer;
    FMenuItem: TMenuItem{TNT-ALLOW TMenuItem};
  end;
{$ENDIF}

function GetMenuItem(Control: TWinControl; DoVerify: Boolean = True): TMenuItem{TNT-ALLOW TMenuItem};
begin
  {$IFDEF MNUBUILD_AVAILABLE}
  if Control is TMenuItemWin then
    Result := TMenuItemWin(Control).MenuItem
  {$ELSE}
  if Control.ClassName = 'TMenuItemWin' then begin
    Result := THackMenuItemWin(Control).FMenuItem;
    Assert((Result = nil) or (Result is TMenuItem{TNT-ALLOW TMenuItem}), 'TNT Internal Error: Unexpected TMenuItem field layout.');
  end
  {$ENDIF}
  else if DoVerify then
    raise Exception.Create('TNT Internal Error: Control is not a TMenuItemWin.')
  else
    Result := nil;
end;

procedure SetMenuItem(Control: TWinControl; Item: TMenuItem{TNT-ALLOW TMenuItem});
begin
  {$IFDEF MNUBUILD_AVAILABLE}
  if Control is TMenuItemWin then
    TMenuItemWin(Control).MenuItem := Item
  {$ELSE}
  if Control.ClassName = 'TMenuItemWin' then begin
    THackMenuItemWin(Control).FMenuItem := Item;
    Item.FreeNotification(Control);
  end
  {$ENDIF}
  else
    raise Exception.Create('TNT Internal Error: Control is not a TMenuItemWin.');
end;

procedure ReplaceMenuItem(Control: TWinControl; ANewItem: TMenuItem{TNT-ALLOW TMenuItem});
var
  OldItem: TMenuItem{TNT-ALLOW TMenuItem};
  OldName: string{TNT-ALLOW string};
begin
  OldItem := GetMenuItem(Control, True);
  Assert(OldItem <> nil);
  OldName := OldItem.Name;
  FreeAndNil(OldItem);
  ANewItem.Name := OldName; { assume old name }
  SetMenuItem(Control, ANewItem);
end;

{ TTntMenuBuilderChecker }

type
  TMenuBuilderChecker = class(TComponent)
  private
    FMenuBuilder: TForm{TNT-ALLOW TForm};
    FCheckMenuAction: TTntAction;
    FLastCaption: string{TNT-ALLOW string};
    FLastActiveControl: TControl;
    FLastMenuItem: TMenuItem{TNT-ALLOW TMenuItem};
    procedure CheckMenuItems(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var MenuBuilderChecker: TMenuBuilderChecker = nil;

constructor TMenuBuilderChecker.Create(AOwner: TComponent);
begin
  inherited;
  MenuBuilderChecker := Self;
  FCheckMenuAction := TTntAction.Create(Self);
  FCheckMenuAction.OnUpdate := CheckMenuItems;
  FCheckMenuAction.OnExecute := CheckMenuItems;
  FMenuBuilder := AOwner as TForm{TNT-ALLOW TForm};
  FMenuBuilder.Action := FCheckMenuAction;
end;

destructor TMenuBuilderChecker.Destroy;
begin
  FMenuBuilder := nil;
  MenuBuilderChecker := nil;
  inherited;
end;

type TAccessTntMenuItem = class(TTntMenuItem);

function CreateTntMenuItem(OldItem: TMenuItem{TNT-ALLOW TMenuItem}): TTntMenuItem;
var
  OldName: AnsiString;
  OldParent: TMenuItem{TNT-ALLOW TMenuItem};
  OldIndex: Integer;
  OldItemsList: TList;
  j: integer;
begin
  // item should be converted.
  OldItemsList := TList.Create;
  try
    // clone properties
    Result := TTntMenuItem.Create(OldItem.Owner);
    TAccessTntMenuItem(Result).FComponentStyle := OldItem.ComponentStyle; {csTransient hides item from object inspector}
    Result.Action := OldItem.Action;
    Result.AutoCheck := OldItem.AutoCheck;
    Result.AutoHotkeys := OldItem.AutoHotkeys;
    Result.AutoLineReduction := OldItem.AutoLineReduction;
    Result.Bitmap := OldItem.Bitmap;
    Result.Break := OldItem.Break;
    Result.Caption := OldItem.Caption;
    Result.Checked := OldItem.Checked;
    Result.Default := OldItem.Default;
    Result.Enabled := OldItem.Enabled;
    Result.GroupIndex := OldItem.GroupIndex;
    Result.HelpContext := OldItem.HelpContext;
    Result.Hint := OldItem.Hint;
    Result.ImageIndex := OldItem.ImageIndex;
    Result.MenuIndex := OldItem.MenuIndex;
    Result.RadioItem := OldItem.RadioItem;
    Result.ShortCut := OldItem.ShortCut;
    Result.SubMenuImages := OldItem.SubMenuImages;
    Result.Visible := OldItem.Visible;
    Result.Tag := OldItem.Tag;

    // clone events
    Result.OnAdvancedDrawItem := OldItem.OnAdvancedDrawItem;
    Result.OnClick := OldItem.OnClick;
    Result.OnDrawItem := OldItem.OnDrawItem;
    Result.OnMeasureItem := OldItem.OnMeasureItem;

    // remember name, parent, index, children
    OldName := OldItem.Name;
    OldParent := OldItem.Parent;
    OldIndex := OldItem.MenuIndex;
    for j := OldItem.Count - 1 downto 0 do begin
      OldItemsList.Insert(0, OldItem.Items[j]);
      OldItem.Remove(OldItem.Items[j]);
    end;
    
    // clone final parts of old item
    for j := 0 to OldItemsList.Count - 1 do
      Result.Add(TMenuItem{TNT-ALLOW TMenuItem}(OldItemsList[j])); { add children }
    if OldParent <> nil then
      OldParent.Insert(OldIndex, Result); { insert into parent }
  finally
    OldItemsList.Free;
  end;
end;

procedure CheckMenuItemWin(MenuItemWin: TWinControl; PartOfATntMenu: Boolean);
var
  OldItem: TMenuItem{TNT-ALLOW TMenuItem};
begin
  OldItem := GetMenuItem(MenuItemWin);
  if OldItem = nil then
    exit;
  if (OldItem.ClassType = TMenuItem{TNT-ALLOW TMenuItem})
  and (PartOfATntMenu or (OldItem.Parent is TTntMenuItem)) then
  begin
    if MenuItemWin.Focused then
      MenuItemWin.Parent.SetFocus;  {Lose focus and regain later to ensure object inspector gets updated.}
    ReplaceMenuItem(MenuItemWin, CreateTntMenuItem(OldItem));
  end else if (OldItem.ClassType = TTntMenuItem)
  and (OldItem.Parent = nil) and (OldItem.Caption = '') and (OldItem.Name = '')
  and not (PartOfATntMenu or (OldItem.Parent is TTntMenuItem)) then begin
    if MenuItemWin.Focused then
      MenuItemWin.Parent.SetFocus;  {Lose focus and regain later to ensure object inspector gets updated.}
    ReplaceMenuItem(MenuItemWin, TMenuItem{TNT-ALLOW TMenuItem}.Create(OldItem.Owner));
  end;
end;

procedure TMenuBuilderChecker.CheckMenuItems(Sender: TObject);
var
  a, i: integer;
  MenuWin: TWinControl;
  MenuItemWin: TWinControl;
  SaveFocus: HWND;
  PartOfATntMenu: Boolean;
  WorkMenu: TMenuItem{TNT-ALLOW TMenuItem};
begin
  if (FMenuBuilder <> nil)
  and (FMenuBuilder.Action = FCheckMenuAction) then begin
    if (FLastCaption <> FMenuBuilder.Caption)
    or (FLastActiveControl <> FMenuBuilder.ActiveControl)
    or (FLastMenuItem <> GetMenuItem(FMenuBuilder.ActiveControl, False))
    then begin
      try
        try
          with FMenuBuilder do begin
            WorkMenu := GetMenuBuilder_WorkMenu(FMenuBuilder);
            PartOfATntMenu := (WorkMenu <> nil)
              and ((WorkMenu.Owner is TTntMainMenu) or (WorkMenu.Owner is TTntPopupMenu));
            SaveFocus := Windows.GetFocus;
            for a := ComponentCount - 1 downto 0 do begin
              {$IFDEF MNUBUILD_AVAILABLE}
              if Components[a] is TMenuWin then begin
              {$ELSE}
              if Components[a].ClassName = 'TMenuWin' then begin
              {$ENDIF}
                MenuWin := Components[a] as TWinControl;
                with MenuWin do begin
                  for i := ComponentCount - 1 downto 0 do begin
                    {$IFDEF MNUBUILD_AVAILABLE}
                    if Components[i] is TMenuItemWin then begin
                    {$ELSE}
                    if Components[i].ClassName = 'TMenuItemWin' then begin
                    {$ENDIF}
                      MenuItemWin := Components[i] as TWinControl;
                      CheckMenuItemWin(MenuItemWin, PartOfATntMenu);
                    end;
                  end;
                end;
              end;
            end;
            if SaveFocus <> Windows.GetFocus then
              Windows.SetFocus(SaveFocus);
          end;
        except
          on E: Exception do begin
            FMenuBuilder.Action := nil;
          end;
        end;
      finally
        FLastCaption := FMenuBuilder.Caption;
        FLastActiveControl := FMenuBuilder.ActiveControl;
        FLastMenuItem := GetMenuItem(FMenuBuilder.ActiveControl, False);
      end;
    end;
  end;
end;

{ TTntMenuEditor }

function TTntMenuEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFNDEF MNUBUILD_AVAILABLE}
resourcestring
  SMenuDesigner = 'Menu Designer...';
{$ENDIF}

function TTntMenuEditor.GetVerb(Index: Integer): string{TNT-ALLOW string};
begin
  Result := SMenuDesigner;
end;

procedure TTntMenuEditor.ExecuteVerb(Index: Integer);
var
  MenuBuilder: TForm{TNT-ALLOW TForm};
begin
  EditPropertyWithDialog(Component, 'Items', Designer);
  MenuBuilder := GetMenuBuilder;
  if Assigned(MenuBuilder) then begin
    if (MenuBuilderChecker = nil) or (MenuBuilderChecker.FMenuBuilder <> MenuBuilder) then begin
      MenuBuilderChecker.Free;
      MenuBuilderChecker := TMenuBuilderChecker.Create(MenuBuilder);
    end;
    EditPropertyWithDialog(Component, 'Items', Designer); // update menu builder caption
  end;
end;

initialization

finalization
  MenuBuilderChecker.Free; // design package might be recompiled

end.
