
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntActnList;

{$INCLUDE compilers.inc}

interface

uses
  Classes, Controls, ActnList, Buttons, ExtCtrls, ComCtrls, StdCtrls, Menus;

type
{TNT-WARN TActionList}
  TTntActionList = class(TActionList{TNT-ALLOW TActionList})
  private
    FCheckActionsTimer: TTimer;
    procedure CheckActions(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  ITntAction = interface
    ['{59D0AE37-8161-4AD6-9102-14B28E5761EB}']
  end;

//---------------------------------------------------------------------------------------------
//                              ACTIONS
//---------------------------------------------------------------------------------------------

{TNT-WARN TCustomAction}
  TTntCustomAction = class(TCustomAction{TNT-ALLOW TCustomAction}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  public
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TAction}
  TTntAction = class(TAction{TNT-ALLOW TAction}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

//---------------------------------------------------------------------------------------------

//                             MENU ACTION LINK
//---------------------------------------------------------------------------------------------

{TNT-WARN TMenuActionLink}
  TTntMenuActionLink = class(TMenuActionLink{TNT-ALLOW TMenuActionLink})
  protected
    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    procedure SetCaption(const Value: string{TNT-ALLOW string}); override;
    procedure SetHint(const Value: string{TNT-ALLOW string}); override;
  end;

//---------------------------------------------------------------------------------------------
//                             CONTROL ACTION LINKS
//---------------------------------------------------------------------------------------------

{TNT-WARN TListViewActionLink}
  TTntListViewActionLink = class(TListViewActionLink{TNT-ALLOW TListViewActionLink})
  protected
    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    procedure SetCaption(const Value: string{TNT-ALLOW string}); override;
    procedure SetHint(const Value: string{TNT-ALLOW string}); override;
  end;

{TNT-WARN TComboBoxExActionLink}
  TTntComboBoxExActionLink = class(TComboBoxExActionLink{TNT-ALLOW TComboBoxExActionLink})
  protected
    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    procedure SetCaption(const Value: string{TNT-ALLOW string}); override;
    procedure SetHint(const Value: string{TNT-ALLOW string}); override;
  end;

{TNT-WARN TSpeedButtonActionLink}
  TTntSpeedButtonActionLink = class(TSpeedButtonActionLink{TNT-ALLOW TSpeedButtonActionLink})
  protected
    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    procedure SetCaption(const Value: string{TNT-ALLOW string}); override;
    procedure SetHint(const Value: string{TNT-ALLOW string}); override;
    {$IFDEF COMPILER_10_UP}
    function IsImageIndexLinked: Boolean; override;
    procedure SetImageIndex(Value: Integer); override;
    {$ENDIF}
  end;

{$IFDEF COMPILER_10_UP}
{TNT-WARN TBitBtnActionLink}
  TTntBitBtnActionLink = class(TBitBtnActionLink{TNT-ALLOW TBitBtnActionLink})
  protected
    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    procedure SetCaption(const Value: string{TNT-ALLOW string}); override;
    procedure SetHint(const Value: string{TNT-ALLOW string}); override;
    {$IFDEF COMPILER_10_UP}
    function IsImageIndexLinked: Boolean; override;
    procedure SetImageIndex(Value: Integer); override;
    {$ENDIF}
  end;
{$ENDIF}

{TNT-WARN TToolButtonActionLink}
  TTntToolButtonActionLink = class(TToolButtonActionLink{TNT-ALLOW TToolButtonActionLink})
  protected
    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    procedure SetCaption(const Value: string{TNT-ALLOW string}); override;
    procedure SetHint(const Value: string{TNT-ALLOW string}); override;
  end;

{TNT-WARN TButtonActionLink}
  TTntButtonActionLink = class(TButtonActionLink{TNT-ALLOW TButtonActionLink})
  protected
    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    procedure SetCaption(const Value: string{TNT-ALLOW string}); override;
    procedure SetHint(const Value: string{TNT-ALLOW string}); override;
  end;

{TNT-WARN TWinControlActionLink}
  TTntWinControlActionLink = class(TWinControlActionLink{TNT-ALLOW TWinControlActionLink})
  protected
    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    procedure SetCaption(const Value: string{TNT-ALLOW string}); override;
    procedure SetHint(const Value: string{TNT-ALLOW string}); override;
  end;

{TNT-WARN TControlActionLink}
  TTntControlActionLink = class(TControlActionLink{TNT-ALLOW TControlActionLink})
  protected
    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    procedure SetCaption(const Value: string{TNT-ALLOW string}); override;
    procedure SetHint(const Value: string{TNT-ALLOW string}); override;
  end;

//---------------------------------------------------------------------------------------------
//                             helper procs
//---------------------------------------------------------------------------------------------

//-- TCustomAction helper routines
procedure TntAction_SetCaption(Action: TCustomAction{TNT-ALLOW TCustomAction}; const Value: WideString);
function TntAction_GetCaption(Action: TCustomAction{TNT-ALLOW TCustomAction}): WideString;
function TntAction_GetNewCaption(Action: TCustomAction{TNT-ALLOW TCustomAction}; const Default: WideString): WideString;
procedure TntAction_SetHint(Action: TCustomAction{TNT-ALLOW TCustomAction}; const Value: WideString);
function TntAction_GetHint(Action: TCustomAction{TNT-ALLOW TCustomAction}): WideString;
function TntAction_GetNewHint(Action: TCustomAction{TNT-ALLOW TCustomAction}; const Default: WideString): WideString;
procedure TntAction_AfterInherited_Assign(Action: TCustomAction{TNT-ALLOW TCustomAction}; Source: TPersistent);

// -- TControl helper routines
function TntControl_GetActionLinkClass(Control: TControl; InheritedLinkClass: TControlActionLinkClass): TControlActionLinkClass;
procedure TntControl_BeforeInherited_ActionChange(Control: TControl; Sender: TObject; CheckDefaults: Boolean);

// -- TControlActionLink helper routines
function TntActionLink_IsCaptionLinked(InheritedIsCaptionLinked: Boolean; Action: TBasicAction; FClient: TControl): Boolean;
function TntActionLink_IsHintLinked(InheritedIsHintLinked: Boolean; Action: TBasicAction; FClient: TControl): Boolean;
procedure TntActionLink_SetCaption(IsCaptionLinked: Boolean; Action: TBasicAction; FClient: TControl; const Value: string{TNT-ALLOW string});
procedure TntActionLink_SetHint(IsHintLinked: Boolean; Action: TBasicAction; FClient: TControl; const Value: string{TNT-ALLOW string});

type
  TUpgradeActionListItemsProc = procedure (ActionList: TTntActionList);

var
  UpgradeActionListItemsProc: TUpgradeActionListItemsProc;

implementation

uses
  SysUtils, TntMenus, TntClasses, TntControls;

{ TActionListList }

type
  TActionListList = class(TList)
  private
    FActionList: TTntActionList;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

procedure TActionListList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if (Action = lnAdded) and (FActionList <> nil) and (Ptr <> nil)
  and (not Supports(TObject(Ptr), ITntAction)) then
  begin
    FActionList.FCheckActionsTimer.Enabled := False;
    FActionList.FCheckActionsTimer.Enabled := True;
  end;
end;

{ THackActionList }

type
{$IFDEF COMPILER_6} // verified against VCL source in Delphi 6 and BCB 6
  THackCustomActionList = class(TComponent)
  private
    FActions: TList;
  end;
{$ENDIF}
{$IFDEF DELPHI_7} // verified against VCL source in Delphi 7
  THackCustomActionList = class(TComponent)
  private
    FActions: TList;
  end;
{$ENDIF}
{$IFDEF DELPHI_9} // verified against VCL source in Delphi 9
  THackCustomActionList = class(TComponent)
  private
    FActions: TList;
  end;
{$ENDIF}
{$IFDEF DELPHI_10} // verified against VCL source in Delphi 10
  THackCustomActionList = class(TComponent)
  private
    FActions: TList;
  end;
{$ENDIF}

{ TTntActionList }

constructor TTntActionList.Create(AOwner: TComponent);
begin
  inherited;
  if (csDesigning in ComponentState) then begin
    FCheckActionsTimer := TTimer.Create(Self);
    FCheckActionsTimer.Enabled := False;
    FCheckActionsTimer.Interval := 50;
    FCheckActionsTimer.OnTimer := CheckActions;
    //
    THackCustomActionList(Self).FActions.Free;
    THackCustomActionList(Self).FActions := TActionListList.Create;
    TActionListList(THackCustomActionList(Self).FActions).FActionList := Self;
  end;
end;

procedure TTntActionList.CheckActions(Sender: TObject);
begin
  if FCheckActionsTimer <> nil then begin
    FCheckActionsTimer.Enabled := False;
  end;
  Assert(csDesigning in ComponentState);
  Assert(Assigned(UpgradeActionListItemsProc));
  UpgradeActionListItemsProc(Self);
end;

{ TCustomActionHelper }

type
  TCustomActionHelper = class(TComponent)
  private
    FAction: TCustomAction{TNT-ALLOW TCustomAction};
  private
    FCaption: WideString;
    FSettingNewCaption: Boolean;
    FOldWideCaption: WideString;
    FNewAnsiCaption: AnsiString;
    procedure SetAnsiCaption(const Value: AnsiString);
    function SettingNewCaption: Boolean;
    procedure SetCaption(const Value: WideString);
    function GetCaption: WideString;
  private
    FHint: WideString;
    FSettingNewHint: Boolean;
    FOldWideHint: WideString;
    FNewAnsiHint: AnsiString;
    procedure SetAnsiHint(const Value: AnsiString);
    function SettingNewHint: Boolean;
    procedure SetHint(const Value: WideString);
    function GetHint: WideString;
  end;

procedure TCustomActionHelper.SetAnsiCaption(const Value: AnsiString);
begin
  FAction.Caption := Value;
  if (Value = '') and (FNewAnsiCaption <> '') then
    FOldWideCaption := '';
end;

function TCustomActionHelper.SettingNewCaption: Boolean;
begin
  Result := FSettingNewCaption and (FAction.Caption <> FNewAnsiCaption);
end;

function TCustomActionHelper.GetCaption: WideString;
begin
  if SettingNewCaption then
    Result := FOldWideCaption
  else
    Result := GetSyncedWideString(FCaption, FAction.Caption)
end;

procedure TCustomActionHelper.SetCaption(const Value: WideString);
begin
  FOldWideCaption := GetCaption;
  FNewAnsiCaption := Value;
  FSettingNewCaption := True;
  try
    SetSyncedWideString(Value, FCaption, FAction.Caption, SetAnsiCaption)
  finally
    FSettingNewCaption := False;
  end;
end;

procedure TCustomActionHelper.SetAnsiHint(const Value: AnsiString);
begin
  FAction.Hint := Value;
  if (Value = '') and (FNewAnsiHint <> '') then
    FOldWideHint := '';
end;

function TCustomActionHelper.SettingNewHint: Boolean;
begin
  Result := FSettingNewHint and (FAction.Hint <> FNewAnsiHint);
end;

function TCustomActionHelper.GetHint: WideString;
begin
  if SettingNewHint then
    Result := FOldWideHint
  else
    Result := GetSyncedWideString(FHint, FAction.Hint)
end;

procedure TCustomActionHelper.SetHint(const Value: WideString);
begin
  FOldWideHint := GetHint;
  FNewAnsiHint := Value;
  FSettingNewHint := True;
  try
    SetSyncedWideString(Value, FHint, FAction.Hint, SetAnsiHint)
  finally
    FSettingNewHint := False;
  end;
end;

function FindActionHelper(Action: TCustomAction{TNT-ALLOW TCustomAction}): TCustomActionHelper;
var
  i: integer;
begin
  Assert(Action <> nil);
  Result := nil;
  if Supports(Action, ITntAction) then begin
    for i := 0 to Action.ComponentCount - 1 do begin
      if Action.Components[i] is TCustomActionHelper then begin
        Result := TCustomActionHelper(Action.Components[i]);
        break;
      end;
    end;
    if Result = nil then begin
      Result := TCustomActionHelper.Create(Action);
      Result.FAction := Action;
    end;
  end;
end;

//-- TCustomAction helper routines

procedure TntAction_SetCaption(Action: TCustomAction{TNT-ALLOW TCustomAction}; const Value: WideString);
begin
  if Supports(Action, ITntAction) then
    with FindActionHelper(Action) do
      SetCaption(Value)
  else
    Action.Caption := Value;
end;

function TntAction_GetCaption(Action: TCustomAction{TNT-ALLOW TCustomAction}): WideString;
begin
  if Supports(Action, ITntAction) then
    with FindActionHelper(Action) do
      Result := GetCaption
  else
    Result := Action.Caption;
end;

function TntAction_GetNewCaption(Action: TCustomAction{TNT-ALLOW TCustomAction}; const Default: WideString): WideString;
begin
  Result := Default;
  if Supports(Action, ITntAction) then
    with FindActionHelper(Action) do
      if SettingNewCaption then
        Result := FCaption;
end;

procedure TntAction_SetHint(Action: TCustomAction{TNT-ALLOW TCustomAction}; const Value: WideString);
begin
  if Supports(Action, ITntAction) then
    with FindActionHelper(Action) do
      SetHint(Value)
  else
    Action.Hint := Value;
end;

function TntAction_GetHint(Action: TCustomAction{TNT-ALLOW TCustomAction}): WideString;
begin
  if Supports(Action, ITntAction) then
    with FindActionHelper(Action) do
      Result := GetHint
  else
    Result := Action.Hint;
end;

function TntAction_GetNewHint(Action: TCustomAction{TNT-ALLOW TCustomAction}; const Default: WideString): WideString;
begin
  Result := Default;
  if Supports(Action, ITntAction) then
    with FindActionHelper(Action) do
      if SettingNewHint then
        Result := FHint;
end;

procedure TntAction_AfterInherited_Assign(Action: TCustomAction{TNT-ALLOW TCustomAction}; Source: TPersistent);
begin
  with Action do begin
    if (Source is TCustomAction{TNT-ALLOW TCustomAction}) then begin
      Caption := TntAction_GetCaption(Source as TCustomAction{TNT-ALLOW TCustomAction});
      Hint := TntAction_GetHint(Source as TCustomAction{TNT-ALLOW TCustomAction});
    end else if (Source is TControl) then begin
      Caption := TntControl_GetText(Source as TControl);
      Hint := TntControl_GetHint(Source as TControl);
    end;
  end;
end;

// -- TControl helper routines

function TntControl_GetActionLinkClass(Control: TControl; InheritedLinkClass: TControlActionLinkClass): TControlActionLinkClass;
begin
  if Control is TCustomListView{TNT-ALLOW TCustomListView} then
    Result := TTntListViewActionLink
  else if Control is TComboBoxEx then
    Result := TTntComboBoxExActionLink
  else if Control is TSpeedButton{TNT-ALLOW TSpeedButton} then
    Result := TTntSpeedButtonActionLink
  {$IFDEF COMPILER_10_UP}
  else if Control is TBitBtn{TNT-ALLOW TBitBtn} then
    Result := TTntBitBtnActionLink
  {$ENDIF}
  else if Control is TToolButton{TNT-ALLOW TToolButton} then
    Result := TTntToolButtonActionLink
  else if Control is TButtonControl then
    Result := TTntButtonActionLink
  else if Control is TWinControl then
    Result := TTntWinControlActionLink
  else
    Result := TTntControlActionLink;

  Assert(Result.ClassParent = InheritedLinkClass);
end;

procedure TntControl_BeforeInherited_ActionChange(Control: TControl; Sender: TObject; CheckDefaults: Boolean);
begin
  if (Sender is TCustomAction{TNT-ALLOW TCustomAction}) and Supports(Sender, ITntAction) then begin
    if not CheckDefaults or (TntControl_GetText(Control) = '') or (TntControl_GetText(Control) = Control.Name) then
      TntControl_SetText(Control, TntAction_GetCaption(TCustomAction{TNT-ALLOW TCustomAction}(Sender)));
    if not CheckDefaults or (TntControl_GetHint(Control) = '') then
      TntControl_SetHint(Control, TntAction_GetHint(TCustomAction{TNT-ALLOW TCustomAction}(Sender)));
  end;
end;

// -- TControlActionLink helper routines

function TntActionLink_IsCaptionLinked(InheritedIsCaptionLinked: Boolean; Action: TBasicAction; FClient: TControl): Boolean;
begin
  Result := InheritedIsCaptionLinked
    and (TntAction_GetCaption(Action as TCustomAction{TNT-ALLOW TCustomAction}) = TntControl_GetText(FClient));
end;

function TntActionLink_IsHintLinked(InheritedIsHintLinked: Boolean; Action: TBasicAction; FClient: TControl): Boolean;
begin
  Result := InheritedIsHintLinked
        and (TntAction_GetHint(Action as TCustomAction{TNT-ALLOW TCustomAction}) = TntControl_GetHint(FClient));
end;

procedure TntActionLink_SetCaption(IsCaptionLinked: Boolean; Action: TBasicAction; FClient: TControl; const Value: string{TNT-ALLOW string});
begin
  if IsCaptionLinked then
    TntControl_SetText(FClient, TntAction_GetNewCaption(Action as TCustomAction{TNT-ALLOW TCustomAction}, Value));
end;

procedure TntActionLink_SetHint(IsHintLinked: Boolean; Action: TBasicAction; FClient: TControl; const Value: string{TNT-ALLOW string});
begin
  if IsHintLinked then
    TntControl_SetHint(FClient, TntAction_GetNewHint(Action as TCustomAction{TNT-ALLOW TCustomAction}, Value));
end;

//---------------------------------------------------------------------------------------------
//                              ACTIONS
//---------------------------------------------------------------------------------------------

{ TTntCustomAction }

procedure TTntCustomAction.Assign(Source: TPersistent);
begin
  inherited;
  TntAction_AfterInherited_Assign(Self, Source);
end;

procedure TTntCustomAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntCustomAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntCustomAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntCustomAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntCustomAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntAction }

procedure TTntAction.Assign(Source: TPersistent);
begin
  inherited;
  TntAction_AfterInherited_Assign(Self, Source);
end;

procedure TTntAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

//---------------------------------------------------------------------------------------------
//                             MENU ACTION LINK
//---------------------------------------------------------------------------------------------

{ TTntMenuActionLink }

function TTntMenuActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked
    and WideSameCaption(TntAction_GetCaption(Action as TCustomAction{TNT-ALLOW TCustomAction}), (FClient as TTntMenuItem).Caption);
end;

function TTntMenuActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked
        and (TntAction_GetHint(Action as TCustomAction{TNT-ALLOW TCustomAction}) = (FClient as TTntMenuItem).Hint);
end;

procedure TTntMenuActionLink.SetCaption(const Value: string{TNT-ALLOW string});
begin
  if IsCaptionLinked then
    (FClient as TTntMenuItem).Caption := TntAction_GetNewCaption(Action as TCustomAction{TNT-ALLOW TCustomAction}, Value);
end;

procedure TTntMenuActionLink.SetHint(const Value: string{TNT-ALLOW string});
begin
  if IsHintLinked then
    (FClient as TTntMenuItem).Hint := TntAction_GetNewHint(Action as TCustomAction{TNT-ALLOW TCustomAction}, Value);
end;

//---------------------------------------------------------------------------------------------
//                             CONTROL ACTION LINKS
//---------------------------------------------------------------------------------------------

{ TTntListViewActionLink }

function TTntListViewActionLink.IsCaptionLinked: Boolean;
begin
  Result := TntActionLink_IsCaptionLinked(inherited IsCaptionLinked, Action, FClient);
end;

function TTntListViewActionLink.IsHintLinked: Boolean;
begin
  Result := TntActionLink_IsHintLinked(inherited IsHintLinked, Action, FClient);
end;

procedure TTntListViewActionLink.SetCaption(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetCaption(IsCaptionLinked, Action, FClient, Value);
end;

procedure TTntListViewActionLink.SetHint(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetHint(IsHintLinked, Action, FClient, Value);
end;

{ TTntComboBoxExActionLink }

function TTntComboBoxExActionLink.IsCaptionLinked: Boolean;
begin
  Result := TntActionLink_IsCaptionLinked(inherited IsCaptionLinked, Action, FClient);
end;

function TTntComboBoxExActionLink.IsHintLinked: Boolean;
begin
  Result := TntActionLink_IsHintLinked(inherited IsHintLinked, Action, FClient);
end;

procedure TTntComboBoxExActionLink.SetCaption(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetCaption(IsCaptionLinked, Action, FClient, Value);
end;

procedure TTntComboBoxExActionLink.SetHint(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetHint(IsHintLinked, Action, FClient, Value);
end;

{ TTntSpeedButtonActionLink }

function TTntSpeedButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := TntActionLink_IsCaptionLinked(inherited IsCaptionLinked, Action, FClient);
end;

function TTntSpeedButtonActionLink.IsHintLinked: Boolean;
begin
  Result := TntActionLink_IsHintLinked(inherited IsHintLinked, Action, FClient);
end;

procedure TTntSpeedButtonActionLink.SetCaption(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetCaption(IsCaptionLinked, Action, FClient, Value);
end;

procedure TTntSpeedButtonActionLink.SetHint(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetHint(IsHintLinked, Action, FClient, Value);
end;

{$IFDEF COMPILER_10_UP}
// bug fix for VCL where ImageIndex on Action ALWAYS overrides the Glyph.

function TTntSpeedButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := Action is TCustomAction{TNT-ALLOW TCustomAction}; // taken from TActionLink.IsImageIndexLinked
end;

procedure TTntSpeedButtonActionLink.SetImageIndex(Value: Integer);
begin
  ; // taken from TActionLink.IsImageIndexLinked
end;
{$ENDIF}

{$IFDEF COMPILER_10_UP}
{ TTntBitBtnActionLink }

function TTntBitBtnActionLink.IsCaptionLinked: Boolean;
begin
  Result := TntActionLink_IsCaptionLinked(inherited IsCaptionLinked, Action, FClient);
end;

function TTntBitBtnActionLink.IsHintLinked: Boolean;
begin
  Result := TntActionLink_IsHintLinked(inherited IsHintLinked, Action, FClient);
end;

procedure TTntBitBtnActionLink.SetCaption(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetCaption(IsCaptionLinked, Action, FClient, Value);
end;

procedure TTntBitBtnActionLink.SetHint(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetHint(IsHintLinked, Action, FClient, Value);
end;

{$IFDEF COMPILER_10_UP}
// bug fix for VCL where ImageIndex on Action ALWAYS overrides the Glyph.

function TTntBitBtnActionLink.IsImageIndexLinked: Boolean;
begin
  Result := Action is TCustomAction{TNT-ALLOW TCustomAction}; // taken from TActionLink.IsImageIndexLinked
end;

procedure TTntBitBtnActionLink.SetImageIndex(Value: Integer);
begin
  ; // taken from TActionLink.IsImageIndexLinked
end;
{$ENDIF}

{$ENDIF}

{ TTntToolButtonActionLink }

function TTntToolButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := TntActionLink_IsCaptionLinked(inherited IsCaptionLinked, Action, FClient);
end;

function TTntToolButtonActionLink.IsHintLinked: Boolean;
begin
  Result := TntActionLink_IsHintLinked(inherited IsHintLinked, Action, FClient);
end;

procedure TTntToolButtonActionLink.SetCaption(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetCaption(IsCaptionLinked, Action, FClient, Value);
end;

procedure TTntToolButtonActionLink.SetHint(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetHint(IsHintLinked, Action, FClient, Value);
end;

{ TTntButtonActionLink }

function TTntButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := TntActionLink_IsCaptionLinked(inherited IsCaptionLinked, Action, FClient);
end;

function TTntButtonActionLink.IsHintLinked: Boolean;
begin
  Result := TntActionLink_IsHintLinked(inherited IsHintLinked, Action, FClient);
end;

procedure TTntButtonActionLink.SetCaption(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetCaption(IsCaptionLinked, Action, FClient, Value);
end;

procedure TTntButtonActionLink.SetHint(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetHint(IsHintLinked, Action, FClient, Value);
end;

{ TTntWinControlActionLink }

function TTntWinControlActionLink.IsCaptionLinked: Boolean;
begin
  Result := TntActionLink_IsCaptionLinked(inherited IsCaptionLinked, Action, FClient);
end;

function TTntWinControlActionLink.IsHintLinked: Boolean;
begin
  Result := TntActionLink_IsHintLinked(inherited IsHintLinked, Action, FClient);
end;

procedure TTntWinControlActionLink.SetCaption(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetCaption(IsCaptionLinked, Action, FClient, Value);
end;

procedure TTntWinControlActionLink.SetHint(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetHint(IsHintLinked, Action, FClient, Value);
end;

{ TTntControlActionLink }

function TTntControlActionLink.IsCaptionLinked: Boolean;
begin
  Result := TntActionLink_IsCaptionLinked(inherited IsCaptionLinked, Action, FClient);
end;

function TTntControlActionLink.IsHintLinked: Boolean;
begin
  Result := TntActionLink_IsHintLinked(inherited IsHintLinked, Action, FClient);
end;

procedure TTntControlActionLink.SetCaption(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetCaption(IsCaptionLinked, Action, FClient, Value);
end;

procedure TTntControlActionLink.SetHint(const Value: string{TNT-ALLOW string});
begin
  TntActionLink_SetHint(IsHintLinked, Action, FClient, Value);
end;

end.
