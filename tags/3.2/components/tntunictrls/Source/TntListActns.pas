
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntListActns;

{$INCLUDE TntCompilers.inc}

interface

uses
  Classes, TntActnList, ListActns;

type
{TNT-WARN TCustomListAction}
  TTntCustomListAction = class(TCustomListAction{TNT-ALLOW TCustomListAction}, ITntAction)
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

{TNT-WARN TStaticListAction}
  TTntStaticListAction = class(TStaticListAction{TNT-ALLOW TStaticListAction}, ITntAction)
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

{TNT-WARN TVirtualListAction}
  TTntVirtualListAction = class(TVirtualListAction{TNT-ALLOW TVirtualListAction}, ITntAction)
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

implementation

uses
  ActnList, TntClasses;

{TNT-IGNORE-UNIT}

type TAccessCustomListAction = class(TCustomListAction);

procedure TntListActn_AfterInherited_Assign(Action: TCustomAction{TNT-ALLOW TCustomAction}; Source: TPersistent);
begin
  TntAction_AfterInherited_Assign(Action, Source);
  // TCustomListAction
  if (Action is TCustomListAction) and (Source is TCustomListAction) then begin
    TAccessCustomListAction(Action).Images         := TAccessCustomListAction(Source).Images;
    TAccessCustomListAction(Action).OnGetItemCount := TAccessCustomListAction(Source).OnGetItemCount;
    TAccessCustomListAction(Action).OnItemSelected := TAccessCustomListAction(Source).OnItemSelected;
    TAccessCustomListAction(Action).Active         := TAccessCustomListAction(Source).Active;
    TAccessCustomListAction(Action).ItemIndex      := TAccessCustomListAction(Source).ItemIndex;
  end;
  // TStaticListAction
  if (Action is TStaticListAction) and (Source is TStaticListAction) then begin
    TStaticListAction(Action).Items     := TStaticListAction(Source).Items;
    TStaticListAction(Action).OnGetItem := TStaticListAction(Source).OnGetItem;
  end;
  // TVirtualListAction
  if (Action is TVirtualListAction) and (Source is TVirtualListAction) then begin
    TVirtualListAction(Action).OnGetItem := TVirtualListAction(Source).OnGetItem;
  end;
end;

//-------------------------
//    TNT LIST ACTNS
//-------------------------

{ TTntCustomListAction }

procedure TTntCustomListAction.Assign(Source: TPersistent);
begin
  inherited;
  TntListActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntCustomListAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntCustomListAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntCustomListAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntCustomListAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntCustomListAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntStaticListAction }

procedure TTntStaticListAction.Assign(Source: TPersistent);
begin
  inherited;
  TntListActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntStaticListAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntStaticListAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntStaticListAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntStaticListAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntStaticListAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntVirtualListAction }

procedure TTntVirtualListAction.Assign(Source: TPersistent);
begin
  inherited;
  TntListActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntVirtualListAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntVirtualListAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntVirtualListAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntVirtualListAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntVirtualListAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

end.
