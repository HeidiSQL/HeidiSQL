
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntDBActns;

{$INCLUDE compilers.inc}

interface

uses
  Classes, ActnList, DBActns, TntActnList;

type
{TNT-WARN TDataSetAction}
  TTntDataSetAction = class(TDataSetAction{TNT-ALLOW TDataSetAction}, ITntAction)
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

{TNT-WARN TDataSetFirst}
  TTntDataSetFirst = class(TDataSetFirst{TNT-ALLOW TDataSetFirst}, ITntAction)
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

{TNT-WARN TDataSetPrior}
  TTntDataSetPrior = class(TDataSetPrior{TNT-ALLOW TDataSetPrior}, ITntAction)
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

{TNT-WARN TDataSetNext}
  TTntDataSetNext = class(TDataSetNext{TNT-ALLOW TDataSetNext}, ITntAction)
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

{TNT-WARN TDataSetLast}
  TTntDataSetLast = class(TDataSetLast{TNT-ALLOW TDataSetLast}, ITntAction)
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

{TNT-WARN TDataSetInsert}
  TTntDataSetInsert = class(TDataSetInsert{TNT-ALLOW TDataSetInsert}, ITntAction)
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

{TNT-WARN TDataSetDelete}
  TTntDataSetDelete = class(TDataSetDelete{TNT-ALLOW TDataSetDelete}, ITntAction)
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

{TNT-WARN TDataSetEdit}
  TTntDataSetEdit = class(TDataSetEdit{TNT-ALLOW TDataSetEdit}, ITntAction)
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

{TNT-WARN TDataSetPost}
  TTntDataSetPost = class(TDataSetPost{TNT-ALLOW TDataSetPost}, ITntAction)
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

{TNT-WARN TDataSetCancel}
  TTntDataSetCancel = class(TDataSetCancel{TNT-ALLOW TDataSetCancel}, ITntAction)
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

{TNT-WARN TDataSetRefresh}
  TTntDataSetRefresh = class(TDataSetRefresh{TNT-ALLOW TDataSetRefresh}, ITntAction)
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

procedure TntDBActn_AfterInherited_Assign(Action: TCustomAction{TNT-ALLOW TCustomAction}; Source: TPersistent);

implementation

uses
  TntClasses;

{TNT-IGNORE-UNIT}

procedure TntDBActn_AfterInherited_Assign(Action: TCustomAction{TNT-ALLOW TCustomAction}; Source: TPersistent);
begin
  TntAction_AfterInherited_Assign(Action, Source);
  // TDataSetAction
  if (Action is TDataSetAction) and (Source is TDataSetAction) then begin
    TDataSetAction(Action).DataSource := TDataSetAction(Source).DataSource;
  end;
end;

//-------------------------
//    TNT DB ACTNS
//-------------------------

{ TTntDataSetAction }

procedure TTntDataSetAction.Assign(Source: TPersistent);
begin
  inherited;
  TntDBActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntDataSetAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDataSetAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntDataSetAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntDataSetAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntDataSetAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntDataSetFirst }

procedure TTntDataSetFirst.Assign(Source: TPersistent);
begin
  inherited;
  TntDBActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntDataSetFirst.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDataSetFirst.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntDataSetFirst.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntDataSetFirst.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntDataSetFirst.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntDataSetPrior }

procedure TTntDataSetPrior.Assign(Source: TPersistent);
begin
  inherited;
  TntDBActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntDataSetPrior.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDataSetPrior.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntDataSetPrior.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntDataSetPrior.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntDataSetPrior.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntDataSetNext }

procedure TTntDataSetNext.Assign(Source: TPersistent);
begin
  inherited;
  TntDBActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntDataSetNext.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDataSetNext.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntDataSetNext.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntDataSetNext.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntDataSetNext.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntDataSetLast }

procedure TTntDataSetLast.Assign(Source: TPersistent);
begin
  inherited;
  TntDBActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntDataSetLast.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDataSetLast.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntDataSetLast.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntDataSetLast.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntDataSetLast.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntDataSetInsert }

procedure TTntDataSetInsert.Assign(Source: TPersistent);
begin
  inherited;
  TntDBActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntDataSetInsert.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDataSetInsert.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntDataSetInsert.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntDataSetInsert.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntDataSetInsert.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntDataSetDelete }

procedure TTntDataSetDelete.Assign(Source: TPersistent);
begin
  inherited;
  TntDBActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntDataSetDelete.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDataSetDelete.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntDataSetDelete.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntDataSetDelete.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntDataSetDelete.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntDataSetEdit }

procedure TTntDataSetEdit.Assign(Source: TPersistent);
begin
  inherited;
  TntDBActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntDataSetEdit.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDataSetEdit.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntDataSetEdit.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntDataSetEdit.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntDataSetEdit.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntDataSetPost }

procedure TTntDataSetPost.Assign(Source: TPersistent);
begin
  inherited;
  TntDBActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntDataSetPost.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDataSetPost.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntDataSetPost.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntDataSetPost.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntDataSetPost.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntDataSetCancel }

procedure TTntDataSetCancel.Assign(Source: TPersistent);
begin
  inherited;
  TntDBActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntDataSetCancel.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDataSetCancel.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntDataSetCancel.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntDataSetCancel.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntDataSetCancel.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntDataSetRefresh }

procedure TTntDataSetRefresh.Assign(Source: TPersistent);
begin
  inherited;
  TntDBActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntDataSetRefresh.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDataSetRefresh.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntDataSetRefresh.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntDataSetRefresh.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntDataSetRefresh.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

end.
