
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntBandActn;

{$INCLUDE compilers.inc}

interface

uses
  Classes, BandActn, TntActnList;

type
{TNT-WARN TCustomizeActionBars}
  TTntCustomizeActionBars = class(TCustomizeActionBars{TNT-ALLOW TCustomizeActionBars}, ITntAction)
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

procedure TntBandActn_AfterInherited_Assign(Action: TCustomAction{TNT-ALLOW TCustomAction}; Source: TPersistent);
begin
  TntAction_AfterInherited_Assign(Action, Source);
  // TCustomizeActionBars
  if (Action is TCustomizeActionBars) and (Source is TCustomizeActionBars) then begin
    TCustomizeActionBars(Action).ActionManager := TCustomizeActionBars(Source).ActionManager;
  end;
end;

//-------------------------
//    TNT BAND ACTN
//-------------------------

{ TTntCustomizeActionBars }

procedure TTntCustomizeActionBars.Assign(Source: TPersistent);
begin
  inherited;
  TntBandActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntCustomizeActionBars.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntCustomizeActionBars.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntCustomizeActionBars.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntCustomizeActionBars.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntCustomizeActionBars.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

end.
