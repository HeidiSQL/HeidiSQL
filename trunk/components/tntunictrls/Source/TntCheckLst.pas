
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntCheckLst;

{$INCLUDE compilers.inc}

interface

uses
  Classes, Messages, Windows, Controls, StdCtrls, CheckLst, 
  TntClasses, TntControls, TntStdCtrls;

type
{TNT-WARN TCheckListBox}
  TTntCheckListBox = class(TCheckListBox{TNT-ALLOW TCheckListBox}, IWideCustomListControl)
  private
    FItems: TTntStrings;
    FSaveItems: TTntStrings;
    FSaveTopIndex: Integer;
    FSaveItemIndex: Integer;
    FSaved_ItemEnabled: array of Boolean;
    FSaved_State: array of TCheckBoxState;
    FSaved_Header: array of Boolean;
    FOnData: TLBGetWideDataEvent;
    procedure SetItems(const Value: TTntStrings);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure LBGetText(var Message: TMessage); message LB_GETTEXT;
    procedure LBGetTextLen(var Message: TMessage); message LB_GETTEXTLEN;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopySelection(Destination: TCustomListControl); override;
    procedure AddItem(const Item: WideString; AObject: TObject); reintroduce; virtual;
  published
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
    property Items: TTntStrings read FItems write SetItems;
    property OnData: TLBGetWideDataEvent read FOnData write FOnData;
  end;

implementation

uses
  SysUtils, Math, TntActnList;

{ TTntCheckListBox }

constructor TTntCheckListBox.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TTntListBoxStrings.Create;
  TTntListBoxStrings(FItems).ListBox := Self;
end;

destructor TTntCheckListBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TTntCheckListBox.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'LISTBOX');
end;

procedure TTntCheckListBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TTntCheckListBox.CreateWnd;
var
  i: integer;
begin
  inherited;
  TntListBox_AfterInherited_CreateWnd(Self, FSaveItems, FItems, FSaveTopIndex, FSaveItemIndex);
  if Length(FSaved_ItemEnabled) > 0 then begin
    for i := 0 to Min(Items.Count - 1, High(FSaved_ItemEnabled)) do begin
      ItemEnabled[i] := FSaved_ItemEnabled[i];
      State[i]       := FSaved_State[i];
      Header[i]      := FSaved_Header[i];
    end;
    SetLength(FSaved_ItemEnabled, 0);
    SetLength(FSaved_State, 0);
    SetLength(FSaved_Header, 0);
  end;
end;

procedure TTntCheckListBox.DestroyWnd;
var
  i: integer;
begin
  SetLength(FSaved_ItemEnabled, Items.Count);
  SetLength(FSaved_State, Items.Count);
  SetLength(FSaved_Header, Items.Count);
  for i := 0 to Items.Count - 1 do begin
    FSaved_ItemEnabled[i] := ItemEnabled[i];
    FSaved_State[i]       := State[i];
    FSaved_Header[i]      := Header[i];
  end;
  TntListBox_BeforeInherited_DestroyWnd(Self, FSaveItems, FItems, FSaveTopIndex, FSaveItemIndex);
  inherited;
end;

procedure TTntCheckListBox.SetItems(const Value: TTntStrings);
begin
  FItems.Assign(Value);
end;

procedure TTntCheckListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  inherited;
  if not Assigned(OnDrawItem) then
    TntListBox_DrawItem_Text(Self, Items, Index, Rect);
end;

function TTntCheckListBox.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;

function TTntCheckListBox.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TTntCheckListBox.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TTntCheckListBox.AddItem(const Item: WideString; AObject: TObject);
begin
  TntListBox_AddItem(Items, Item, AObject);
end;

procedure TTntCheckListBox.CopySelection(Destination: TCustomListControl);
begin
  TntListBox_CopySelection(Self, Items, Destination);
end;

procedure TTntCheckListBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TTntCheckListBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

procedure TTntCheckListBox.LBGetText(var Message: TMessage);
begin
  if not TntCustomListBox_LBGetText(Self, OnData, Message) then
    inherited;
end;

procedure TTntCheckListBox.LBGetTextLen(var Message: TMessage);
begin
  if not TntCustomListBox_LBGetTextLen(Self, OnData, Message) then
    inherited;
end;

end.
