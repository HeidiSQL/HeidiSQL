
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntDesignEditors_Design;

{$INCLUDE compilers.inc}

interface

uses
  Classes, Forms, TypInfo, DesignIntf, DesignEditors;

type
  ITntDesigner = IDesigner;

  TTntDesignerSelections = class(TInterfacedObject, IDesignerSelections)
  private
    FList: TList;
    {$IFDEF COMPILER_9_UP}
    function GetDesignObject(Index: Integer): IDesignObject;
    {$ENDIF}
  protected
    function Add(const Item: TPersistent): Integer;
    function Equals(const List: IDesignerSelections): Boolean;
    function Get(Index: Integer): TPersistent;
    function GetCount: Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPersistent read Get; default;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ReplaceSelection(const OldInst, NewInst: TPersistent);
  end;

function GetObjectInspectorForm: TCustomForm;
procedure EditPropertyWithDialog(Component: TPersistent; const PropName: AnsiString; const Designer: ITntDesigner);

implementation

uses
  SysUtils;

{ TTntDesignerSelections }

function TTntDesignerSelections.Add(const Item: TPersistent): Integer;
begin
  Result := FList.Add(Item);
end;

constructor TTntDesignerSelections.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TTntDesignerSelections.Destroy;
begin
  FList.Free;
  inherited;
end;

function TTntDesignerSelections.Equals(const List: IDesignerSelections): Boolean;
var
  I: Integer;
begin
  Result := False;
  if List.Count <> Count then Exit;
  for I := 0 to Count - 1 do
  begin
    if Items[I] <> List[I] then Exit;
  end;
  Result := True;
end;

function TTntDesignerSelections.Get(Index: Integer): TPersistent;
begin
  Result := TPersistent(FList[Index]);
end;

function TTntDesignerSelections.GetCount: Integer;
begin
  Result := FList.Count;
end;

{$IFDEF COMPILER_9_UP}
function TTntDesignerSelections.GetDesignObject(Index: Integer): IDesignObject;
begin
  Result := nil; {TODO: Figure out what IDesignerSelections.GetDesignObject is all about.  Must wait for more documentation!}
end;
{$ENDIF}

procedure TTntDesignerSelections.ReplaceSelection(const OldInst, NewInst: TPersistent);
var
  Idx: Integer;
begin
  Idx := FList.IndexOf(OldInst);
  if Idx <> -1 then
    FList[Idx] := NewInst;
end;

{//------------------------------
//  Helpful discovery routines to explore the components and classes inside the IDE...
//
procedure EnumerateComponents(Comp: TComponent);
var
  i: integer;
begin
  for i := Comp.ComponentCount - 1 downto 0 do
    MessageBoxW(0, PWideChar(WideString(Comp.Components[i].Name + ': ' + Comp.Components[i].ClassName)),
      PWideChar(WideString(Comp.Name)), 0);
end;

procedure EnumerateClasses(Comp: TComponent);
var
  AClass: TClass;
begin
  AClass := Comp.ClassType;
  repeat
    MessageBoxW(0, PWideChar(WideString(AClass.ClassName)),
      PWideChar(WideString(Comp.Name)), 0);
    AClass := Aclass.ClassParent;
  until AClass = nil;
end;
//------------------------------}

//------------------------------
function GetIdeMainForm: TCustomForm;
var
  Comp: TComponent;
begin
  Result := nil;
  if Application <> nil then begin
    Comp := Application.FindComponent('AppBuilder');
    if Comp is TCustomForm then
      Result := TCustomForm(Comp);
  end;
end;

function GetObjectInspectorForm: TCustomForm;
var
  Comp: TComponent;
  IdeMainForm: TCustomForm;
begin
  Result := nil;
  IdeMainForm := GetIdeMainForm;
  if IdeMainForm <> nil then begin
    Comp := IdeMainForm.FindComponent('PropertyInspector');
    if Comp is TCustomForm then
      Result := TCustomForm(Comp);
  end;
end;

{ TPropertyEditorWithDialog }
type
  TPropertyEditorWithDialog = class
  private
    FPropName: AnsiString;
    procedure CheckEditProperty(const Prop: IProperty);
    procedure EditProperty(Component: TPersistent; const PropName: AnsiString; const Designer: ITntDesigner);
  end;

procedure TPropertyEditorWithDialog.CheckEditProperty(const Prop: IProperty);
begin
  if Prop.GetName = FPropName then
    Prop.Edit;
end;

procedure TPropertyEditorWithDialog.EditProperty(Component: TPersistent; const PropName: AnsiString; const Designer: ITntDesigner);
var
  Components: IDesignerSelections;
begin
  FPropName := PropName;
  Components := TDesignerSelections.Create;
  Components.Add(Component);
  GetComponentProperties(Components, [tkClass], Designer, CheckEditProperty);
end;

procedure EditPropertyWithDialog(Component: TPersistent; const PropName: AnsiString; const Designer: ITntDesigner);
begin
  with TPropertyEditorWithDialog.Create do
  try
    EditProperty(Component, PropName, Designer);
  finally
    Free;
  end;
end;

end.
