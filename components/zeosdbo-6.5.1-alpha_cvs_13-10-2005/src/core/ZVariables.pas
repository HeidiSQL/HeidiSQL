{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Variables classes and interfaces            }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZVariables;

interface

{$I ZCore.inc}

uses SysUtils, Classes, Contnrs, ZClasses, ZCompatibility, ZVariant, ZExpression;

type
  {** Implements a variable holder object. }
  TZVariable = class (TObject)
  private
    FName: string;
    FValue: TZVariant;
  public
    constructor Create(Name: string; Value: TZVariant);

    property Name: string read FName write FName;
    property Value: TZVariant read FValue write FValue;
  end;

  {** Implements a variables list. }
  TZVariablesList = class (TInterfacedObject, IZVariablesList)
  private
    FVariables: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetCount: Integer;
    function GetName(Index: Integer): string;
    function GetValue(Index: Integer): TZVariant;
    procedure SetValue(Index: Integer; Value: TZVariant);
    function GetValueByName(Name: string): TZVariant;
    procedure SetValueByName(Name: string; Value: TZVariant);

    procedure Add(Name: string; Value: TZVariant);
    procedure Remove(Name: string);
    function FindByName(Name: string): Integer;

    procedure ClearValues;
    procedure Clear;
  end;

implementation

uses ZMessages;

{ TZVariable }

{**
  Creates a new instance of variable
  @param Name a variable name.
  @param Value a variable value.
}
constructor TZVariable.Create(Name: string; Value: TZVariant);
begin
  FName := Name;
  FValue := Value;
end;

{ TZVariablesList }

{**
  Creates this variable list object.
}
constructor TZVariablesList.Create;
begin
  FVariables := TObjectList.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZVariablesList.Destroy;
begin
  FVariables.Free;
  inherited Destroy;
end;

{**
  Finds a variable by specified name.
  @param Name a name of the variable.
  @returns a found variable index or <code>-1</code> otherwise.
}
function TZVariablesList.FindByName(Name: string): Integer;
var
  I: Integer;
  Current: TZVariable;
begin
  Result := -1;
  Name := UpperCase(Name);
  for I := 0 to FVariables.Count - 1 do
  begin
    Current := TZVariable(FVariables[I]);
    if Current.Name = Name then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{**
  Adds a new variable with value.
  @param Name a name of the new variable.
  @param Value a value for the new variable.
}
procedure TZVariablesList.Add(Name: string; Value: TZVariant);
begin
  if FindByName(Name) >= 0 then
    raise Exception.Create(Format(SVariableAlreadyExists, [Name]));
  FVariables.Add(TZVariable.Create(UpperCase(Name), Value));
end;

{**
  Removes a variable by specified name.
  @param Name a name of variable to be removed.
}
procedure TZVariablesList.Remove(Name: string);
var
  Index: Integer;
begin
  Index := FindByName(Name);
  if Index >= 0 then
    FVariables.Delete(Index);
end;

{**
  Clears all variables.
}
procedure TZVariablesList.Clear;
begin
  FVariables.Clear;
end;

{**
  Clears only variable values.
}
procedure TZVariablesList.ClearValues;
var
  I: Integer;
begin
  for I := 0 to FVariables.Count - 1 do
    TZVariable(FVariables[I]).Value := NullVariant;
end;

{**
  Gets a number of registered variables.
  @returns a number of all registered variables.
}
function TZVariablesList.GetCount: Integer;
begin
  Result := FVariables.Count;
end;

{**
  Gets a variable name by it's index.
  @param Index a variable index.
  @returns a variable name.
}
function TZVariablesList.GetName(Index: Integer): string;
begin
  Result := TZVariable(FVariables[Index]).Name;
end;

{**
  Gets a variable value by it's index.
  @param Index a variable index.
  @returns a variable value
}
function TZVariablesList.GetValue(Index: Integer): TZVariant;
begin
  Result := TZVariable(FVariables[Index]).Value;
end;

{**
  Gets a variable name by it's name.
  @param Name a variable name.
  @returns a variable value.
}
function TZVariablesList.GetValueByName(Name: string): TZVariant;
var
  Index: Integer;
begin
  Index := FindByName(Name);
  if Index >= 0 then
    Result := TZVariable(FVariables[Index]).Value
  else Result := NullVariant;
end;

{**
  Sets a variable name by it's index.
  @param Index a variable index.
  @param Value a variable value.
}
procedure TZVariablesList.SetValue(Index: Integer; Value: TZVariant);
begin
  TZVariable(FVariables[Index]).Value := Value;
end;

{**
  Sets a variable name by it's name.
  @param Index a variable name.
  @param Value a variable value.
}
procedure TZVariablesList.SetValueByName(Name: string; Value: TZVariant);
var
  Index: Integer;
begin
  Index := FindByName(Name);
  if Index >= 0 then
    TZVariable(FVariables[Index]).Value := Value
  else Add(Name, Value);
end;

end.
