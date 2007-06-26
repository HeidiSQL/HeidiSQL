{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Variables classes and interfaces            }
{                                                         }
{            Originally written by Sergey Seroukhov       }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

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
    constructor Create(const Name: string; const Value: TZVariant);

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
    procedure SetValue(Index: Integer; const Value: TZVariant);
    function GetValueByName(const Name: string): TZVariant;
    procedure SetValueByName(const Name: string; const Value: TZVariant);

    procedure Add(const Name: string; const Value: TZVariant);
    procedure Remove(const Name: string);
    function FindByName(const Name: string): Integer;

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
constructor TZVariable.Create(const Name: string; const Value: TZVariant);
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
function TZVariablesList.FindByName(const Name: string): Integer;
var
  I: Integer;
  Current: TZVariable;
  UpperName: string;
begin
  Result := -1;
  UpperName := UpperCase(Name);
  for I := 0 to FVariables.Count - 1 do
  begin
    Current := TZVariable(FVariables[I]);
    if Current.Name = UpperName then
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
procedure TZVariablesList.Add(const Name: string; const Value: TZVariant);
begin
  if FindByName(Name) >= 0 then
    raise Exception.Create(Format(SVariableAlreadyExists, [Name]));
  FVariables.Add(TZVariable.Create(UpperCase(Name), Value));
end;

{**
  Removes a variable by specified name.
  @param Name a name of variable to be removed.
}
procedure TZVariablesList.Remove(const Name: string);
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
function TZVariablesList.GetValueByName(const Name: string): TZVariant;
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
procedure TZVariablesList.SetValue(Index: Integer; const Value: TZVariant);
begin
  TZVariable(FVariables[Index]).Value := Value;
end;

{**
  Sets a variable name by it's name.
  @param Index a variable name.
  @param Value a variable value.
}
procedure TZVariablesList.SetValueByName(const Name: string; const Value: TZVariant);
var
  Index: Integer;
begin
  Index := FindByName(Name);
  if Index >= 0 then
    TZVariable(FVariables[Index]).Value := Value
  else Add(Name, Value);
end;

end.
