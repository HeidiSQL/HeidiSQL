{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Variables classes and interfaces            }
{                                                         }
{           Originally written by Sergey Seroukhov        }
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

unit ZFunctions;

interface

{$I ZCore.inc}

uses SysUtils, Classes, ZClasses, ZCollections, ZCompatibility, ZVariant,
  ZExpression;

type

  {** Implements a list of functions. }
  TZFunctionsList = class (TInterfacedObject, IZFunctionsList)
  private
    FFunctions: IZCollection;
  protected
    property Functions: IZCollection read FFunctions write FFunctions;
  public
    constructor Create;
    destructor Destroy; override;

    function GetCount: Integer;
    function GetName(Index: Integer): string;
    function GetFunction(Index: Integer): IZFunction;

    procedure Add(Func: IZFunction);
    procedure Remove(const Name: string);
    function FindByName(const Name: string): Integer;

    procedure Clear;
  end;

  {** Implements an abstract function. }
  TZAbstractFunction = class (TInterfacedObject, IZFunction)
  protected
    FName: string;

    function CheckParamsCount(Stack: TZExecutionStack;
      ExpectedCount: Integer): Integer;
  public
    function GetName: string;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; virtual; abstract;

    property Name: string read GetName;
  end;

  {** Implements a TIME function. }
  TZTimeFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a EMPTY function. }
  TZEmptyFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MIN function. }
  TZMinFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MAX function. }
  TZMaxFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a IIF function. }
  TZIIFFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SUM function. }
  TZSUMFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a E function. }
  TZEFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a PI function. }
  TZPIFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a RND function. }
  TZRndFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ABS function. }
  TZAbsFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ACOS function. }
  TZAcosFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ASIN function. }
  TZAsinFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ATAN function. }
  TZAtanFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a CEIL function. }
  TZCeilFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a FLOOR function. }
  TZFloorFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a COS function. }
  TZCosFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SIN function. }
  TZSinFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a TAN function. }
  TZTanFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a EXP function. }
  TZExpFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LOG function. }
  TZLogFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LOG10 function. }
  TZLog10Function = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ROUND function. }
  TZRoundFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SQR function. }
  TZSqrFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a UPPER function. }
  TZUpperFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LOWER function. }
  TZLowerFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a CONCAT function. }
  TZConcatFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SUBSTR function. }
  TZSubStrFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a STRPOS function. }
  TZStrPosFunction = class (TZAbstractFunction)
  public
    constructor Create;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a default function list. }
  TZDefaultFunctionsList = class (TZFunctionsList)
  public
    constructor Create;
  end;

implementation

uses Math, ZMessages;

{ TZFunctionsList }

{**
  Constructs this object.
}
constructor TZFunctionsList.Create;
begin
  FFunctions := TZCollection.Create;
end;

{**
  Destroys this object and cleanup the memory.
}
destructor TZFunctionsList.Destroy;
begin
  FFunctions := nil;
  inherited Destroy;
end;

{**
  Finds a function reference
}
function TZFunctionsList.FindByName(const Name: string): Integer;
var
  I: Integer;
  Current: IZFunction;
  UpperName: string;
begin
  Result := -1;
  UpperName := UpperCase(Name);
  for I := 0 to FFunctions.Count - 1 do
  begin
    Current := FFunctions[I] as IZFunction;
    if UpperCase(Current.Name) = UpperName then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{**
  Adds a new function to this list.
  @param Func a function reference.
}
procedure TZFunctionsList.Add(Func: IZFunction);
var
  Index: Integer;
begin
  Index := FindByName(Func.Name);
  if Index < 0 then
    FFunctions.Add(Func);
end;

{**
  Removes a reference to functoin by it's name.
  @param Name a name of the function to be removed.
}
procedure TZFunctionsList.Remove(const Name: string);
var
  Index: Integer;
begin
  Index := FindByName(Name);
  if Index >= 0 then
    FFunctions.Delete(Index);
end;

{**
  Cleans the list of registered functions.
}
procedure TZFunctionsList.Clear;
begin
  FFunctions.Clear;
end;

{**
  Gets a number of registered functions.
  @returns a number of registered functions.
}
function TZFunctionsList.GetCount: Integer;
begin
  Result := FFunctions.Count;
end;

{**
  Gets a function reference by it's index.
  @param Index a function index.
  @returns a function reference.
}
function TZFunctionsList.GetFunction(Index: Integer): IZFunction;
begin
  Result := FFunctions[Index] as IZFunction;
end;

{**
  Gets a name of the functions by it's index.
  @param Index a functon index.
  @returns a name of the function.
}
function TZFunctionsList.GetName(Index: Integer): string;
begin
  Result := (FFunctions[Index] as IZFunction).Name;
end;

{ TZDefaultFunctionsList }

{**
  Constructs a default functions list and adds all available
  standard functions.
}
constructor TZDefaultFunctionsList.Create;
begin
  inherited Create;
  Functions.Add(TZTimeFunction.Create);
  Functions.Add(TZEmptyFunction.Create);
  Functions.Add(TZMinFunction.Create);
  Functions.Add(TZMaxFunction.Create);
  Functions.Add(TZIIFFunction.Create);
  Functions.Add(TZSumFunction.Create);
  Functions.Add(TZEFunction.Create);
  Functions.Add(TZPIFunction.Create);
  Functions.Add(TZRndFunction.Create);
  Functions.Add(TZAbsFunction.Create);
  Functions.Add(TZAcosFunction.Create);
  Functions.Add(TZAsinFunction.Create);
  Functions.Add(TZAtanFunction.Create);
  Functions.Add(TZCeilFunction.Create);
  Functions.Add(TZFloorFunction.Create);
  Functions.Add(TZCosFunction.Create);
  Functions.Add(TZSinFunction.Create);
  Functions.Add(TZTanFunction.Create);
  Functions.Add(TZExpFunction.Create);
  Functions.Add(TZLogFunction.Create);
  Functions.Add(TZLog10Function.Create);
  Functions.Add(TZRoundFunction.Create);
  Functions.Add(TZSqrFunction.Create);
  Functions.Add(TZUpperFunction.Create);
  Functions.Add(TZLowerFunction.Create);
  Functions.Add(TZConcatFunction.Create);
  Functions.Add(TZSubStrFunction.Create);
  Functions.Add(TZStrPosFunction.Create);
end;

{ TZAbstractFunction }

{**
  Gets the assigned function name.
  @returns the assigned function name.
}
function TZAbstractFunction.GetName: string;
begin
  Result := FName;
end;

{**
  Checks the function parameter count number.
  @param Stack a stack object.
  @param ExpectedCount a number of expected parameters.
  @returns a real number of parameters.
}
function TZAbstractFunction.CheckParamsCount(Stack: TZExecutionStack;
  ExpectedCount: Integer): Integer;
begin
  Result := DefVarManager.GetAsInteger(Stack.GetParameter(0));
  if Result <> ExpectedCount then
  begin
    raise TZExpressionError.Create(Format(SParametersError,
      [ExpectedCount, Result]));
  end;
end;

{ TZTimeFunction }

{**
  Creates this function object.
}
constructor TZTimeFunction.Create;
begin
  FName := 'TIME';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZTimeFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsDateTime(Result, Now);
end;

{ TZEmptyFunction }

{**
  Creates this function object.
}
constructor TZEmptyFunction.Create;
begin
  FName := 'EMPTY';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZEmptyFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  Value: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value := Stack.GetParameter(1);
  VariantManager.SetAsBoolean(Result, VariantManager.IsNull(Value));
end;

{ TZMinFunction }

{**
  Creates this function object.
}
constructor TZMinFunction.Create;
begin
  FName := 'MIN';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZMinFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  I, ParamsCount: Integer;
  Value: TZVariant;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if ParamsCount < 2 then
    raise TZExpressionError.Create(SExpectedMoreParams);

  Result := Stack.GetParameter(ParamsCount);
  for I := 1 to ParamsCount - 1 do
  begin
    Value := Stack.GetParameter(I);
    if VariantManager.Compare(Result, Value) > 0 then
      Result := Value;
  end;
end;

{ TZMaxFunction }

{**
  Creates this function object.
}
constructor TZMaxFunction.Create;
begin
  FName := 'MAX';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZMaxFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  I, ParamsCount: Integer;
  Value: TZVariant;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if ParamsCount < 2 then
    raise TZExpressionError.Create(SExpectedMoreParams);

  Result := Stack.GetParameter(ParamsCount);
  for I := 1 to ParamsCount - 1 do
  begin
    Value := Stack.GetParameter(I);
    if VariantManager.Compare(Result, Value) < 0 then
      Result := Value;
  end;
end;

{ TZIIFFunction }

{**
  Creates this function object.
}
constructor TZIIFFunction.Create;
begin
  FName := 'IIF';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZIIFFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 3);
  if VariantManager.GetAsBoolean(Stack.GetParameter(3)) then
    Result := Stack.GetParameter(2)
  else Result := Stack.GetParameter(1);
end;

{ TZSumFunction }

{**
  Creates this function object.
}
constructor TZSumFunction.Create;
begin
  FName := 'SUM';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZSumFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  I, ParamsCount: Integer;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if ParamsCount < 2 then
    raise TZExpressionError.Create(SExpectedMoreParams);

  Result := Stack.GetParameter(ParamsCount);
  for I := ParamsCount - 1 downto 1 do
    Result := VariantManager.OpAdd(Result, Stack.GetParameter(I));
end;

{ TZEFunction }

{**
  Creates this function object.
}
constructor TZEFunction.Create;
begin
  FName := 'E';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZEFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsFloat(Result, Exp(1));
end;

{ TZPIFunction }

{**
  Creates this function object.
}
constructor TZPIFunction.Create;
begin
  FName := 'PI';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZPIFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsFloat(Result, PI);
end;

{ TZRndFunction }

{**
  Creates this function object.
}
constructor TZRndFunction.Create;
begin
  FName := 'RND';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZRndFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsFloat(Result, Random);
end;

{ TZAbsFunction }

{**
  Creates this function object.
}
constructor TZAbsFunction.Create;
begin
  FName := 'ABS';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZAbsFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  Value: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value := Stack.GetParameter(1);
  if Value.VType = vtInteger then
    VariantManager.SetAsInteger(Result, Abs(Value.VInteger))
  else if Value.VType = vtFloat then
    VariantManager.SetAsFloat(Result, Abs(Value.VFloat))
  else
    Result := Value;
end;

{ TZAcosFunction }

{**
  Creates this function object.
}
constructor TZAcosFunction.Create;
begin
  FName := 'ACOS';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZAcosFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, ArcCos(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZAsinFunction }

{**
  Creates this function object.
}
constructor TZAsinFunction.Create;
begin
  FName := 'ASIN';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZAsinFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, ArcSin(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZAtanFunction }

{**
  Creates this function object.
}
constructor TZAtanFunction.Create;
begin
  FName := 'ATAN';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZAtanFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, ArcTan(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZCeilFunction }

{**
  Creates this function object.
}
constructor TZCeilFunction.Create;
begin
  FName := 'CEIL';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZCeilFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, Ceil(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZFloorFunction }

{**
  Creates this function object.
}
constructor TZFloorFunction.Create;
begin
  FName := 'FLOOR';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZFloorFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, Floor(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZCosFunction }

{**
  Creates this function object.
}
constructor TZCosFunction.Create;
begin
  FName := 'COS';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZCosFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Cos(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZSinFunction }

{**
  Creates this function object.
}
constructor TZSinFunction.Create;
begin
  FName := 'SIN';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZSinFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Sin(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZTanFunction }

{**
  Creates this function object.
}
constructor TZTanFunction.Create;
begin
  FName := 'TAN';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZTanFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Tan(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZExpFunction }

{**
  Creates this function object.
}
constructor TZExpFunction.Create;
begin
  FName := 'EXP';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZExpFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Exp(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZLogFunction }

{**
  Creates this function object.
}
constructor TZLogFunction.Create;
begin
  FName := 'LOG';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZLogFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Ln(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZLog10Function }

{**
  Creates this function object.
}
constructor TZLog10Function.Create;
begin
  FName := 'LOG10';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZLog10Function.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Log10(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZRoundFunction }

{**
  Creates this function object.
}
constructor TZRoundFunction.Create;
begin
  FName := 'ROUND';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZRoundFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, Round(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZSqrFunction }

{**
  Creates this function object.
}
constructor TZSqrFunction.Create;
begin
  FName := 'SQR';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZSqrFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Sqrt(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZLowerFunction }

{**
  Creates this function object.
}
constructor TZLowerFunction.Create;
begin
  FName := 'LOWER';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZLowerFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsString(Result, AnsiLowerCase(
    VariantManager.GetAsString(Stack.GetParameter(1))));
end;

{ TZUpperFunction }

{**
  Creates this function object.
}
constructor TZUpperFunction.Create;
begin
  FName := 'UPPER';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZUpperFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsString(Result, AnsiUpperCase(
    VariantManager.GetAsString(Stack.GetParameter(1))));
end;

{ TZSubStrFunction }

{**
  Creates this function object.
}
constructor TZSubStrFunction.Create;
begin
  FName := 'SUBSTR';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZSubStrFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 3);
  VariantManager.SetAsString(Result, Copy(
    VariantManager.GetAsString(Stack.GetParameter(3)),
    VariantManager.GetAsInteger(Stack.GetParameter(2)),
    VariantManager.GetAsInteger(Stack.GetParameter(1))));
end;

{ TZStrPosFunction }

{**
  Creates this function object.
}
constructor TZStrPosFunction.Create;
begin
  FName := 'STRPOS';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZStrPosFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, Pos(
    VariantManager.GetAsString(Stack.GetParameter(1)),
    VariantManager.GetAsString(Stack.GetParameter(2))));
end;

{ TZConcatFunction }

{**
  Creates this function object.
}
constructor TZConcatFunction.Create;
begin
  FName := 'CONCAT';
end;

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZConcatFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  I, ParamsCount: Integer;
  Temp: string;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if ParamsCount < 2 then
    raise TZExpressionError.Create(SExpectedMoreParams);

  Temp := VariantManager.GetAsString(Stack.GetParameter(ParamsCount));
  for I := ParamsCount - 1 downto 1 do
    Temp := Temp + VariantManager.GetAsString(Stack.GetParameter(I));
  VariantManager.SetAsString(Result, Temp);
end;

end.
