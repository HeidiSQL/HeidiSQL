{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Expression classes and interfaces           }
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

unit ZExpression;

interface

{$I ZCore.inc}

uses SysUtils, Classes, ZClasses, ZCompatibility, ZVariant, ZTokenizer;

type
  {** Defines an expression exception. }
  TZExpressionError = class (Exception);

  {** Defines an execution stack object. }
  TZExecutionStack = class (TObject)
  private
    FValues: TZVariantDynArray;
    FCount: Integer;
    FCapacity: Integer;

    function GetValue(Index: Integer): TZVariant;
  public
    constructor Create;

    function Pop: TZVariant;
    function Peek: TZVariant;
    procedure Push(Value: TZVariant);
    function GetParameter(Index: Integer): TZVariant;
    procedure Swap;

    procedure Clear;

    property Count: Integer read FCount;
    property Values[Index: Integer]: TZVariant read GetValue;
  end;

  {** Defines a list of variables. }
  IZVariablesList = interface (IZInterface)
    ['{F4347F46-32F3-4021-B6DB-7A39BF171275}']

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

    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName;
    property Values[Index: Integer]: TZVariant read GetValue write SetValue;
    property NamedValues[Index: string]: TZVariant read GetValueByName
      write SetValueByName;
  end;

  {** Defines a function interface. }
  IZFunction = interface (IZInterface)
    ['{E9B3AFF9-6CD9-49C8-AB66-C8CF60ED8686}']

    function GetName: string;
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant;

    property Name: string read GetName;
  end;

  {** Defines a list of functions. }
  IZFunctionsList = interface (IZInterface)
    ['{54453054-F012-475B-84C3-7E5C46187FDB}']

    function GetCount: Integer;
    function GetName(Index: Integer): string;
    function GetFunction(Index: Integer): IZFunction;

    procedure Add(Func: IZFunction);
    procedure Remove(Name: string);
    function FindByName(Name: string): Integer;
    procedure Clear;

    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName;
    property Functions[Index: Integer]: IZFunction read GetFunction;
  end;

  {** Defines an interface to expression calculator. }
  IZExpression = interface (IZInterface)
    ['{26F9D379-5618-446C-8999-D50FBB2F8560}']

    function GetTokenizer: IZTokenizer;
    procedure SetTokenizer(Value: IZTokenizer);
    function GetExpression: string;
    procedure SetExpression(Value: string);
    function GetVariantManager: IZVariantManager;
    procedure SetVariantManager(Value: IZVariantManager);
    function GetDefaultVariables: IZVariablesList;
    procedure SetDefaultVariables(Value: IZVariablesList);
    function GetDefaultFunctions: IZFunctionsList;
    procedure SetDefaultFunctions(Value: IZFunctionsList);
    function GetAutoVariables: Boolean;
    procedure SetAutoVariables(Value: Boolean);

    function Evaluate: TZVariant;
    function Evaluate2(Variables: IZVariablesList): TZVariant;
    function Evaluate3(Variables: IZVariablesList;
      Functions: IZFunctionsList): TZVariant;
    function Evaluate4(Variables: IZVariablesList;
      Functions: IZFunctionsList; Stack: TZExecutionStack): TZVariant;

    procedure CreateVariables(Variables: IZVariablesList);
    procedure Clear;

    property Tokenizer: IZTokenizer read GetTokenizer write SetTokenizer;
    property Expression: string read GetExpression write SetExpression;
    property VariantManager: IZVariantManager read GetVariantManager
      write SetVariantManager;
    property DefaultVariables: IZVariablesList read GetDefaultVariables
      write SetDefaultVariables;
    property DefaultFunctions: IZFunctionsList read GetDefaultFunctions
      write SetDefaultFunctions;
    property AutoVariables: Boolean read GetAutoVariables
      write SetAutoVariables;
  end;

  {** Implements an expression calculator class. }
  TZExpression = class (TInterfacedObject, IZExpression)
  private
    FTokenizer: IZTokenizer;
    FDefaultVariables: IZVariablesList;
    FDefaultFunctions: IZFunctionsList;
    FVariantManager: IZVariantManager;
    FParser: TObject;
    FAutoVariables: Boolean;

    function GetTokenizer: IZTokenizer;
    procedure SetTokenizer(Value: IZTokenizer);
    function GetExpression: string;
    procedure SetExpression(Value: string);
    function GetVariantManager: IZVariantManager;
    procedure SetVariantManager(Value: IZVariantManager);
    function GetDefaultVariables: IZVariablesList;
    procedure SetDefaultVariables(Value: IZVariablesList);
    function GetDefaultFunctions: IZFunctionsList;
    procedure SetDefaultFunctions(Value: IZFunctionsList);
    function GetAutoVariables: Boolean;
    procedure SetAutoVariables(Value: Boolean);
  public
    constructor Create;
    constructor CreateWithExpression(Expression: string);
    destructor Destroy; override;

    function Evaluate: TZVariant;
    function Evaluate2(Variables: IZVariablesList): TZVariant;
    function Evaluate3(Variables: IZVariablesList;
      Functions: IZFunctionsList): TZVariant;
    function Evaluate4(Variables: IZVariablesList;
      Functions: IZFunctionsList; Stack: TZExecutionStack): TZVariant;

    procedure CreateVariables(Variables: IZVariablesList);
    procedure Clear;

    property Expression: string read GetExpression write SetExpression;
    property VariantManager: IZVariantManager read GetVariantManager
      write SetVariantManager;
    property DefaultVariables: IZVariablesList read GetDefaultVariables
      write SetDefaultVariables;
    property DefaultFunctions: IZFunctionsList read GetDefaultFunctions
      write SetDefaultFunctions;
    property AutoVariables: Boolean read GetAutoVariables
      write SetAutoVariables;
  end;

implementation

uses
  ZMessages, ZExprToken, ZExprParser, ZVariables, ZFunctions, ZMatchPattern;

{ TZExecutionStack }

{**
  Creates this object.
}
constructor TZExecutionStack.Create;
begin
  FCapacity := 100;
  SetLength(FValues, FCapacity);
  FCount := 0;
end;

{**
  Gets a value from absolute position in the stack.
  @param Index a value index.
  @returns a variant value from requested position.
}
function TZExecutionStack.GetValue(Index: Integer): TZVariant;
begin
  Result := FValues[Index];
end;

{**
  Gets a value from the top of the stack without removing it.
  @returns a value from the top.
}
function TZExecutionStack.Peek: TZVariant;
begin
  if FCount > 0 then
    Result := FValues[FCount - 1]
  else Result := NullVariant;
end;

{**
  Gets a function parameter by index.
  @param a function parameter index. O is used for parameter count.
  @returns a parameter value.
}
function TZExecutionStack.GetParameter(Index: Integer): TZVariant;
begin
  if FCount <= Index then
    raise TZExpressionError.Create(SStackIsEmpty);
  Result := FValues[FCount - Index - 1];
end;

{**
  Gets a value from the top and removes it from the stack.
  @returns a value from the top.
}
function TZExecutionStack.Pop: TZVariant;
begin
  Result := NullVariant;
  if FCount <= 0 then
    raise TZExpressionError.Create(SStackIsEmpty);
  Dec(FCount);
  Result := FValues[FCount];
end;

{**
  Puts a value to the top of the stack.
}
procedure TZExecutionStack.Push(Value: TZVariant);
begin
  if FCapacity = FCount then
  begin
    Inc(FCapacity, 100);
    SetLength(FValues, FCapacity);
  end;
  DefVarManager.Assign(Value, FValues[FCount]);
  Inc(FCount);
end;

{**
  Swaps two values on the top of the stack.
}
procedure TZExecutionStack.Swap;
var
  Temp: TZVariant;
begin
  if FCount <= 1 then
    raise TZExpressionError.Create(SStackIsEmpty);

  Temp := FValues[FCount - 1];
  FValues[FCount - 1] := FValues[FCount - 2];
  FValues[FCount - 2] := Temp;
end;

{**
  Clears this stack.
}
procedure TZExecutionStack.Clear;
begin
  FCount := 0;
end;

{ TZExpression }

{**
  Creates this expression calculator object.
}
constructor TZExpression.Create;
begin
  FTokenizer := TZExpressionTokenizer.Create;
  FDefaultVariables := TZVariablesList.Create;
  FDefaultFunctions := TZDefaultFunctionsList.Create;
  FVariantManager := TZSoftVariantManager.Create;
  FParser := TZExpressionParser.Create(FTokenizer);
  FAutoVariables := True;
end;

{**
  Creates this expression calculator and assignes expression string.
  @param Expression an expression string.
}
constructor TZExpression.CreateWithExpression(Expression: string);
begin
  Create;
  SetExpression(Expression);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZExpression.Destroy;
begin
  FTokenizer := nil;
  FDefaultVariables := nil;
  FDefaultFunctions := nil;
  FVariantManager := nil;
  FParser.Free;

  inherited Destroy;
end;

{**
  Gets the current auto variables create flag.
  @returns the auto variables create flag.
}
function TZExpression.GetAutoVariables: Boolean;
begin
  Result := FAutoVariables;
end;

{**
  Sets a new auto variables create flag.
  @param value a new auto variables create flag.
}
procedure TZExpression.SetAutoVariables(Value: Boolean);
begin
  FAutoVariables := Value;
end;

{**
  Gets a list of default functions.
  @returns a list of default functions.
}
function TZExpression.GetDefaultFunctions: IZFunctionsList;
begin
  Result := FDefaultFunctions;
end;

{**
  Sets a new list of functions.
  @param Value a new list of functions.
}
procedure TZExpression.SetDefaultFunctions(Value: IZFunctionsList);
begin
  FDefaultFunctions := Value;
end;

{**
  Gets a list of default variables.
  @returns a list of default variables.
}
function TZExpression.GetDefaultVariables: IZVariablesList;
begin
  Result := FDefaultVariables;
end;

{**
  Sets a new list of variables.
  @param Value a new list of variables.
}
procedure TZExpression.SetDefaultVariables(Value: IZVariablesList);
begin
  FDefaultVariables := Value;
end;

{**
  Gets the current set expression string.
  @returns the current expression string.
}
function TZExpression.GetExpression: string;
begin
  Result := TZExpressionParser(FParser).Expression;
end;

{**
  Sets a new expression string.
  @param Value a new expression string.
}
procedure TZExpression.SetExpression(Value: string);
begin
  TZExpressionParser(FParser).Expression := Value;
  if FAutoVariables then
    CreateVariables(FDefaultVariables);
end;

{**
  Gets a reference to the current variant manager.
  @returns a reference to the current variant manager.
}
function TZExpression.GetVariantManager: IZVariantManager;
begin
  Result := FVariantManager;
end;

{**
  Sets a new variant manager.
  @param Value a new variant manager.
}
procedure TZExpression.SetVariantManager(Value: IZVariantManager);
begin
  FVariantManager := Value;
end;

{**
  Gets the current expression tokenizer.
  @returns the current expression tokenizer.
}
function TZExpression.GetTokenizer: IZTokenizer;
begin
  Result := FTokenizer;
end;

{**
  Sets a new expression tokenizer.
  @param Value a new expression tokenizer.
}
procedure TZExpression.SetTokenizer(Value: IZTokenizer);
begin
  FTokenizer := Value;
  TZExpressionParser(FParser).Tokenizer := Value;
end;

{**
  Clears this class from all data.
}
procedure TZExpression.Clear;
begin
  TZExpressionParser(FParser).Clear;
  FDefaultVariables.Clear;
end;

{**
  Creates an empty variables.
  @param Variables a list of variables.
}
procedure TZExpression.CreateVariables(Variables: IZVariablesList);
var
  I: Integer;
  Parser: TZExpressionParser;
  Name: string;
begin
  Parser := TZExpressionParser(FParser);
  for I := 0 to Parser.Variables.Count - 1 do
  begin
    Name := Parser.Variables[I];
    if Variables.FindByName(Name) < 0 then
      Variables.Add(Name, NullVariant);
  end;
end;

{**
  Evaluates this expression.
  @returns an evaluated expression value.
}
function TZExpression.Evaluate: TZVariant;
begin
  Result := Evaluate3(FDefaultVariables, FDefaultFunctions);
end;

{**
  Evaluates this expression.
  @param Variables a list of variables.
  @returns an evaluated expression value.
}
function TZExpression.Evaluate2(Variables: IZVariablesList): TZVariant;
begin
  Result := Evaluate3(Variables, FDefaultFunctions);
end;

{**
  Evaluates this expression.
  @param Variables a list of variables.
  @param Functions a list of functions.
  @returns an evaluated expression value.
}
function TZExpression.Evaluate3(Variables: IZVariablesList;
  Functions: IZFunctionsList): TZVariant;
var
  Stack: TZExecutionStack;
begin
  Stack := TZExecutionStack.Create;
  try
    Result := Evaluate4(Variables, Functions, Stack);
  finally
    Stack.Free;
  end;
end;

{**
  Evaluates this expression.
  @param Variables a list of variables.
  @param Functions a list of functions.
  @param Stack an execution stack.
  @returns an evaluated expression value.
}
function TZExpression.Evaluate4(Variables: IZVariablesList;
  Functions: IZFunctionsList; Stack: TZExecutionStack): TZVariant;
var
  I, Index, ParamsCount: Integer;
  Current: TZExpressionToken;
  Temp: string;
  Parser: TZExpressionParser;
  Value1, Value2: TZVariant;
begin
  Parser := TZExpressionParser(FParser);
  Stack.Clear;

  for I := 0 to Parser.ResultTokens.Count - 1 do
  begin
    Current := TZExpressionToken(Parser.ResultTokens[I]);
    case Current.TokenType of
      ttConstant:
        Stack.Push(Current.Value);
      ttVariable:
        begin
          Index := Variables.FindByName(DefVarManager.GetAsString(Current.Value));
          if Index < 0 then
          begin
            Temp := DefVarManager.GetAsString(Current.Value);
            raise TZExpressionError.Create(
              Format(SVariableWasNotFound, [Temp]));
          end;
          Stack.Push(Variables.Values[Index]);
        end;
      ttFunction:
        begin
          Index := Functions.FindByName(DefVarManager.GetAsString(Current.Value));
          if Index < 0 then
          begin
            Temp := DefVarManager.GetAsString(Current.Value);
            raise TZExpressionError.Create(
              Format(SFunctionWasNotFound, [Temp]));
          end;
          Value1 := Functions.Functions[Index].Execute(Stack,
            FVariantManager);
          ParamsCount := DefVarManager.GetAsInteger(Stack.Pop);
          while ParamsCount > 0 do
          begin
            Stack.Pop;
            Dec(ParamsCount);
          end;
          Stack.Push(Value1);
        end;
      ttAnd:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpAnd(Value1, Value2));
        end;
      ttOr:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpOr(Value1, Value2));
        end;
      ttXor:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpXor(Value1, Value2));
        end;
      ttNot:
        Stack.Push(FVariantManager.OpNot(Stack.Pop));
      ttPlus:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpAdd(Value1, Value2));
        end;
      ttMinus:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpSub(Value1, Value2));
        end;
      ttStar:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpMul(Value1, Value2));
        end;
      ttSlash:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpDiv(Value1, Value2));
        end;
      ttProcent:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpMod(Value1, Value2));
        end;
      ttEqual:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpEqual(Value1, Value2));
        end;
      ttNotEqual:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpNotEqual(Value1, Value2));
        end;
      ttMore:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpMore(Value1, Value2));
        end;
      ttLess:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpLess(Value1, Value2));
        end;
      ttEqualMore:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpMoreEqual(Value1, Value2));
        end;
      ttEqualLess:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpLessEqual(Value1, Value2));
        end;
      ttPower:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          Stack.Push(FVariantManager.OpPow(Value1, Value2));
        end;
      ttUnary:
        Stack.Push(FVariantManager.OpNegative(Stack.Pop));
      ttLike:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          FVariantManager.SetAsBoolean(Value1,
            IsMatch(FVariantManager.GetAsString(Value2),
            FVariantManager.GetAsString(Value1)));
          Stack.Push(Value1);
        end;
      ttNotLike:
        begin
          Value2 := Stack.Pop;
          Value1 := Stack.Pop;
          FVariantManager.SetAsBoolean(Value1,
            not IsMatch(FVariantManager.GetAsString(Value2),
            FVariantManager.GetAsString(Value1)));
          Stack.Push(Value1);
        end;
      ttIsNull:
        begin
          Value1 := Stack.Pop;
          FVariantManager.SetAsBoolean(Value1,
            FVariantManager.IsNull(Value1));
          Stack.Push(Value1);
        end;
      ttIsNotNull:
        begin
          Value1 := Stack.Pop;
          FVariantManager.SetAsBoolean(Value1,
            not FVariantManager.IsNull(Value1));
          Stack.Push(Value1);
        end;
      else
        raise TZExpressionError.Create(SInternalError);
    end;
  end;

  if Stack.Count <> 1 then
    raise TZExpressionError.Create(SInternalError);
  Result := Stack.Pop;
end;

end.
