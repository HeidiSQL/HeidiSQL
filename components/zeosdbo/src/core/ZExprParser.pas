{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Expression Parser classes and interfaces        }
{                                                         }
{         Originally written by Sergey Seroukhov          }
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

unit ZExprParser;

interface

{$I ZCore.inc}

uses SysUtils, Classes, Contnrs, ZClasses, ZCompatibility, ZVariant,
  ZTokenizer;

type
  {** Define types of expression tokens. }
  TZExpressionTokenType = (
    ttUnknown, ttLeftBrace, ttRightBrace, ttLeftSquareBrace,
    ttRightSquareBrace, ttPlus, ttMinus, ttStar, ttSlash, ttProcent, ttPower,
    ttEqual, ttNotEqual, ttMore, ttLess, ttEqualMore, ttEqualLess,
    ttAnd, ttOr, ttXor, ttIs, ttNull, ttNot, ttLike, ttNotLike, ttIsNull,
    ttIsNotNull, ttComma, ttUnary, ttFunction, ttVariable, ttConstant
  );

  {** Defines a parser exception. }
  TZParseError = class (Exception);

  {** Defines an expression token holder. }
  TZExpressionToken = class (TObject)
  private
    FTokenType: TZExpressionTokenType;
    FValue: TZVariant;
  public
    constructor Create(TokenType: TZExpressionTokenType; const Value: TZVariant);

    property TokenType: TZExpressionTokenType read FTokenType write FTokenType;
    property Value: TZVariant read FValue write FValue;
  end;

  {** Implements an expression parser class. }
  TZExpressionParser = class (TObject)
  private
    FTokenizer: IZTokenizer;
    FExpression: string;
    FInitialTokens: TObjectList;
    FTokenIndex: Integer;
    FResultTokens: TObjectList;
    FVariables: TStrings;

    function HasMoreTokens: Boolean;
    function GetToken: TZExpressionToken;
    function GetNextToken: TZExpressionToken;
    procedure ShiftToken;
    function CheckTokenTypes(
      TokenTypes: array of TZExpressionTokenType): Boolean;

    procedure TokenizeExpression;

    procedure SyntaxAnalyse;
    procedure SyntaxAnalyse1;
    procedure SyntaxAnalyse2;
    procedure SyntaxAnalyse3;
    procedure SyntaxAnalyse4;
    procedure SyntaxAnalyse5;
    procedure SyntaxAnalyse6;
  public
    constructor Create(Tokenizer: IZTokenizer);
    destructor Destroy; override;

    procedure Parse(Expression: string);
    procedure Clear;

    property Tokenizer: IZTokenizer read FTokenizer write FTokenizer;
    property Expression: string read FExpression write Parse;
    property ResultTokens: TObjectList read FResultTokens;
    property Variables: TStrings read FVariables;
  end;

implementation

uses ZSysUtils, ZMessages;

{ TZExpressionToken }

{**
  Creates an expression token object.
  @param TokenType a type of the token.
  @param Value a token value.
}
constructor TZExpressionToken.Create(TokenType: TZExpressionTokenType;
  const Value: TZVariant);
begin
  FTokenType := TokenType;
  FValue := Value;
end;

const
  {** Defines a list of operators. }
  OperatorTokens: array[0..24] of string = (
    '(', ')', '[', ']', '+', '-', '*', '/', '%', '^',
    '=', '<>', '!=', '>', '<', '>=', '<=',
    'AND', 'OR', 'XOR', 'NOT', 'IS', 'NULL', 'LIKE', ','
  );

  {** Defines a list of operator codes. }
  OperatorCodes: array[0..24] of TZExpressionTokenType = (
    ttLeftBrace, ttRightBrace, ttLeftSquareBrace, ttRightSquareBrace,
    ttPlus, ttMinus, ttStar, ttSlash, ttProcent, ttPower, ttEqual, ttNotEqual,
    ttNotEqual, ttMore, ttLess, ttEqualMore, ttEqualLess, ttAnd, ttOr, ttXor,
    ttNot, ttIs, ttNull, ttLike, ttComma
  );

{ TZExpressionParser }

{**
  Creates this expression parser object.
  @param Tokenizer an expression tokenizer.
}
constructor TZExpressionParser.Create(Tokenizer: IZTokenizer);
begin
  FTokenizer := Tokenizer;
  FExpression := '';
  FInitialTokens := TObjectList.Create;
  FTokenIndex := 0;
  FResultTokens := TObjectList.Create;
  FVariables := TStringList.Create;
end;

{**
  Destroyes this object and cleanups the memory.
}
destructor TZExpressionParser.Destroy;
begin
  inherited Destroy;
  FInitialTokens.Free;
  FResultTokens.Free;
  FVariables.Free;
end;

{**
  Clears parsing result.
}
procedure TZExpressionParser.Clear;
begin
  FExpression := '';
  FInitialTokens.Clear;
  FResultTokens.Clear;
  FTokenIndex := 0;
  FVariables.Clear;
end;

{**
  Sets a new expression string and parses it into internal byte code.
  @param expression a new expression string.
}
procedure TZExpressionParser.Parse(Expression: string);
begin
  Clear;
  FExpression := Trim(Expression);
  if FExpression <> '' then
  begin
    TokenizeExpression;
    SyntaxAnalyse;
    if HasMoreTokens then
    begin
      raise TZParseError.Create(
        Format(SSyntaxErrorNear, [SoftVarManager.GetAsString(GetToken.Value)]));
    end;
  end;
end;

{**
  Checks are there more tokens for processing.
  @return <code>TRUE</code> if some tokens are present.
}
function TZExpressionParser.HasMoreTokens: Boolean;
begin
  Result := FTokenIndex < FInitialTokens.Count;
end;

{**
  Gets the current token object.
  @param tokens a collection of tokens.
  @returns the current token object.
}
function TZExpressionParser.GetToken: TZExpressionToken;
begin
  if FTokenIndex < FInitialTokens.Count then
    Result := TZExpressionToken(FInitialTokens[FTokenIndex])
  else Result := nil;
end;

{**
  Gets the next token object.
  @param tokens a collection of tokens.
  @returns the next token object.
}
function TZExpressionParser.GetNextToken: TZExpressionToken;
begin
  if (FTokenIndex + 1) < FInitialTokens.Count then
    Result := TZExpressionToken(FInitialTokens[FTokenIndex + 1])
  else Result := nil;
end;

{**
  Shifts the current token object.
}
procedure TZExpressionParser.ShiftToken;
begin
  Inc(FTokenIndex);
end;

{**
  Checks available token types with token types from the list.
  If they match it shifts the tokens.
  @param TokenTypes a list of token types to compare.
  @return <code>True</code> if token types match.
}
function TZExpressionParser.CheckTokenTypes(
  TokenTypes: array of TZExpressionTokenType): Boolean;
var
  I: Integer;
  Temp: TZExpressionToken;
begin
  Result := False;
  for I := Low(TokenTypes) to High(TokenTypes) do
  begin
    if (FTokenIndex + I) < FInitialTokens.Count then
    begin
      Temp := TZExpressionToken(FInitialTokens[FTokenIndex + I]);
      Result := Temp.TokenType = TokenTypes[I];
    end else
      Result := False;

    if not Result then
      Break;
  end;
  if Result then
    Inc(FTokenIndex, Length(TokenTypes));
end;

{**
  Tokenizes the given expression and prepares an initial tokens list.
}
procedure TZExpressionParser.TokenizeExpression;
var
  I: Integer;
  TokenIndex: Integer;
  Temp: string;
  Tokens: TStrings;
  TokenType: TZExpressionTokenType;
  TokenValue: TZVariant;
begin
  Tokens := FTokenizer.TokenizeBufferToList(FExpression,
    [toSkipWhitespaces, toSkipComments, toSkipEOF, toDecodeStrings]);
  try
    TokenIndex := 0;

    while TokenIndex < Tokens.Count do
    begin
      TokenType := ttUnknown;
      TokenValue := NullVariant;
      case TZTokenType({$IFDEF FPC}Pointer({$ENDIF}
        Tokens.Objects[TokenIndex]{$IFDEF FPC}){$ENDIF}) of
        ttKeyword:
          begin
            Temp := UpperCase(Tokens[TokenIndex]);
            if Temp = 'TRUE' then
            begin
              TokenType := ttConstant;
              DefVarManager.SetAsBoolean(TokenValue, True);
            end
            else if Temp = 'FALSE' then
            begin
              TokenType := ttConstant;
              DefVarManager.SetAsBoolean(TokenValue, False);
            end
            else
            begin
              for I := Low(OperatorTokens) to High(OperatorTokens) do
              begin
                if OperatorTokens[I] = Temp then
                begin
                  TokenType := OperatorCodes[I];
                  Break;
                end;
              end;
            end;
          end;
        ttWord:
          begin
            TokenType := ttVariable;
            Temp := Tokens[TokenIndex];
            if FVariables.IndexOf(Temp) < 0 then
              FVariables.Add(Temp);
            DefVarManager.SetAsString(TokenValue, Temp);
          end;
        ttInteger:
          begin
            TokenType := ttConstant;
            DefVarManager.SetAsInteger(TokenValue, StrToInt(Tokens[TokenIndex]));
          end;
        ttFloat:
          begin
            TokenType := ttConstant;
            DefVarManager.SetAsFloat(TokenValue, SqlStrToFloat(Tokens[TokenIndex]));
          end;
        ttQuoted:
          begin
            TokenType := ttConstant;
            DefVarManager.SetAsString(TokenValue, Tokens[TokenIndex]);
          end;
        ttSymbol:
          begin
            Temp := Tokens[TokenIndex];
            for I := Low(OperatorTokens) to High(OperatorTokens) do
            begin
              if Temp = OperatorTokens[I] then
              begin
                TokenType := OperatorCodes[I];
                Break;
              end;
            end;
          end;
      end;

      if TokenType = ttUnknown then
        raise TZParseError.Create(Format(SUnknownSymbol, [Tokens[TokenIndex]]));

      Inc(TokenIndex);
      FInitialTokens.Add(TZExpressionToken.Create(TokenType, TokenValue));
    end;
  finally
    Tokens.Free;
  end;
end;

{**
  Performs a syntax analyze at level 0.
}
procedure TZExpressionParser.SyntaxAnalyse;
var
  Token: TZExpressionToken;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  SyntaxAnalyse1;
  while HasMoreTokens do
  begin
    Token := GetToken;
    if not (Token.TokenType in [ttAnd, ttOr, ttXor]) then
      Break;
    ShiftToken;
    SyntaxAnalyse1;
    FResultTokens.Add(TZExpressionToken.Create(Token.TokenType, NullVariant));
  end;
end;

{**
  Performs a syntax analyze at level 1.
}
procedure TZExpressionParser.SyntaxAnalyse1;
var
  Token: TZExpressionToken;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  Token := GetToken;
  if Token.TokenType = ttNot then
  begin
    ShiftToken;
    SyntaxAnalyse2;
    FResultTokens.Add(TZExpressionToken.Create(Token.TokenType, NullVariant));
  end else
    SyntaxAnalyse2;
end;

{**
  Performs a syntax analyze at level 2.
}
procedure TZExpressionParser.SyntaxAnalyse2;
var
  Token: TZExpressionToken;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  SyntaxAnalyse3;
  while HasMoreTokens do
  begin
    Token := GetToken;
    if not (Token.TokenType in [ttEqual, ttNotEqual, ttMore, ttLess,
      ttEqualMore, ttEqualLess]) then
      Break;
    ShiftToken;
    SyntaxAnalyse3;
    FResultTokens.Add(TZExpressionToken.Create(Token.TokenType, NullVariant));
  end;
end;

{**
  Performs a syntax analyze at level 3.
}
procedure TZExpressionParser.SyntaxAnalyse3;
var
  Token: TZExpressionToken;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  SyntaxAnalyse4;
  while HasMoreTokens do
  begin
    Token := GetToken;
    if Token.TokenType in [ttPlus, ttMinus, ttLike] then
    begin
      ShiftToken;
      SyntaxAnalyse4;
      FResultTokens.Add(TZExpressionToken.Create(Token.TokenType, NullVariant));
    end
    else if CheckTokenTypes([ttNot, ttLike]) then
    begin
      SyntaxAnalyse4;
      FResultTokens.Add(TZExpressionToken.Create(ttNotLike, NullVariant));
    end
    else if CheckTokenTypes([ttIs, ttNull]) then
    begin
      FResultTokens.Add(TZExpressionToken.Create(ttIsNull, NullVariant));
    end
    else if CheckTokenTypes([ttIs, ttNot, ttNull]) then
    begin
      FResultTokens.Add(TZExpressionToken.Create(ttIsNotNull, NullVariant));
    end else
      Break;
  end;
end;

{**
  Performs a syntax analyze at level 4.
}
procedure TZExpressionParser.SyntaxAnalyse4;
var
  Token: TZExpressionToken;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  SyntaxAnalyse5;
  while HasMoreTokens do
  begin
    Token := GetToken;
    if not (Token.TokenType in [ttStar, ttSlash, ttProcent]) then
      Break;
    ShiftToken;
    SyntaxAnalyse5;
    FResultTokens.Add(TZExpressionToken.Create(Token.TokenType, NullVariant));
  end;
end;

{**
  Performs a syntax analyze at level 5.
}
procedure TZExpressionParser.SyntaxAnalyse5;
var
  Token: TZExpressionToken;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  SyntaxAnalyse6;
  while HasMoreTokens do
  begin
    Token := GetToken;
    if Token.TokenType <> ttPower then
      Break;
    ShiftToken;
    SyntaxAnalyse6;
    FResultTokens.Add(TZExpressionToken.Create(Token.TokenType, NullVariant));
  end;
end;

{**
  Performs a syntax analyze at level 6.
}
procedure TZExpressionParser.SyntaxAnalyse6;
var
  ParamsCount: Integer;
  Unary, Token: TZExpressionToken;
  Primitive, NextToken: TZExpressionToken;
  Temp: TZVariant;
begin
  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  Unary := GetToken;
  if Unary.TokenType = ttPlus then
  begin
    Unary := nil;
    ShiftToken;
  end
  else if Unary.TokenType = ttMinus then
  begin
    Unary.TokenType := ttUnary;
    ShiftToken;
  end else
    Unary := nil;

  if not HasMoreTokens then
    raise TZParseError.Create(SUnexpectedExprEnd);

  Primitive := GetToken;
  NextToken := GetNextToken;
  if (Primitive.TokenType = ttVariable) and (NextToken <> nil)
    and (NextToken.TokenType = ttLeftBrace) then
    Primitive.TokenType := ttFunction;

  if Primitive.TokenType in [ttConstant, ttVariable] then
  begin
    ShiftToken;
    FResultTokens.Add(TZExpressionToken.Create(
      Primitive.TokenType, Primitive.Value));
  end
  else if Primitive.TokenType = ttLeftBrace then
  begin
    ShiftToken;
    SyntaxAnalyse;
    if not HasMoreTokens then
      raise TZParseError.Create(SUnexpectedExprEnd);
    Primitive := GetToken;
    if Primitive.TokenType <> ttRightBrace then
      raise TZParseError.Create(SRightBraceExpected);
    ShiftToken;
  end
  else if Primitive.TokenType = ttFunction then
  begin
    ShiftToken;
    Token := GetToken;
    if Token.TokenType <> ttLeftBrace then
      raise TZParseError.Create(SInternalError);
    ParamsCount := 0;
    repeat
      ShiftToken;
      Token := GetToken;
      if (Token = nil) or (Token.TokenType = ttRightBrace) then
        Break;
      Inc(ParamsCount);
      SyntaxAnalyse;
      Token := GetToken;
    until (Token = nil) or (Token.TokenType <> ttComma);

    if not HasMoreTokens then
      raise TZParseError.Create(SUnexpectedExprEnd);
    if Token.TokenType <> ttRightBrace then
      raise TZParseError.Create(SRightBraceExpected);
    ShiftToken;

    DefVarManager.SetAsInteger(Temp, ParamsCount);
    FResultTokens.Add(TZExpressionToken.Create(ttConstant, Temp));
    FResultTokens.Add(TZExpressionToken.Create(Primitive.TokenType,
      Primitive.Value));
  end else
    raise TZParseError.Create(SSyntaxError);

  if Unary <> nil then
    FResultTokens.Add(TZExpressionToken.Create(Unary.TokenType, NullVariant));
end;

end.
