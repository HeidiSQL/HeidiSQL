{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       String tokenizing classes for Generic SQL         }
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

unit ZGenericSqlToken;

interface

{$I ZParseSql.inc}

uses
  Classes, SysUtils, ZClasses, ZTokenizer, ZCompatibility;

type

  {** Implements a symbol state object. }
  TZGenericSQLSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZGenericSQLWordState = class (TZWordState)
  public
    constructor Create;

    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a quote string state object. }
  TZGenericSQLQuoteState = class (TZQuoteState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;

    function EncodeString(Value: string; QuoteChar: Char): string; override;
    function DecodeString(Value: string; QuoteChar: Char): string; override;
  end;

  {** Implements a default tokenizer object. }
  TZGenericSQLTokenizer = class (TZTokenizer)
  public
    constructor Create;
  end;

implementation

{ TZGenericSQLSymbolState }

{**
  Creates this SQL-specific symbol state object.
}
constructor TZGenericSQLSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('<<');
  Add('>>');
end;

{ TZGenericSQLWordState }

{**
  Constructs this SQL-specific word state object.
}
constructor TZGenericSQLWordState.Create;
begin
  SetWordChars(#0, #255, False);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('$', '$', True);
  SetWordChars('_', '_', True);
  SetWordChars(Char($c0), Char($ff), True);
end;

const
  {** List of keywords. }
  Keywords: array [0..8] of string = (
    'AND','OR','NOT','XOR','LIKE','IS','NULL','TRUE','FALSE'
  );

{**
  Gets a word tokens or special operators.
  @return a processed token.
}
function TZGenericSQLWordState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  I: Integer;
  Temp: string;
begin
  Result := inherited NextToken(Stream, FirstChar, Tokenizer);
  Temp := UpperCase(Result.Value);

  for I := Low(Keywords) to High(Keywords) do
  begin
    if Temp = Keywords[I] then
    begin
      Result.TokenType := ttKeyword;
      Break;
    end;
  end;
end;


{ TZGenericSQLQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZGenericSQLQuoteState.NextToken(Stream: TStream;
  FirstChar: Char; Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  LastChar: Char;
begin
  Result.Value := FirstChar;
  LastChar := #0;
  while Stream.Read(ReadChar, 1) > 0 do
  begin
    if (LastChar = FirstChar) and (ReadChar <> FirstChar) then
    begin
      Stream.Seek(-1, soFromCurrent);
      Break;
    end;
    Result.Value := Result.Value + ReadChar;
    if (LastChar = FirstChar) and (ReadChar = FirstChar) then
      LastChar := #0
    else LastChar := ReadChar;
  end;

  if FirstChar = '"' then
    Result.TokenType := ttWord
  else Result.TokenType := ttQuoted;
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZGenericSQLQuoteState.EncodeString(Value: string;
  QuoteChar: Char): string;
begin
  if QuoteChar in [#39, '"', '`'] then
    Result := AnsiQuotedStr(Value, QuoteChar)
  else Result := Value;
end;

{**
  Decodes a string value.
  @param Value a string value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZGenericSQLQuoteState.DecodeString(Value: string;
  QuoteChar: Char): string;
begin
  if (Length(Value) >= 2) and (QuoteChar in [#39, '"', '`'])
    and (Value[1] = QuoteChar) and (Value[Length(Value)] = QuoteChar) then
  begin
    if Length(Value) > 2 then
      Result := AnsiDequotedStr(Value, QuoteChar)
    else Result := '';
  end
  else Result := Value;
end;

{ TZGenericSQLTokenizer }

{**
  Constructs a tokenizer with a default state table (as
  described in the class comment).
}
constructor TZGenericSQLTokenizer.Create;
begin
  NumberState := TZNumberState.Create;
  QuoteState := TZGenericSQLQuoteState.Create;
  WhitespaceState := TZWhitespaceState.Create;
  CommentState := TZCppCommentState.Create;

  SymbolState := TZGenericSQLSymbolState.Create;
  WordState := TZGenericSQLWordState.Create;

  SetCharacterState(#0, #255, SymbolState);
  SetCharacterState(#0, ' ', WhitespaceState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState(Chr($c0),  Chr($ff), WordState);
  SetCharacterState('_', '_', WordState);
  SetCharacterState('$', '$', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState(#39, #39, QuoteState);
  SetCharacterState('`', '`', QuoteState);

  SetCharacterState('/', '/', CommentState);
end;

end.

