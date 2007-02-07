{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes for PostgreSQL         }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZPostgreSqlToken;

interface

{$I ZParseSql.inc}

uses
  Classes, ZClasses, ZTokenizer, ZGenericSqlToken, ZMySqlToken;

type

  {** Implements a PostgreSQL-specific number state object. }
  TZPostgreSQLNumberState = class (TZMySQLNumberState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a PostgreSQL-specific quote string state object. }
  TZPostgreSQLQuoteState = class (TZMySQLQuoteState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZPostgreSQLCommentState = class (TZCppCommentState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a symbol state object. }
  TZPostgreSQLSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZPostgreSQLWordState = class (TZGenericSQLWordState)
  public
    constructor Create;
  end;

  {** Implements a default tokenizer object. }
  TZPostgreSQLTokenizer = class (TZTokenizer)
  public
    constructor Create;
  end;

implementation

{ TZPostgreSQLNumberState }

{**
  Return a number token from a reader.
  @return a number token from a reader
}
function TZPostgreSQLNumberState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  TempChar: Char;
  FloatPoint: Boolean;
  LastChar: Char;

  function ReadDecDigits: string;
  begin
    Result := '';
    LastChar := #0;
    while Stream.Read(LastChar, 1) > 0 do
    begin
      if LastChar in ['0'..'9'] then
      begin
        Result := Result + LastChar;
        LastChar := #0;
      end
      else
      begin
        Stream.Seek(-1, soFromCurrent);
        Break;
      end;
    end;
  end;

begin
  FloatPoint := FirstChar = '.';
  Result.Value := FirstChar;
  Result.TokenType := ttUnknown;
  LastChar := #0;

  { Reads the first part of the number before decimal point }
  if not FloatPoint then
  begin
    Result.Value := Result.Value + ReadDecDigits;
    FloatPoint := LastChar = '.';
    if FloatPoint then
    begin
      Stream.Read(TempChar, 1);
      Result.Value := Result.Value + TempChar;
    end;
  end;

  { Reads the second part of the number after decimal point }
  if FloatPoint then
    Result.Value := Result.Value + ReadDecDigits;

  { Reads a power part of the number }
  if LastChar in ['e','E'] then
  begin
    Stream.Read(TempChar, 1);
    Result.Value := Result.Value + TempChar;
    FloatPoint := True;

    Stream.Read(TempChar, 1);
    if TempChar in ['0'..'9','-','+'] then
      Result.Value := Result.Value + TempChar + ReadDecDigits
    else
    begin
      Result.Value := Copy(Result.Value, 1, Length(Result.Value) - 1);
      Stream.Seek(-2, soFromCurrent);
    end;
  end;

  { Prepare the result }
  if Result.Value = '.' then
  begin
    if Tokenizer.SymbolState <> nil then
      Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer);
  end
  else
  begin
    if FloatPoint then
      Result.TokenType := ttFloat
    else Result.TokenType := ttInteger;
  end;
end;

{ TZPostgreSQLQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZPostgreSQLQuoteState.NextToken(Stream: TStream;
  FirstChar: Char; Tokenizer: TZTokenizer): TZToken;
begin
  Result := inherited NextToken(Stream, FirstChar, Tokenizer);
  if FirstChar = '"' then
    Result.TokenType := ttWord;
end;

{ TZPostgreSQLCommentState }

{**
  Gets a MySQL specific comments like -- or /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZPostgreSQLCommentState.NextToken(Stream: TStream;
  FirstChar: Char; Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  ReadNum: Integer;
begin
  Result.TokenType := ttUnknown;
  Result.Value := FirstChar;

  if FirstChar = '-' then
  begin
    ReadNum := Stream.Read(ReadChar, 1);
    if (ReadNum > 0) and (ReadChar = '-') then
    begin
      Result.TokenType := ttComment;
      Result.Value := '--' + GetSingleLineComment(Stream);
    end
    else
    begin
      if ReadNum > 0 then
        Stream.Seek(-1, soFromCurrent);
    end;
  end
  else if FirstChar = '/' then
  begin
    ReadNum := Stream.Read(ReadChar, 1);
    if (ReadNum > 0) and (ReadChar = '*') then
    begin
      Result.TokenType := ttComment;
      Result.Value := '/*' + GetMultiLineComment(Stream);
    end
    else
    begin
      if ReadNum > 0 then
        Stream.Seek(-1, soFromCurrent);
    end;
  end;

  if (Result.TokenType = ttUnknown) and (Tokenizer.SymbolState <> nil) then
    Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer);
end;

{ TZPostgreSQLSymbolState }

{**
  Creates this PostgreSQL-specific symbol state object.
}
constructor TZPostgreSQLSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('<<');
  Add('>>');
end;

{ TZPostgreSQLWordState }

{**
  Constructs this PostgreSQL-specific word state object.
}
constructor TZPostgreSQLWordState.Create;
begin
  SetWordChars(#0, #255, False);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('_', '_', True);
  SetWordChars(Char($c0), Char($ff), True);
end;

{ TZPostgreSQLTokenizer }

{**
  Constructs a tokenizer with a default state table (as
  described in the class comment).
}
constructor TZPostgreSQLTokenizer.Create;
begin
  WhitespaceState := TZWhitespaceState.Create;

  SymbolState := TZPostgreSQLSymbolState.Create;
  NumberState := TZPostgreSQLNumberState.Create;
  QuoteState := TZPostgreSQLQuoteState.Create;
  WordState := TZPostgreSQLWordState.Create;
  CommentState := TZPostgreSQLCommentState.Create;

  SetCharacterState(#0, #255, SymbolState);
  SetCharacterState(#0, ' ', WhitespaceState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState(Chr($c0),  Chr($ff), WordState);
  SetCharacterState('_', '_', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState(#39, #39, QuoteState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

end.

