{*********************************************************}
{                                                         }
{                     Zeos SQL Shell                      }
{                 Script Parsing Classes                  }
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

unit ZScriptParser;

interface

{$I ZParseSql.inc}

uses Classes, SysUtils, ZTokenizer;

type
  {** Defines a SQL delimiter type. }
  TZDelimiterType = (dtDefault, dtGo, dtSetTerm, dtEmptyLine);

  {** Implements a SQL script parser. }
  TZSQLScriptParser = class
  private
    FDelimiter: string;
    FDelimiterType: TZDelimiterType;
    FCleanupStatements: Boolean;
    FTokenizer: IZTokenizer;
    FUncompletedStatement: string;
    FStatements: TStrings;

    function GetStatementCount: Integer;
    function GetStatement(Index: Integer): string;

  public
    constructor Create;
    constructor CreateWithTokenizer(Tokenizer: IZTokenizer);
    destructor Destroy; override;

    procedure Clear;
    procedure ClearCompleted;
    procedure ClearUncompleted;

    procedure ParseText(Text: string);
    procedure ParseLine(Line: string);

    property Delimiter: string read FDelimiter write FDelimiter;
    property DelimiterType: TZDelimiterType read FDelimiterType
      write FDelimiterType default dtDefault;
    property CleanupStatements: Boolean read FCleanupStatements
      write FCleanupStatements default True;
    property Tokenizer: IZTokenizer read FTokenizer write FTokenizer;
    property UncompletedStatement: string read FUncompletedStatement;
    property StatementCount: Integer read GetStatementCount;
    property Statements[Index: Integer]: string read GetStatement;
  end;

implementation

uses ZMessages, ZSysUtils;

{ TZSQLScriptParser }

{**
  Constructs this script parser class.
}
constructor TZSQLScriptParser.Create;
begin
  FStatements := TStringList.Create;
  FDelimiter := ';';
  FDelimiterType := dtDefault;
  FCleanupStatements := True;
end;

{**
  Creates this object and assignes a tokenizer object.
  @param Tokenizer a tokenizer object.
}
constructor TZSQLScriptParser.CreateWithTokenizer(Tokenizer: IZTokenizer);
begin
  Create;
  FTokenizer := Tokenizer;
end;

{**
  Destroys this class and cleanups the memory.
}
destructor TZSQLScriptParser.Destroy;
begin
  FStatements.Free;
  inherited Destroy;
end;

{**
  Gets SQL statements number.
  @returns SQL statements number.
}
function TZSQLScriptParser.GetStatementCount: Integer;
begin
  Result := FStatements.Count;
end;

{**
  Gets a parsed SQL statement by it's index.
  @param Index a statement index.
  @returns a SQL statement string.
}
function TZSQLScriptParser.GetStatement(Index: Integer): string;
begin
  Result := FStatements[Index];
end;

{**
  Clears all completed and uncompleted statements and line delimiter.
}
procedure TZSQLScriptParser.Clear;
begin
  FStatements.Clear;
  FDelimiter := ';';
  FUncompletedStatement := '';
end;

{**
  Clears only completed statements.
}
procedure TZSQLScriptParser.ClearCompleted;
begin
  FStatements.Clear;
end;

{**
  Clears completed and uncompleted statements.
}
procedure TZSQLScriptParser.ClearUncompleted;
begin
  FStatements.Clear;
  FUncompletedStatement := '';
end;

{**
  Parses incrementaly only one single line.
  The line appends with EOL character.
  @param Line a line to be parsed.
}
procedure TZSQLScriptParser.ParseLine(Line: string);
begin
  ParseText(#10 + Line + #10);
end;

{**
  Parses a complete text with several lines.
  @oaram Text a text of the SQL script to be parsed.
}
procedure TZSQLScriptParser.ParseText(Text: string);
var
  Tokens: TStrings;
  TokenType: TZTokenType;
  TokenValue: string;
  TokenIndex: Integer;
  SQL, Temp: string;
  EndOfStatement: Boolean;
  Extract: Boolean;

  function CountChars(Str: string; Chr: Char): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to Length(Str) do
    begin
      if Str[I] = Chr then
        Inc(Result);
    end;
  end;

begin
  if Tokenizer = nil then
    raise Exception.Create(STokenizerIsNotDefined);

  if CleanupStatements then
    Tokens := Tokenizer.TokenizeBufferToList(Text, [toSkipComments])
  else Tokens := Tokenizer.TokenizeBufferToList(Text, []);

  TokenIndex := 0;
  SQL := FUncompletedStatement;
  if SQL <> '' then
  begin
    if CleanupStatements then
      SQL := SQL + ' '
    else SQL := SQL + #13;
  end;
  FUncompletedStatement := '';
  FStatements.Clear;

  try
    repeat
      TokenValue := Tokens[TokenIndex];
      TokenType := TZTokenType({$IFDEF FPC}Pointer({$ENDIF}
        Tokens.Objects[TokenIndex]{$IFDEF FPC}){$ENDIF});
      Inc(TokenIndex);

      case DelimiterType of
        dtDefault:
          EndOfStatement := (TokenValue = ';');
        dtGo:
          EndOfStatement := (UpperCase(TokenValue) = 'GO');
       dtEmptyLine:
          begin
            EndOfStatement := False;
            if TokenType = ttWhitespace then
            begin
              Temp := TokenValue;
              while (CountChars(Temp, #10) < 2) and (TokenType = ttWhitespace) do
              begin
                TokenValue := Tokens[TokenIndex];
                TokenType := TZTokenType({$IFDEF FPC}Pointer({$ENDIF}
                  Tokens.Objects[TokenIndex]{$IFDEF FPC}){$ENDIF});
                Inc(TokenIndex);

                if TokenType = ttWhitespace then
                  Temp := Temp + TokenValue;
              end;
              EndOfStatement := (TokenType = ttWhitespace);
              if not EndOfStatement then
              begin
                if SQL <> '' then
                  SQL := Trim(SQL) + ' ';
              end;
            end;
          end;
        dtSetTerm:
          begin
            EndOfStatement := False;
            if not (TokenType in [ttWhitespace, ttEOF]) then
            begin
              Temp := TokenValue;
              Extract := True;
              while (Length(Delimiter) > Length(Temp))
                and not (TokenType in [ttWhitespace, ttEOF]) do
              begin
                TokenValue := Tokens[TokenIndex];
                TokenType := TZTokenType({$IFDEF FPC}Pointer({$ENDIF}
                  Tokens.Objects[TokenIndex]{$IFDEF FPC}){$ENDIF});
                Inc(TokenIndex);

                if not (TokenType in [ttWhitespace, ttEOF]) then
                begin
                  Temp := Temp + TokenValue;
                  Extract := True;
                end else
                  Extract := False;
              end;
              EndOfStatement := (Delimiter = Temp);
              if not EndOfStatement then
              begin
                if Extract then
                  Temp := Copy(Temp, 1, Length(Temp) - Length(TokenValue));
                SQL := SQL + Temp;
              end;
            end;
          end;
        else
          EndOfStatement := False;
      end;

      if TokenType = ttEOF then Break;

      { Processes the end of statements. }
      if EndOfStatement then
      begin
        if CleanupStatements then
          SQL := Trim(SQL);
        if SQL <> '' then
        begin
          if not CleanupStatements then
            Temp := Trim(SQL)
          else Temp := SQL;
          if (DelimiterType = dtSetTerm)
            and StartsWith(UpperCase(Temp), 'SET TERM ') then
          begin
            Delimiter := Copy(Temp, 10, Length(Temp) - 9);
          end
          else
          begin
            if (DelimiterType = dtEmptyLine) and EndsWith(SQL, ';') then
              SQL := Copy(SQL, 1, Length(SQL) - 1);
            if CleanupStatements then
              SQL := Trim(SQL);
            FStatements.Add(SQL);
          end;
        end;
        SQL := '';
      end
      { Adds a whitespace token. }
      else if CleanupStatements and (TokenType = ttWhitespace) then
      begin
        if SQL <> '' then
          SQL := Trim(SQL) + ' ';
      end
      { Adds a default token. }
      else
      begin
        SQL := SQL + TokenValue;
      end;
    until TokenType = ttEOF;
  finally
    Tokens.Free;
  end;

  if CleanupStatements then
    SQL := Trim(SQL);
  if SQL <> '' then
    FUncompletedStatement := SQL;
end;

end.
