{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            SQL Statements Analysing classes             }
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

unit ZGenericSqlAnalyser;

interface

{$I ZParseSql.inc}

uses Classes, Contnrs, ZClasses, ZTokenizer, ZSelectSchema, ZCompatibility;

type

  {** Implements a section of the parsed SQL statement. }
  TZStatementSection = class (TObject)
  private
    FName: string;
    FTokens: TStrings;
  public
    constructor Create(Name: string; Tokens: TStrings);
    destructor Destroy; override;

    function Clone: TZStatementSection;

    property Name: string read FName write FName;
    property Tokens: TStrings read FTokens;
  end;

  {** Implements a publicly available interface to statement analyser. }
  IZStatementAnalyser = interface(IZInterface)
    ['{967635B6-411B-4DEF-990C-9C6C01F3DC0A}']

    function TokenizeQuery(Tokenizer: IZTokenizer; SQL: string;
      Cleanup: Boolean): TStrings;
    function SplitSections(Tokens: TStrings): TObjectList;

    function ComposeTokens(Tokens: TStrings): string;
    function ComposeSections(Sections: TObjectList): string;

    function DefineSelectSchemaFromSections(
      Sections: TObjectList): IZSelectSchema;
    function DefineSelectSchemaFromQuery(Tokenizer: IZTokenizer;
      SQL: string): IZSelectSchema;
  end;

  {** Implements an SQL statements analyser. }
  TZGenericStatementAnalyser = class (TZAbstractObject, IZStatementAnalyser)
  private
    FSectionNames: TStrings;
    FSelectOptions: TStrings;
    FFromJoins: TStrings;
    FFromClauses: TStrings;
  protected
    function ArrayToStrings(Value: array of string): TStrings;
    function CheckForKeyword(Tokens: TStrings; TokenIndex: Integer;
      Keywords: TStrings; var Keyword: string; var WordCount: Integer): Boolean;
    function FindSectionTokens(Sections: TObjectList; Name: string): TStrings;

    procedure FillFieldRefs(SelectSchema: IZSelectSchema; SelectTokens: TStrings);
    procedure FillTableRefs(SelectSchema: IZSelectSchema; FromTokens: TStrings);

    function SkipOptionTokens(Tokens: TStrings; var TokenIndex: Integer;
      Options: TStrings): Boolean;
    function SkipBracketTokens(Tokens: TStrings; var TokenIndex: Integer):
      Boolean;

    property SectionNames: TStrings read FSectionNames write FSectionNames;
    property SelectOptions: TStrings read FSelectOptions write FSelectOptions;
    property FromJoins: TStrings read FFromJoins write FFromJoins;
    property FromClauses: TStrings read FFromClauses write FFromClauses;
  public
    constructor Create;
    destructor Destroy; override;

    function TokenizeQuery(Tokenizer: IZTokenizer; SQL: string;
      Cleanup: Boolean): TStrings;
    function SplitSections(Tokens: TStrings): TObjectList;

    function ComposeTokens(Tokens: TStrings): string;
    function ComposeSections(Sections: TObjectList): string;

    function DefineSelectSchemaFromSections(
      Sections: TObjectList): IZSelectSchema;
    function DefineSelectSchemaFromQuery(Tokenizer: IZTokenizer; SQL: string):
      IZSelectSchema;
  end;

implementation

uses SysUtils;

{ TZStatementSection }

{**
  Create SQL statement section object.
}
constructor TZStatementSection.Create(Name: string; Tokens: TStrings);
begin
  FName := Name;
  FTokens := Tokens;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZStatementSection.Destroy;
begin
  FTokens.Free;
  inherited Destroy;
end;

{**
  Clones an object instance.
  @return a clonned object instance.
}
function TZStatementSection.Clone: TZStatementSection;
var
  Temp: TStrings;
begin
  Temp := TStringList.Create;
  Temp.AddStrings(FTokens);
  Result := TZStatementSection.Create(FName, Temp);
end;

const
  {** The generic constants.}
  GenericSectionNames: array[0..12] of string = (
    'SELECT', 'UPDATE', 'DELETE', 'INSERT', 'FROM',
    'WHERE', 'INTO', 'GROUP*BY', 'HAVING', 'ORDER*BY',
    'FOR*UPDATE', 'LIMIT', 'OFFSET'
  );
  GenericSelectOptions: array[0..1] of string = (
    'DISTINCT', 'ALL'
  );
  GenericFromJoins: array[0..5] of string = (
    'NATURAL', 'RIGHT', 'LEFT', 'INNER', 'OUTER', 'JOIN'
  );
  GenericFromClauses: array[0..0] of string = (
    'ON'
  );

{ TZGenericStatementAnalyser }

{**
  Creates the object and assignes the main properties.
}
constructor TZGenericStatementAnalyser.Create;
begin
  FSectionNames := ArrayToStrings(GenericSectionNames);
  FSelectOptions := ArrayToStrings(GenericSelectOptions);
  FFromJoins := ArrayToStrings(GenericFromJoins);
  FFromClauses := ArrayToStrings(GenericFromClauses);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZGenericStatementAnalyser.Destroy;
begin
  if Assigned(FSectionNames) then
    FSectionNames.Free;
  if Assigned(FSelectOptions) then
    FSelectOptions.Free;
  if Assigned(FFromJoins) then
    FFromJoins.Free;
  if Assigned(FFromClauses) then
    FFromClauses.Free;

  inherited Destroy;
end;

{**
  Converts an array of strings into TStrings object.
  @param Value an array of strings to be converted.
  @return a TStrings object with specified strings.
}
function TZGenericStatementAnalyser.ArrayToStrings(
  Value: array of string): TStrings;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := Low(Value) to High(Value) do
    Result.Add(Value[I]);
end;

{**
  Checks for keyword with one, two or three consisted words in the list
  @param Tokens a list or tokens
  @param TokenIndex an index of the current token
  @param Keywords a list of keywords (in uppers case delimited with '*')
  @param Keyword an out parameter with found keyword.
  @param WordCount a count of words in the found keyword.
}
function TZGenericStatementAnalyser.CheckForKeyword(Tokens: TStrings;
  TokenIndex: Integer; Keywords: TStrings; var Keyword: string;
  var WordCount: Integer): Boolean;
var
  I: Integer;
begin
  WordCount := 0;
  Keyword := '';
  Result := False;

  for I := 1 to 3 do
  begin
    if (Tokens.Count <= TokenIndex) then
      Break;
    if TZTokenType({$IFDEF FPC}Pointer({$ENDIF}
      Tokens.Objects[TokenIndex]{$IFDEF FPC}){$ENDIF}) <> ttWord then
      Break;
    if Keyword <> '' then
      Keyword := Keyword + '*';
    Keyword := Keyword + AnsiUpperCase(Tokens[TokenIndex]);
    Inc(WordCount);
    if Keywords.IndexOf(Keyword) >= 0 then
    begin
      Result := True;
      Break;
    end;
    Inc(TokenIndex);
    { Skips whitespaces. }
    while Tokens.Count > TokenIndex do
    begin
      if not (TZTokenType({$IFDEF FPC}Pointer({$ENDIF}
        Tokens.Objects[TokenIndex]{$IFDEF FPC}){$ENDIF})
        in [ttWhitespace, ttComment]) then
        Break;
      Inc(TokenIndex);
      Inc(WordCount);
    end;
  end;

  if not Result then
  begin
    WordCount := 0;
    Keyword := '';
  end;
end;

{**
  Finds a section by it's name.
  @param Sections a list of sections.
  @param Name a name of the section to be found.
  @return a list of section tokens or <code>null</code>
    if section is was not found.
}
function TZGenericStatementAnalyser.FindSectionTokens(
  Sections: TObjectList; Name: string): TStrings;
var
  I: Integer;
  Current: TZStatementSection;
begin
  Result := nil;
  for I := 0 to Sections.Count - 1 do
  begin
    Current := TZStatementSection(Sections[I]);
    if Current.Name = Name then
    begin
      Result := Current.Tokens;
      Break;
    end;
  end;
end;

{**
  Tokenizes a given SQL query into a list of tokens with tokenizer.
  @param Tokenizer a tokenizer object.
  @param SQL a SQL query to be tokenized.
  @return a list with tokens.
}
function TZGenericStatementAnalyser.TokenizeQuery(
  Tokenizer: IZTokenizer; SQL: string; Cleanup: Boolean): TStrings;
begin
  if Cleanup then
  begin
    Result := Tokenizer.TokenizeBufferToList(SQL,
      [toSkipEOF, toSkipComments, toUnifyWhitespaces])
  end else
    Result := Tokenizer.TokenizeBufferToList(SQL, [toSkipEOF]);
end;

{**
  Splits a given list of tokens into the list named sections.
  @param Tokens a list of tokens.
  @return a list of section names where object property contains
    a list of tokens in the section. It initial list is not started
    with a section name the first section is unnamed ('').
}
function TZGenericStatementAnalyser.SplitSections(
  Tokens: TStrings): TObjectList;
var
  I: Integer;
  Keyword: string;
  WordCount: Integer;
  TokenIndex: Integer;
  Elements: TStrings;
  FoundSection: Boolean;
  BracketCount: Integer;
begin
  Result := TObjectList.Create;
  TokenIndex := 0;
  FoundSection := True;
  Elements := nil;
  CheckForKeyword(Tokens, TokenIndex, SectionNames, Keyword, WordCount);

  while TokenIndex < Tokens.Count do
  begin
    if FoundSection then
    begin
      Elements := TStringList.Create;
      for I := 0 to WordCount - 1 do
      begin
        Elements.AddObject(Tokens[TokenIndex + I],
          Tokens.Objects[TokenIndex + I]);
      end;
      Inc(TokenIndex, WordCount);
      Result.Add(TZStatementSection.Create(Keyword, Elements));
    end;
    FoundSection := CheckForKeyword(Tokens, TokenIndex, SectionNames,
      Keyword, WordCount);
    if not FoundSection and (TokenIndex < Tokens.Count) then
    begin
      BracketCount := 0;
      repeat
        Elements.AddObject(Tokens[TokenIndex], Tokens.Objects[TokenIndex]);
        if Tokens[TokenIndex] = '(' then
          Inc(BracketCount)
        else if Tokens[TokenIndex] = ')' then
          Dec(BracketCount);
        Inc(TokenIndex);
      until (BracketCount <= 0) or (TokenIndex >= Tokens.Count);
    end;
  end;
end;

{**
  Composes a string from the list of tokens.
  @param Tokens a list of tokens.
  @returns a composes string.
}
function TZGenericStatementAnalyser.ComposeTokens(Tokens: TStrings): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Tokens.Count - 1 do
    Result := Result + Tokens[I];
end;

{**
  Composes a string from the list of statement sections.
  @param Tokens a list of statement sections.
  @returns a composes string.
}
function TZGenericStatementAnalyser.ComposeSections(
  Sections: TObjectList): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Sections.Count - 1 do
    Result := Result + ComposeTokens(TZStatementSection(Sections[I]).Tokens);
end;

{**
  Skips tokens inside brackets.
  @param Tokens a list of tokens to scan.
  @param TokenIndex the index of the current token.
  @return <code>true</code> if some tokens were skipped.
}
function TZGenericStatementAnalyser.SkipBracketTokens(Tokens: TStrings;
  var TokenIndex: Integer): Boolean;
var
  BracketCount: Integer;
  Current: string;
begin
  { Checks for the start bracket. }
  if (TokenIndex < Tokens.Count) and (Tokens[TokenIndex] <> '(') then
  begin
    Result := False;
    Exit;
  end;

  { Skips the expression in brackets. }
  Result := True;
  BracketCount := 1;
  Inc(TokenIndex);
  while (TokenIndex < Tokens.Count) and (BracketCount > 0) do
  begin
    Current := Tokens[TokenIndex];
    if Current = '(' then
      Inc(BracketCount)
    else if Current = ')' then
      Dec(BracketCount);
    Inc(TokenIndex);
  end;
end;

{**
  Skips option tokens specified in the string list.
  @param Tokens a list of tokens to scan.
  @param TokenIndex the index of the current token.
  @param Options a list of option keyword strings in the upper case.
  @return <code>true</code> if some tokens were skipped.
}
function TZGenericStatementAnalyser.SkipOptionTokens(Tokens: TStrings;
  var TokenIndex: Integer; Options: TStrings): Boolean;
begin
  Result := False;
  while TokenIndex < Tokens.Count do
  begin
    if not (TZTokenType({$IFDEF FPC}Pointer({$ENDIF}
      Tokens.Objects[TokenIndex]{$IFDEF FPC}){$ENDIF})
      in [ttWhitespace, ttComment])
      and (Options.IndexOf(AnsiUpperCase(Tokens[TokenIndex])) < 0) then
    begin
      Break;
    end;
    Inc(TokenIndex);
    Result := True;
  end;
end;

{**
  Fills select schema with field references.
  @param SelectSchema a select schema object.
  @param SelectTokens a list of tokens in select section.
}
procedure TZGenericStatementAnalyser.FillFieldRefs(
  SelectSchema: IZSelectSchema; SelectTokens: TStrings);
var
  TokenIndex: Integer;
  Catalog: string;
  Schema: string;
  Table: string;
  Field: string;
  Alias: string;
  CurrentValue: string;
  CurrentType: TZTokenType;
  CurrentUpper: string;
  ReadField: Boolean;

  procedure ClearElements;
  begin
    Catalog := '';
    Schema := '';
    Table := '';
    Field := '';
    Alias := '';
    ReadField := True;
  end;

begin
  TokenIndex := 1;
  SkipOptionTokens(SelectTokens, TokenIndex, Self.SelectOptions);

  ClearElements;
  while TokenIndex < SelectTokens.Count do
  begin
    CurrentValue := SelectTokens[TokenIndex];
    CurrentUpper := AnsiUpperCase(CurrentValue);
    CurrentType := TZTokenType({$IFDEF FPC}Pointer({$ENDIF}
      SelectTokens.Objects[TokenIndex]{$IFDEF FPC}){$ENDIF});

    { Switches to alias part. }
    if (CurrentUpper = 'AS') or (CurrentType = ttWhitespace) then
    begin
      ReadField := ReadField and (CurrentUpper <> 'AS') and (Field = '');
    end
    { Reads field. }
    else if (ReadField = True) and ((CurrentType = ttWord) or
      (CurrentValue = '*')) then
    begin
      Catalog := Schema;
      Schema := Table;
      Table := Field;
      Field := CurrentValue;
    end
    { Skips a '.' in field part. }
    else if (ReadField = True) and (CurrentValue = '.') then
    begin
    end
    { Reads alias. }
    else if (ReadField = False) and (CurrentType = ttWord) then
    begin
      Alias := CurrentValue;
    end
    { Ends field reading. }
    else if CurrentValue = ',' then
    begin
      if Field <> '' then
      begin
        SelectSchema.AddField(TZFieldRef.Create(True, Catalog, Schema, Table,
          Field, Alias, nil));
      end;
      ClearElements;
    end
    { Skips till the next field. }
    else
    begin
      ClearElements;
      Inc(TokenIndex);
      while (TokenIndex < SelectTokens.Count) and (CurrentValue <> ',') do
      begin
        CurrentValue := SelectTokens[TokenIndex];
        if CurrentValue = '(' then
          SkipBracketTokens(SelectTokens, TokenIndex)
        else begin
          CurrentType := TZTokenType({$IFDEF FPC}Pointer({$ENDIF}
            SelectTokens.Objects[TokenIndex]{$IFDEF FPC}){$ENDIF});
          if CurrentType = ttWord then
            Alias := CurrentValue
          else if not (CurrentType in [ttWhitespace, ttComment])
            and (CurrentValue <> ',') then
            Alias := '';
          Inc(TokenIndex);
        end;
      end;
      if Alias <> '' then
      begin
        SelectSchema.AddField(TZFieldRef.Create(False, '', '', '', '',
          Alias, nil));
        ClearElements;
      end;
    end;
    Inc(TokenIndex);
  end;

  { Creates a reference to the last processed field. }
  if Field <> '' then
  begin
    SelectSchema.AddField(TZFieldRef.Create(True, Catalog, Schema, Table,
      Field, Alias, nil));
  end;
end;

{**
  Fills select schema with table references.
  @param SelectSchema a select schema object.
  @param FromTokens a list of tokens in from section.
}
procedure TZGenericStatementAnalyser.FillTableRefs(
  SelectSchema: IZSelectSchema; FromTokens: TStrings);
var
  TokenIndex: Integer;
  Catalog: string;
  Schema: string;
  Table: string;
  Alias: string;
  CurrentValue: string;
  CurrentType: TZTokenType;
  CurrentUpper: string;
  ReadTable: Boolean;

  procedure ClearElements;
  begin
    Catalog := '';
    Schema := '';
    Table := '';
    Alias := '';
    ReadTable := True;
  end;

begin
  TokenIndex := 1;

  ClearElements;
  while TokenIndex < FromTokens.Count do
  begin
    CurrentValue := FromTokens[TokenIndex];
    CurrentUpper := AnsiUpperCase(CurrentValue);
    CurrentType := TZTokenType({$IFDEF FPC}Pointer({$ENDIF}
      FromTokens.Objects[TokenIndex]{$IFDEF FPC}){$ENDIF});

    { Processes from join keywords. }
    if FromJoins.IndexOf(CurrentUpper) >= 0 then
    begin
      if Table <> '' then
        SelectSchema.AddTable(TZTableRef.Create(Catalog, Schema, Table, Alias));
      ClearElements;
      SkipOptionTokens(FromTokens, TokenIndex, FromJoins);
      Continue;
    end
    { Skips from clause keywords. }
    else if FromClauses.IndexOf(CurrentUpper) >= 0 then
    begin
      Inc(TokenIndex);
      while (TokenIndex < FromTokens.Count)
        and (FromJoins.IndexOf(CurrentUpper) < 0) and (CurrentUpper <> ',') do
      begin
        CurrentUpper := AnsiUpperCase(FromTokens[TokenIndex]);
        if CurrentUpper = '(' then
          SkipBracketTokens(FromTokens, TokenIndex)
        else Inc(TokenIndex);
      end;
    end
    { Switches to alias part. }
    else if (CurrentUpper = 'AS') or (CurrentType = ttWhitespace) then
    begin
      ReadTable := ReadTable and (CurrentUpper <> 'AS') and (Table = '');
    end
    { Reads table. }
    else if (ReadTable = True) and (CurrentType = ttWord) then
    begin
      Catalog := Schema;
      Schema := Table;
      Table := CurrentValue;
    end
    { Skips a '.' in table part. }
    else if (ReadTable = True) and (CurrentValue = '.') then
    begin
    end
    { Reads alias. }
    else if (ReadTable = False) and (CurrentType = ttWord) then
    begin
      Alias := CurrentValue;
    end
    { Ends field reading. }
    else if CurrentValue = ',' then
    begin
      if Table <> '' then
        SelectSchema.AddTable(TZTableRef.Create(Catalog, Schema, Table, Alias));
      ClearElements;
    end;
    { Skips till the next field. }
    if CurrentValue = '(' then
      SkipBracketTokens(FromTokens, TokenIndex)
    else Inc(TokenIndex);
  end;

  { Creates a reference to the last processed field. }
  if Table <> '' then
    SelectSchema.AddTable(TZTableRef.Create(Catalog, Schema, Table, Alias));
end;

{**
  Extracts a select schema from the specified parsed select statement.
  @param Sections a list of sections.
  @return a select statement schema.
}
function TZGenericStatementAnalyser.DefineSelectSchemaFromSections(
  Sections: TObjectList): IZSelectSchema;
var
  SelectTokens: TStrings;
  FromTokens: TStrings;
begin
  Result := nil;
  { Checks for the correct select statement. }
  if (Sections.Count < 2)
    or not ((TZStatementSection(Sections[0]).Name = 'SELECT')
    or ((TZStatementSection(Sections[0]).Name = '')
    and (TZStatementSection(Sections[1]).Name = 'SELECT'))) then
    Exit;

  { Defins sections. }
  SelectTokens := FindSectionTokens(Sections, 'SELECT');
  FromTokens := FindSectionTokens(Sections, 'FROM');
  if (SelectTokens = nil) or (FromTokens = nil) then
    Exit;

  { Creates and fills the result object. }
  Result := TZSelectSchema.Create;
  FillFieldRefs(Result, SelectTokens);
  FillTableRefs(Result, FromTokens);
end;

{**
  Defines a select schema from the specified SQL query.
  @param Tokenizer a tokenizer object.
  @param SQL a SQL query.
  @return a select statement schema.
}
function TZGenericStatementAnalyser.DefineSelectSchemaFromQuery(
  Tokenizer: IZTokenizer; SQL: string): IZSelectSchema;
var
  Tokens: TStrings;
  Sections: TObjectList;
begin
  Tokens := TokenizeQuery(Tokenizer, SQL, True);
  Sections := SplitSections(Tokens);
  try
    Result := DefineSelectSchemaFromSections(Sections);
  finally
    Tokens.Free;
    Sections.Free;
  end;
end;

end.

