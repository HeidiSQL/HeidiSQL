{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        SQL Select Objects and Assembler classes         }
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

unit ZSelectSchema;

interface

{$I ZParseSql.inc}

uses ZClasses, Contnrs, ZCompatibility;

type

  {** Case Sensitive/Unsensitive identificator processor. }
  IZIdentifierConvertor = interface (IZInterface)
    ['{2EB07B9B-1E96-4A42-8084-6F98D9140B27}']

    function IsCaseSensitive(Value: string): Boolean;
    function IsQuoted(Value: string): Boolean;
    function Quote(Value: string): string;
    function ExtractQuote(Value: string): string;
  end;

  {** Implements a table reference assembly. }
  TZTableRef = class (TObject)
  private
    FCatalog: string;
    FSchema: string;
    FTable: string;
    FAlias: string;
  public
    constructor Create(Catalog, Schema, Table, Alias: string);
    function FullName: string;

    property Catalog: string read FCatalog write FCatalog;
    property Schema: string read FSchema write FSchema;
    property Table: string read FTable write FTable;
    property Alias: string read FAlias write FAlias;
  end;

  {** Implements a field reference assembly. }
  TZFieldRef = class (TObject)
  private
    FIsField: Boolean;
    FCatalog: string;
    FSchema: string;
    FTable: string;
    FField: string;
    FAlias: string;
    FTableRef: TZTableRef;
    FLinked: Boolean;
  public
    constructor Create(IsField: Boolean; Catalog, Schema, Table,
      Field, Alias: string; TableRef: TZTableRef);

    property IsField: Boolean read FIsField write FIsField;
    property Catalog: string read FCatalog write FCatalog;
    property Schema: string read FSchema write FSchema;
    property Table: string read FTable write FTable;
    property Field: string read FField write FField;
    property Alias: string read FAlias write FAlias;
    property TableRef: TZTableRef read FTableRef write FTableRef;
    property Linked: Boolean read FLinked write FLinked;
  end;

  {** Defines an interface to select assembly. }
  IZSelectSchema = interface (IZInterface)
    ['{3B892975-57E9-4EB7-8DB1-BDDED91E7FBC}']

    procedure AddField(FieldRef: TZFieldRef);
    procedure InsertField(Index: Integer; FieldRef: TZFieldRef);
    procedure DeleteField(FieldRef: TZFieldRef);

    procedure AddTable(TableRef: TZTableRef);

    procedure LinkReferences(Convertor: IZIdentifierConvertor);

    function FindTableByFullName(Catalog, Schema, Table: string): TZTableRef;
    function FindTableByShortName(Table: string): TZTableRef;
    function FindFieldByShortName(Field: string): TZFieldRef;

    function LinkFieldByIndexAndShortName(
      ColumnIndex: Integer; Field: string): TZFieldRef;

    function GetFieldCount: Integer;
    function GetTableCount: Integer;
    function GetField(Index: Integer): TZFieldRef;
    function GetTable(Index: Integer): TZTableRef;

    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TZFieldRef read GetField;
    property TableCount: Integer read GetTableCount;
    property Tables[Index: Integer]: TZTableRef read GetTable;
  end;

  {** Implements a select assembly. }
  TZSelectSchema = class (TZAbstractObject, IZSelectSchema)
  private
    FFields: TObjectList;
    FTables: TObjectList;

    procedure ConvertIdentifiers(Convertor: IZIdentifierConvertor);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddField(FieldRef: TZFieldRef);
    procedure InsertField(Index: Integer; FieldRef: TZFieldRef);
    procedure DeleteField(FieldRef: TZFieldRef);

    procedure AddTable(TableRef: TZTableRef);

    procedure LinkReferences(Convertor: IZIdentifierConvertor);

    function FindTableByFullName(Catalog, Schema, Table: string): TZTableRef;
    function FindTableByShortName(Table: string): TZTableRef;
    function FindFieldByShortName(Field: string): TZFieldRef;

    function LinkFieldByIndexAndShortName(
      ColumnIndex: Integer; Field: string): TZFieldRef;

    function GetFieldCount: Integer;
    function GetTableCount: Integer;
    function GetField(Index: Integer): TZFieldRef;
    function GetTable(Index: Integer): TZTableRef;

    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TZFieldRef read GetField;
    property TableCount: Integer read GetTableCount;
    property Tables[Index: Integer]: TZTableRef read GetTable;
  end;

implementation

{ TZTableRef }

{**
  Creates a table reference object.
  @param Catalog a catalog name.
  @param Schema a schema name.
  @param Table a table name.
  @param Alias a table alias.
}
constructor TZTableRef.Create(Catalog, Schema, Table, Alias: string);
begin
  FCatalog := Catalog;
  FSchema := Schema;
  FTable := Table;
  FAlias := Alias;
end;

{**
  Gets a full database table name.
  @return a full database table name.
}
function TZTableRef.FullName: string;
begin
  Result := FTable;
  if FCatalog <> '' then
    Result := FCatalog + '.' + Result;
  if FSchema <> '' then
    Result := FSchema + '.' + Result;
end;

{ TZFieldRef }

{**
  Creates a field reference object.
  @param IsField flag which separates table columns from expressions.
  @param Catalog a catalog name.
  @param Schema a schema name.
  @param Table a table name.
  @param Field a field name.
  @param Alias a field alias.
}
constructor TZFieldRef.Create(IsField: Boolean; Catalog, Schema, Table,
  Field, Alias: string; TableRef: TZTableRef);
begin
  FIsField := IsField;
  FCatalog := Catalog;
  FSchema := Schema;
  FTable := Table;
  FField := Field;
  FAlias := Alias;
  FTableRef := TableRef;
  FLinked := False;
end;

{ TZSelectSchema }

{**
  Constructs this assembly object and assignes the main properties.
}
constructor TZSelectSchema.Create;
begin
  FFields := TObjectList.Create;
  FTables := TObjectList.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZSelectSchema.Destroy;
begin
  FFields.Free;
  FTables.Free;
end;

{**
  Finds a table reference by catalog and table name.
  @param Catalog a database catalog name.
  @param Schema a database schema name.
  @param Table a database table name.
  @return a found table reference object or <code>null</code> otherwise.
}
function TZSelectSchema.FindTableByFullName(
  Catalog, Schema, Table: string): TZTableRef;
var
  I: Integer;
  Current: TZTableRef;
begin
  Result := nil;

  { Looks a table by it's full name. }
  for I := 0 to FTables.Count - 1 do
  begin
    Current := TZTableRef(FTables[I]);
    if (Current.Schema = Schema) and (Current.Table = Table) then
    begin
      Result := Current;
      Exit;
    end;
  end;

  { Looks a table by it's short name. }
  for I := 0 to FTables.Count - 1 do
  begin
    Current := TZTableRef(FTables[I]);
    if (Current.Schema = '') and (Current.Table = Table) then
    begin
      Result := Current;
      Exit;
    end;
  end;
end;

{**
  Finds a table reference by table name or table alias.
  @param Table a database table name or alias.
  @return a found table reference object or <code>null</code> otherwise.
}
function TZSelectSchema.FindTableByShortName(Table: string): TZTableRef;
var
  I: Integer;
  Current: TZTableRef;
begin
  Result := nil;

  { Looks a table by it's alias. }
  for I := 0 to FTables.Count - 1 do
  begin
    Current := TZTableRef(FTables[I]);
    if Current.Alias = Table then
    begin
      Result := Current;
      Exit;
    end;
  end;

  { Looks a table by it's name. }
  for I := 0 to FTables.Count - 1 do
  begin
    Current := TZTableRef(FTables[I]);
    if Current.Table = Table then
    begin
      Result := Current;
      Exit;
    end;
  end;
end;

{**
  Finds a field reference by field name or field alias.
  @param Field a table field name or alias.
  @return a found field reference object or <code>null</code> otherwise.
}
function TZSelectSchema.FindFieldByShortName(Field: string): TZFieldRef;
var
  I: Integer;
  Current: TZFieldRef;
begin
  Result := nil;
  if Field = '' then
    Exit;

  { Looks a field by it's alias. }
  for I := 0 to FFields.Count - 1 do
  begin
    Current := TZFieldRef(FFields[I]);
    if Current.Alias = Field then
    begin
      Result := Current;
      Exit;
    end;
  end;

  { Looks a field by it's name. }
  for I := 0 to FFields.Count - 1 do
  begin
    Current := TZFieldRef(FFields[I]);
    if Current.Field = Field then
    begin
      Result := Current;
      Exit;
    end;
  end;
end;

{**
  Links a field reference by index and/or field name or field alias.
  @param ColumnIndex an index of the column.
  @param Field a table field name or alias.
  @return a found field reference object or <code>null</code> otherwise.
}
function TZSelectSchema.LinkFieldByIndexAndShortName(
  ColumnIndex: Integer; Field: string): TZFieldRef;
var
  I: Integer;
  Current: TZFieldRef;
begin
  Result := nil;
  if Field = '' then
    Exit;

  { Looks by field index. }
  if (ColumnIndex > 0) and (ColumnIndex <= FFields.Count) then
  begin
    Current := TZFieldRef(FFields[ColumnIndex - 1]);
    if not Current.Linked
      and ((Current.Alias = Field) or (Current.Field = Field)) then
    begin
      Result := Current;
      Result.Linked := True;
      Exit;
    end;
  end;

  { Looks a field by it's alias. }
  for I := 0 to FFields.Count - 1 do
  begin
    Current := TZFieldRef(FFields[I]);
    if not Current.Linked and (Current.Alias = Field) then
    begin
      Result := Current;
      Result.Linked := True;
      Exit;
    end;
  end;

  { Looks a field by field and table aliases. }
  for I := 0 to FFields.Count - 1 do
  begin
    Current := TZFieldRef(FFields[I]);
    if not Current.Linked and Assigned(Current.TableRef)
      and (((Current.TableRef.Alias + '.' + Current.Field) = Field)
      or (((Current.TableRef.Table + '.' + Current.Field) = Field))) then
    begin
      Result := Current;
      Result.Linked := True;
      Exit;
    end;
  end;

  { Looks a field by it's name. }
  for I := 0 to FFields.Count - 1 do
  begin
    Current := TZFieldRef(FFields[I]);
    if not Current.Linked and (Current.Field = Field) then
    begin
      Result := Current;
      Result.Linked := True;
      Exit;
    end;
  end;
end;

{**
  Convert all table and field identifiers..
  @param Convertor an identifier convertor.
}
procedure TZSelectSchema.ConvertIdentifiers(Convertor: IZIdentifierConvertor);
var
  I: Integer;
begin
  if Convertor = nil then Exit;

  for I := 0 to FFields.Count - 1 do
  begin
    with TZFieldRef(FFields[I]) do
    begin
      Catalog := Convertor.ExtractQuote(Catalog);
      Schema := Convertor.ExtractQuote(Schema);
      Table := Convertor.ExtractQuote(Table);
      Field := Convertor.ExtractQuote(Field);
      Alias := Convertor.ExtractQuote(Alias);
    end;
  end;

  for I := 0 to FTables.Count - 1 do
  begin
    with TZTableRef(FTables[I]) do
    begin
      Catalog := Convertor.ExtractQuote(Catalog);
      Schema := Convertor.ExtractQuote(Schema);
      Table := Convertor.ExtractQuote(Table);
      Alias := Convertor.ExtractQuote(Alias);
    end;
  end;
end;

{**
  Links references between fields and tables.
  @param Convertor an identifier convertor.
}
procedure TZSelectSchema.LinkReferences(Convertor: IZIdentifierConvertor);
var
  I, J: Integer;
  FieldRef: TZFieldRef;
  TableRef: TZTableRef;
  TempFields: TObjectList;
begin
  ConvertIdentifiers(Convertor);
  TempFields := FFields;
  FFields := TObjectList.Create;

  try
    for I := 0 to TempFields.Count - 1 do
    begin
      FieldRef := TZFieldRef(TempFields[I]);
      TableRef := nil;

      if not FieldRef.IsField then
      begin
        FFields.Add(TZFieldRef.Create(FieldRef.IsField, FieldRef.Catalog,
          FieldRef.Schema, FieldRef.Table, FieldRef.Field, FieldRef.Alias,
          FieldRef.TableRef));
        Continue;
      end
      else if (FieldRef.Schema <> '') and (FieldRef.Table <> '') then
      begin
        TableRef := FindTableByFullName(FieldRef.Catalog, FieldRef.Schema,
          FieldRef.Table);
      end
      else if FieldRef.Table <> '' then
        TableRef := FindTableByShortName(FieldRef.Table)
      else if FieldRef.Field = '*' then
      begin
        { Add all fields from all tables. }
        for J := 0 to FTables.Count - 1 do
        begin
          with TZTableRef(FTables[J]) do
          begin
            FFields.Add(TZFieldRef.Create(True, Catalog, Schema,
              Table, '*', '', TZTableRef(FTables[J])));
          end;
        end;
        Continue;
      end;

      if TableRef <> nil then
      begin
        FFields.Add(TZFieldRef.Create(True, TableRef.Catalog, TableRef.Schema,
          TableRef.Table, FieldRef.Field, FieldRef.Alias, TableRef));
      end
      else
      begin
        FFields.Add(TZFieldRef.Create(True, FieldRef.Catalog, FieldRef.Schema,
          FieldRef.Table, FieldRef.Field, FieldRef.Alias, TableRef));
      end;
    end;
  finally
    TempFields.Free;
  end;
end;

{**
  Adds a new field to this select schema.
  @param FieldRef a field reference object.
}
procedure TZSelectSchema.AddField(FieldRef: TZFieldRef);
begin
  FFields.Add(FieldRef);
end;

{**
  Inserts a new field to this select schema.
  @param Index an index where to insert a new field reference.
  @param FieldRef a field reference object.
}
procedure TZSelectSchema.InsertField(Index: Integer; FieldRef: TZFieldRef);
begin
  FFields.Insert(Index, FieldRef);
end;

{**
  Deletes a field from this select schema.
  @param FieldRef a field reference object.
}
procedure TZSelectSchema.DeleteField(FieldRef: TZFieldRef);
begin
  FFields.Remove(FieldRef);
end;

{**
  Adds a new table to this select schema.
  @param TableRef a table reference object.
}
procedure TZSelectSchema.AddTable(TableRef: TZTableRef);
begin
  FTables.Add(TableRef);
end;

{**
  Gets a field reference by index.
  @param Index an index of the reference.
  @returns a pointer to the field reference.
}
function TZSelectSchema.GetField(Index: Integer): TZFieldRef;
begin
  Result := TZFieldRef(FFields[Index]);
end;

{**
  Gets a count of field references.
  @returns a count of field references.
}
function TZSelectSchema.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

{**
  Gets a table reference by index.
  @param Index an index of the reference.
  @returns a pointer to the table reference.
}
function TZSelectSchema.GetTable(Index: Integer): TZTableRef;
begin
  Result := TZTableRef(FTables[Index]);
end;

{**
  Gets a count of table references.
  @returns a count of table references.
}
function TZSelectSchema.GetTableCount: Integer;
begin
  Result := FTables.Count;
end;

end.

