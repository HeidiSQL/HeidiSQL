{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            SQL Metadata Dataset component               }
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

unit ZSqlMetadata;

interface

{$I ZComponent.inc}

uses
  SysUtils, DB, Classes, ZDbcIntfs, ZAbstractRODataset;

type

  {** Defines an enumiration for SQL metadata resultsets. }
  TZMetadataType = (mdProcedures, mdProcedureColumns, mdTables, mdSchemas,
    mdCatalogs, mdTableTypes, mdColumns, mdColumnPrivileges, mdTablePrivileges,
    mdBestRowIdentifier, mdVersionColumns, mdPrimaryKeys, mdImportedKeys,
    mdExportedKeys, mdCrossReference, mdTypeInfo, mdIndexInfo, mdSequences,
    mdUserDefinedTypes);

  {**
    Abstract dataset component which works with one specified table.
  }
  TZSQLMetadata = class(TZAbstractRODataset)
  private
    FMetadataType: TZMetadataType;
    FCatalog: string;
    FSchema: string;
    FTableName: string;
    FColumnName: string;
    FProcedureName: string;
    FScope: Integer;
    FNullable: Boolean;
    FForeignCatalog: string;
    FForeignSchema: string;
    FForeignTableName: string;
    FUnique: Boolean;
    FApproximate: Boolean;
    FTypeName: string;
    FSequenceName: string;

    procedure SetMetadataType(Value: TZMetadataType);

  protected
    function CreateResultSet(SQL: string; MaxRows: Integer): IZResultSet;
      override;

  public

  published
    property MetadataType: TZMetadataType read FMetadataType
      write SetMetadataType;
    property Catalog: string read FCatalog write FCatalog;
    property Schema: string read FSchema write FSchema;
    property TableName: string read FTableName write FTableName;
    property ColumnName: string read FColumnName write FColumnName;
    property ProcedureName: string read FProcedureName write FProcedureName;
    property Scope: Integer read FScope write FScope default 0;
    property Nullable: Boolean read FNullable write FNullable default False;
    property ForeignCatalog: string read FForeignCatalog write FForeignCatalog;
    property ForeignSchema: string read FForeignSchema write FForeignSchema;
    property ForeignTableName: string read FForeignTableName
      write FForeignTableName;
    property Unique: Boolean read FUnique write FUnique default False;
    property Approximate: Boolean read FApproximate write FApproximate
      default False;
    property TypeName: string read FTypeName write FTypeName;
    property SequenceName: string read FSequenceName write FSequenceName;

    property Active;
    property MasterFields;
    property MasterSource;
    property IndexFieldNames;
  end;

implementation

{ TZSQLMetadata }

{**
  Sets a new SQL metadata type.
  @param Value a new SQL metadata type.
}
procedure TZSQLMetadata.SetMetadataType(Value: TZMetadataType);
begin
  if FMetadataType <> Value then
  begin
    Active := False;
    FMetadataType := Value;
  end;
end;

{**
  Creates a DBC resultset for the query.
  @param SQL an SQL query.
  @param MaxRows a maximum rows number (-1 for all).
  @returns a created DBC resultset.
}
function TZSQLMetadata.CreateResultSet(SQL: string; MaxRows: Integer):
  IZResultSet;
var
  Metadata: IZDatabaseMetadata;
begin
  Connection.ShowSQLHourGlass;
  try
    Metadata := Connection.DbcConnection.GetMetadata;

    case FMetadataType of
      mdProcedures:
        Result := Metadata.GetProcedures(FCatalog, FSchema, FProcedureName);
      mdProcedureColumns:
        Result := Metadata.GetProcedureColumns(FCatalog, FSchema,
          FProcedureName, FColumnName);
      mdTables:
        Result := Metadata.GetTables(FCatalog, FSchema, FTableName, nil);
      mdSchemas:
        Result := Metadata.GetSchemas;
      mdCatalogs:
        Result := Metadata.GetCatalogs;
      mdTableTypes:
        Result := Metadata.GetTableTypes;
      mdColumns:
        Result := Metadata.GetColumns(FCatalog, FSchema, FTableName,
          FColumnName);
      mdColumnPrivileges:
        Result := Metadata.GetColumnPrivileges(FCatalog, FSchema, FTableName,
          FColumnName);
      mdTablePrivileges:
        Result := Metadata.GetTablePrivileges(FCatalog, FSchema, FTableName);
      mdBestRowIdentifier:
        Result := Metadata.GetBestRowIdentifier(FCatalog, FSchema, FTableName,
          FScope, FNullable);
      mdVersionColumns:
        Result := Metadata.GetVersionColumns(FCatalog, FSchema, FTableName);
      mdPrimaryKeys:
        Result := Metadata.GetPrimaryKeys(FCatalog, FSchema, FTableName);
      mdImportedKeys:
        Result := Metadata.GetImportedKeys(FCatalog, FSchema, FTableName);
      mdExportedKeys:
        Result := Metadata.GetExportedKeys(FCatalog, FSchema, FTableName);
      mdCrossReference:
        Result := Metadata.GetCrossReference(FCatalog, FSchema, FTableName,
          FForeignCatalog, FForeignSchema, FForeignTableName);
      mdTypeInfo:
        Result := Metadata.GetTypeInfo;
      mdIndexInfo:
        Result := Metadata.GetIndexInfo(FCatalog, FSchema, FTableName, FUnique,
          FApproximate);
      mdSequences:
        Result := Metadata.GetSequences(FCatalog, FSchema, FSequenceName);
      mdUserDefinedTypes:
        Result := Metadata.GetUDTs(FCatalog, FSchema, FTypeName, nil);
     end;
  finally
    Connection.HideSQLHourGlass;
  end;
end;

end.
