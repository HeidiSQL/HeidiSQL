{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Database Components Registration             }
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

unit ZComponentReg;

interface

{$I ZComponent.inc}

{ Zeos palette names }
const
  ZEOS_DB_PALETTE = 'Zeos Access';

procedure Register;

implementation

uses
{$IFDEF WITH_PROPERTY_EDITOR}
  ZPropertyEditor,
{$IFDEF FPC}
  PropEdits,
  ZUpdateSqlEditor,
  ComponentEditors,
  LResources,
{$ELSE}
{$IFNDEF VER130BELOW}
{$IFNDEF UNIX}
{$IFNDEF FPC}
  ZUpdateSqlEditor,
{$ENDIF}
{$ENDIF}
  DesignIntf,
{$ELSE}
  DsgnIntf,
{$ENDIF}
{$ENDIF}
{$ENDIF}
  Classes, ZConnection, ZDataset, ZSqlUpdate, ZSqlProcessor, ZStoredProcedure,
  ZSqlMonitor, ZSqlMetadata, ZSequence;

{**
  Registers components in a component palette.
}
procedure Register;
begin
  RegisterComponents(ZEOS_DB_PALETTE, [
    TZConnection, TZReadOnlyQuery, TZQuery, TZTable, TZUpdateSQL,
    TZStoredProc, TZSQLMetadata, TZSQLProcessor, TZSQLMonitor, TZSequence]);

{$IFDEF WITH_PROPERTY_EDITOR}
  RegisterPropertyEditor(TypeInfo(string), TZConnection, 'Protocol',
    TZProtocolPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZConnection, 'Database',
    TZDatabasePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZConnection, 'Catalog',
    TZCatalogPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TZQuery, 'IndexFieldNames',
    TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZQuery, 'MasterFields',
    TZMasterFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZQuery, 'SortedFields',
    TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZQuery, 'SequenceField',
    TZDataFieldPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TZReadOnlyQuery, 'IndexFieldNames',
    TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZReadOnlyQuery, 'MasterFields',
    TZMasterFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZReadOnlyQuery, 'SortedFields',
    TZDataFieldPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TZTable, 'TableName',
    TZTableNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZTable, 'IndexFieldNames',
    TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZTable, 'MasterFields',
    TZMasterFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZTable, 'SortedFields',
    TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZTable, 'SequenceField',
    TZDataFieldPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TZStoredProc, 'StoredProcName',
    TZProcedureNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZStoredProc, 'SortedFields',
    TZDataFieldPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TZSequence, 'SequenceName',
    TZSequenceNamePropertyEditor);

{$IFDEF USE_METADATA}
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'Catalog',
    TZCatalogProperty);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'ColumnName',
    TZColumnNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'ForeignCatalog',
    TZCatalogProperty);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'ForeignSchema',
    TZSchemaPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'ForeignTableName',
    TZTableNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'IndexFieldNames',
    TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'MasterFields',
    TZMasterFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'ProcedureName',
    TZProcedureNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'Schema',
    TZSchemaPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'SequenceName',
    TZSequenceNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'SortedFields',
    TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'TableName',
    TZTableNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'TypeName',
    TZTypeNamePropertyEditor);
{$ENDIF}
{$IFDEF FPC}
  RegisterComponentEditor(TZUpdateSQL, TZUpdateSQLEditor);
{$ELSE}
  {$IFNDEF VER130BELOW}
    {$IFNDEF UNIX}
  RegisterComponentEditor(TZUpdateSQL, TZUpdateSQLEditor);
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$ENDIF}
end;

{$IFDEF FPC}
initialization
  {$I ZComponentReg.lrs}
{$ENDIF}

end.

