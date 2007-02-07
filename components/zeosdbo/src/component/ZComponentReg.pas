{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Database Components Registration             }
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

  RegisterPropertyEditor(TypeInfo(string), TZQuery, 'LinkedFields',
    TZDataFieldPropertyEditor); {renamed by bangfauzan}
  RegisterPropertyEditor(TypeInfo(string), TZQuery, 'MasterFields',
    TZMasterFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZQuery, 'SortedFields',
    TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZQuery, 'SequenceField',
    TZDataFieldPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TZReadOnlyQuery, 'LinkedFields',
    TZDataFieldPropertyEditor); {renamed by bangfauzan}
  RegisterPropertyEditor(TypeInfo(string), TZReadOnlyQuery, 'MasterFields',
    TZMasterFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZReadOnlyQuery, 'SortedFields',
    TZDataFieldPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TZTable, 'TableName',
    TZTableNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZTable, 'LinkedFields',
    TZDataFieldPropertyEditor); {renamed by bangfauzan}
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
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'LinkedFields',
    TZDataFieldPropertyEditor); {renamed by bangfauzan}
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

