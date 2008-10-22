{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Abstract Table component                  }
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

unit ZAbstractTable;

interface

{$I ZComponent.inc}

uses
  SysUtils, DB, Classes, ZAbstractDataset;

type

  {**
    Abstract dataset component which works with one specified table.
  }
  TZAbstractTable = class(TZAbstractDataset)
  private
    FTableName: string;

  private
    procedure SetTableName(const Value: string);

  protected
  {$IFDEF WITH_IPROVIDER}
    function PSIsSQLBased: Boolean; override;
    function PSGetTableNameW: WideString; override;
    procedure PSSetCommandText(const ACommandText: string); override;
  {$ENDIF}

  protected
    property TableName: string read FTableName write SetTableName;
  end;

implementation


{ TZAbstractTable }

{**
  Sets a new table name and generates a related SQL statement.
  @param Value a new name of table.
}
procedure TZAbstractTable.SetTableName(const Value: string);
begin
  if FTableName <> Value then
  begin
    FTableName := Value;
    if Value <> '' then
      SQL.Text := Format('SELECT * FROM %s', [FTableName])
    else SQL.Text := '';
  end;
end;

{$IFDEF WITH_IPROVIDER}

{**
  Checks if dataset can execute SQL queries?
  @returns <code>True</code> if the query can execute SQL.
}
function TZAbstractTable.PSIsSQLBased: Boolean;
begin
  Result := False;
end;

{**
  Gets the name of the table.
  @returns the name of this table.
}
function TZAbstractTable.PSGetTableNameW: WideString;
begin
  Result := TableName;
end;

{**
  Assignes a new name for this table.
  @param ACommandText a new name for this table.
}
procedure TZAbstractTable.PSSetCommandText(const ACommandText: string);
begin
  TableName := ACommandText;
end;

{$ENDIF}

end.
