{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Abstract Table component                  }
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
    procedure SetTableName(Value: string);

  protected
  {$IFDEF WITH_IPROVIDER}
    function PSIsSQLBased: Boolean; override;
    function PSGetTableName: string; override;
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
procedure TZAbstractTable.SetTableName(Value: string);
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
function TZAbstractTable.PSGetTableName: string;
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
