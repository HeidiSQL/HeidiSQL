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

unit ZPostgreSqlAnalyser;

interface

{$I ZParseSql.inc}

uses Classes, ZGenericSqlAnalyser;

type

  {** Implements an PostgreSQL statements analyser. }
  TZPostgreSQLStatementAnalyser = class (TZGenericStatementAnalyser)
  public
    constructor Create;
  end;

implementation

const
  {** The generic constants.}
  PostgreSQLSectionNames: array[0..12] of string = (
    'SELECT', 'UPDATE', 'DELETE', 'INSERT', 'FROM',
    'WHERE', 'INTO', 'GROUP*BY', 'HAVING', 'ORDER*BY',
    'FOR*UPDATE', 'LIMIT', 'OFFSET'
  );
  PostgreSQLSelectOptions: array[0..1] of string = (
    'DISTINCT', 'ALL'
  );
  PostgreSQLFromJoins: array[0..7] of string = (
    'NATURAL', 'RIGHT', 'LEFT', 'INNER', 'OUTER', 'JOIN',
    'FULL', 'CROSS'
  );
  PostgreSQLFromClauses: array[0..1] of string = (
    'ON', 'USING'
  );

{ TZPostgreSQLStatementAnalyser }

{**
  Creates the object and assignes the main properties.
}
constructor TZPostgreSQLStatementAnalyser.Create;
begin
  SectionNames := ArrayToStrings(PostgreSQLSectionNames);
  SelectOptions := ArrayToStrings(PostgreSQLSelectOptions);
  FromJoins := ArrayToStrings(PostgreSQLFromJoins);
  FromClauses := ArrayToStrings(PostgreSQLFromClauses);
end;

end.
 
