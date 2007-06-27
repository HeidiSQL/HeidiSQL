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

unit ZMySqlAnalyser;

interface

{$I ZParseSql.inc}

uses Classes, ZGenericSqlAnalyser;

type

  {** Implements an MySQL statements analyser. }
  TZMySQLStatementAnalyser = class (TZGenericStatementAnalyser)
  public
    constructor Create;
  end;

implementation

const
  {** The generic constants.}
  MySQLSectionNames: array[0..16] of string = (
    'SELECT', 'UPDATE', 'DELETE', 'INSERT', 'FROM',
    'WHERE', 'INTO', 'GROUP*BY', 'HAVING', 'ORDER*BY',
    'FOR*UPDATE', 'LIMIT', 'OFFSET', 'INTO*OUTFILE',
    'INTO*DUMPFILE', 'PROCEDURE', 'LOCK*IN*SHARE'
  );
  MySQLSelectOptions: array[0..7] of string = (
    'DISTINCT', 'ALL', 'DISTINCTROW', 'STRAIGHT_JOIN', 'SQL_SMALL_RESULT',
    'SQL_BIG_RESULT', 'SQL_BUFFER_RESULT', 'HIGH_PRIORITY'
  );
  MySQLFromJoins: array[0..7] of string = (
    'NATURAL', 'RIGHT', 'LEFT', 'INNER', 'OUTER', 'JOIN',
    'STRAIGHT_JOIN', 'CROSS'
  );
  MySQLFromClauses: array[0..3] of string = (
    'ON', 'USING', 'USE', 'IGNORE'
  );

{ TZMySQLStatementAnalyser }

{**
  Creates the object and assignes the main properties.
}
constructor TZMySQLStatementAnalyser.Create;
begin
  SectionNames := ArrayToStrings(MySQLSectionNames);
  SelectOptions := ArrayToStrings(MySQLSelectOptions);
  FromJoins := ArrayToStrings(MySQLFromJoins);
  FromClauses := ArrayToStrings(MySQLFromClauses);
end;

end.

