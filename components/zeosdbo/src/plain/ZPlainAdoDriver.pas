{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Delphi plain driver interface to ADO            }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Janos Fegyverneki                 }
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

unit ZPlainAdoDriver;

interface

{$I ZPlain.inc}

uses ZClasses, ZPlainDriver;

type
  TZAdoPlainDriver = class (TZAbstractObject, IZPlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;
  end;

implementation

constructor TZAdoPlainDriver.Create;
begin
end;

function TZAdoPlainDriver.GetProtocol: string;
begin
  Result := 'ado';
end;

function TZAdoPlainDriver.GetDescription: string;
begin
  Result := 'Native driver for Microsoft ADO';
end;

procedure TZAdoPlainDriver.Initialize;
begin
end;

end.

