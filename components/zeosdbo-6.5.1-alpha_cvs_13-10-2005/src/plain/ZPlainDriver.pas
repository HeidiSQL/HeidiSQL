{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Interfaces for Native Plain Drivers          }
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

unit ZPlainDriver;

interface

{$I ZPlain.inc}

uses ZClasses;

type

  {** Represents a generic interface to plain driver. }
  IZPlainDriver = interface (IZInterface)
    ['{2A0CC600-B3C4-43AF-92F5-C22A3BB1BB7D}']
    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;
  end;

implementation

end.

