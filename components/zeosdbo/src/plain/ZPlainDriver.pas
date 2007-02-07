{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Interfaces for Native Plain Drivers          }
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
    {
    function GetClientVersion: Integer;
    function GetServerVersion: Integer;
    procedure GetClientVersionEx(out MajorVersion: Integer;
     out MinorVersion: Integer; out SubVersion: Integer);
    procedure GetServerVersionEx(out MajorVersion: Integer;
     out MinorVersion: Integer; out SubVersion: Integer);
    }
    procedure Initialize;
  end;

  {ADDED by fduenas 15-06-2006}
  {** Base class of a generic plain driver. }
  TZAbstractPlainDriver = class(TZAbstractObject, IZPlainDriver)
    function GetProtocol: string; virtual; abstract;
    function GetDescription: string; virtual; abstract;
    {
    function GetClientVersion: Integer; virtual;
    function GetServerVersion: Integer; virtual;
    procedure GetClientVersionEx(out MajorVersion: Integer;
     out MinorVersion: Integer; out SubVersion: Integer); virtual;
    procedure GetServerVersionEx(out MajorVersion: Integer;
     out MinorVersion: Integer; out SubVersion: Integer); virtual;
    }
    procedure Initialize; virtual; abstract;
  end;
  {END ADDED by fduenas 15-06-2006}
implementation
uses ZSysUtils;
{ TZAbstractPlainDriver }

{ADDED by fduenas 15-06-2006}
{**
  Gets the clients's full version number. Initially this should be 0.
  @return the clients's full version number in the format XYYYZZZ where:
   X   = major_version
   YYY = minor_version
   ZZZ = sub_version

   Version number must be encoded the way below:
    client_version := major_version*1000000 + minor_version *1000 + sub_version

   For example, 4.1.12 is returned as 4001012.
}
{
function TZAbstractPlainDriver.GetClientVersion: Integer;
begin
 Result := 0;
end;
}
{**
  Get Decoded the values of a Client's Full Version encoded with the format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  @param FullVersion an integer containing the Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
{
procedure TZAbstractPlainDriver.GetClientVersionEx(out MajorVersion,
  MinorVersion, SubVersion: integer);
begin
 ZSysUtils.DecodeVersion(GetClientVersion,
                         MajorVersion, MinorVersion, SubVersion);
end;
}
{**
  Gets the servers's full version number. Initially this should be 0.
  @return the server's full version number in the format XYYYZZZ where:
   X   = major_version
   YYY = minor_version
   ZZZ = sub_version

   Version number must be encoded the way below:
    server_version := major_version*1000000 + minor_version *1000 + sub_version

   For example, 4.1.12 is returned as 4001012.
}
{
function TZAbstractPlainDriver.GetServerVersion: Integer;
begin
 Result := 0;
end;
}
{**
  Get Decoded the values of a Server's Full Version encoded with the format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  @param FullVersion an integer containing the Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
{
procedure TZAbstractPlainDriver.GetServerVersionEx(out MajorVersion,
  MinorVersion, SubVersion: integer);
begin
 ZSysUtils.DecodeVersion(GetServerVersion,
                         MajorVersion, MinorVersion, SubVersion);
end;
}
end.

