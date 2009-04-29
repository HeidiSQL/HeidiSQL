{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Database Logging Classes and Interfaces         }
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

unit ZDbcLogging;

interface

{$I ZDbc.inc}

uses SysUtils, ZClasses;

type

  {** Defines a time or the message. }
  TZLoggingCategory = (lcConnect, lcDisconnect, lcTransaction, lcExecute, lcOther);

  {** Defines a object for logging event. }
  TZLoggingEvent = class (TObject)
  private
    FCategory: TZLoggingCategory;
    FProtocol: string;
    FMessage: WideString;
    FErrorCode: Integer;
    FError: WideString;
    FTimestamp: TDateTime;
  public
    constructor Create(Category: TZLoggingCategory; Protocol: string;
      Msg: WideString; ErrorCode: Integer; Error: WideString);

    function AsString: WideString;

    property Category: TZLoggingCategory read FCategory;
    property Protocol: string read FProtocol;
    property Message: WideString read FMessage;
    property ErrorCode: Integer read FErrorCode;
    property Error: WideString read FError;
    property Timestamp: TDateTime read FTimestamp;
  end;

  {** Defines an interface to accept logging events. }
  IZLoggingListener = interface (IZInterface)
    ['{53559F5F-AC22-4DDC-B2EA-45D21ADDD2D4}']

    procedure LogEvent(Event: TZLoggingEvent);
  end;

implementation

{ TZLoggingEvent }

{**
  Constructs this logging event.
  @param Protocol a DBC protocol.
  @param Msg a description message.
  @param ErrorCode an error code.
  @param Error an error message.
}
constructor TZLoggingEvent.Create(Category: TZLoggingCategory;
  Protocol: string; Msg: WideString; ErrorCode: Integer; Error: WideString);
begin
  FCategory := Category;
  FProtocol := Protocol;
  FMessage := Msg;
  FErrorCode := ErrorCode;
  FError := Error;
  FTimestamp := Now;
end;

{**
  Gets a string representation for this event.
  @returns a string representation.
}
function TZLoggingEvent.AsString: WideString;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:mm:ss', FTimestamp) + ' cat: ';
  case FCategory of
    lcConnect: Result := Result + 'Connect';
    lcDisconnect: Result := Result + 'Disconnect';
    lcTransaction: Result := Result + 'Transaction';
    lcExecute: Result := Result + 'Execute';
    else Result := Result + 'Other';
  end;
  if Protocol <> '' then
    Result := Result + ', proto: ' + FProtocol;
  Result := Result + ', msg: ' + FMessage;
  if (FErrorCode <> 0) or (FError <> '') then
  begin
    Result := Result + ', errcode: ' + IntToStr(FErrorCode)
      + ', error: ' + FError;
  end;
end;

end.

