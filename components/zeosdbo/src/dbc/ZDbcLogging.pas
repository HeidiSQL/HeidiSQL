{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Database Logging Classes and Interfaces         }
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
    FMessage: string;
    FErrorCode: Integer;
    FError: string;
    FTimestamp: TDateTime;
  public
    constructor Create(Category: TZLoggingCategory; Protocol: string;
      Msg: string; ErrorCode: Integer; Error: string);

    function AsString: string;

    property Category: TZLoggingCategory read FCategory;
    property Protocol: string read FProtocol;
    property Message: string read FMessage;
    property ErrorCode: Integer read FErrorCode;
    property Error: string read FError;
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
  Protocol: string; Msg: string; ErrorCode: Integer; Error: string);
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
function TZLoggingEvent.AsString: string;
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

