{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 SQL Monitor component                   }
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

unit ZSqlMonitor;

interface

{$I ZComponent.inc}

uses
  SysUtils, Classes, Contnrs, ZClasses, ZCompatibility, ZDbcIntfs, ZDbcLogging;

type

  {** Repeat declaration of TZLoggingEvent. }
  TZLoggingEvent = ZDbcLogging.TZLoggingEvent;

  {** Declares event before logging. }
  TZTraceEvent = procedure(Sender: TObject; Event: TZLoggingEvent;
    var LogTrace: Boolean) of object;

  {** Declares event after logging. }
  TZTraceLogEvent = procedure(Sender: TObject; Event: TZLoggingEvent) of object;

  {**
    Implements an object to log events from SQL client.
  }
  TZSQLMonitor = class(TComponent, IZLoggingListener, IZInterface)
  private
    FActive: Boolean;
    FAutoSave: Boolean;
    FFileName: string;
    FMaxTraceCount: Integer;
    FTraceList: TObjectList;
    FOnTrace: TZTraceEvent;
    FOnLogTrace: TZTraceLogEvent;

    function GetTraceCount: Integer;
    function GetTraceItem(Index: Integer): TZLoggingEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetMaxTraceCount(const Value: Integer);

    procedure TruncateTraceList(Count: Integer);
    procedure DoTrace(Event: TZLoggingEvent; var LogTrace: Boolean);
    procedure DoLogTrace(Event: TZLoggingEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LogEvent(Event: TZLoggingEvent);
    procedure Save();
    procedure SaveToFile(const FileName: string);

    property TraceCount: Integer read GetTraceCount;
    property TraceList[Index: Integer]: TZLoggingEvent read GetTraceItem;
  published
    property Active: Boolean read FActive write SetActive default False;
    property AutoSave: Boolean read FAutoSave write FAutoSave default False;
    property FileName: string read FFileName write FFileName;
    property MaxTraceCount: Integer read FMaxTraceCount write SetMaxTraceCount;

    property OnTrace: TZTraceEvent read FOnTrace write FOnTrace;
    property OnLogTrace: TZTraceLogEvent read FOnLogTrace write FOnLogTrace;
  end;

implementation

{ TZSQLMonitor }

{**
  Constructs this object and assignes main properties.
  @param AOwner a component owner.
}
constructor TZSQLMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTraceList := TObjectList.Create;
  FMaxTraceCount := 100;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZSQLMonitor.Destroy;
begin
  SetActive(False);
  FTraceList.Free;
  inherited Destroy;
end;

{**
  Gets a number of stored logging events.
  @returns a number of stored logging events.
}
function TZSQLMonitor.GetTraceCount: Integer;
begin
  Result := FTraceList.Count;
end;

{**
  Gets a logging event by it's index.
  @param Index an event index.
  @retuns a requested event object.
}
function TZSQLMonitor.GetTraceItem(Index: Integer): TZLoggingEvent;
begin
  Result := TZLoggingEvent(FTraceList[Index]);
end;

{**
  Sets an active state for this monitor.
  @param Value <code>True</code> to activate this monitor
    and <code>False</code> to deactivate it.
}
procedure TZSQLMonitor.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if DriverManager = nil then Exit;
    if Value then
      DriverManager.AddLoggingListener(Self)
    else DriverManager.RemoveLoggingListener(Self);
  end;
end;

{**
  Sets a new number of logging events in the storage.
  @param Value a new number of logging events.
}
procedure TZSQLMonitor.SetMaxTraceCount(const Value: Integer);
begin
  if Value <> FMaxTraceCount then
  begin
    FMaxTraceCount := Value;
    TruncateTraceList(Value);
  end;
end;

{**
  Truncates a storage of logging events to the specified limit.
  @param Count a number of events in the storage.
}
procedure TZSQLMonitor.TruncateTraceList(Count: Integer);
begin
  while FTraceList.Count > Count do
    FTraceList.Delete(0);
end;

{**
  Invokes an event listener after logging event.
  @param Event a logging event object.
}
procedure TZSQLMonitor.DoLogTrace(Event: TZLoggingEvent);
begin
  if Assigned(FOnLogTrace) then
    FOnLogTrace(Self, Event);
end;

{**
  Invokes an event listener before logging event.
  @param Event a logging event object.
  @param LogTrace a flag which switches storing the event.
}
procedure TZSQLMonitor.DoTrace(Event: TZLoggingEvent;
  var LogTrace: Boolean);
begin
  if Assigned(FOnTrace) then
    FOnTrace(Self, Event, LogTrace);
end;

{**
  Saves the logging events into predefined file
  set in FileName property.
}
procedure TZSQLMonitor.Save;
begin
  SaveToFile(FFileName);
end;

{**
  Saves the logging events to the specified file.
  @param FileName a name of the file to write the events.
}
procedure TZSQLMonitor.SaveToFile(const FileName: string);
var
  I: Integer;
  Stream: TFileStream;
  Temp: string;
  Buffer: PChar;
begin
  if not FileExists(FileName) then
    Stream := TFileStream.Create(FileName, fmCreate)
  else
    Stream := TFileStream.Create(FileName, fmOpenWrite or fmShareDenyWrite);
  try
    for I := 0 to FTraceList.Count - 1 do
    begin
      Temp := TZLoggingEvent(FTraceList[I]).AsString + #13#10;
      Buffer := PChar(Temp);
      Stream.Write(Buffer^, StrLen(Buffer));
    end;
  finally
    Stream.Free;
  end;
end;

{**
  Handles a new incoming logging event.
  @param Event an incoming logging event.
}
procedure TZSQLMonitor.LogEvent(Event: TZLoggingEvent);
var
  LogTrace: Boolean;
  Stream: TFileStream;
  Temp: WideString;
  Buffer: PWideChar;
begin
  LogTrace := True;
  DoTrace(Event, LogTrace);
  if not LogTrace then Exit;

  { Store the event. }
  if FMaxTraceCount <> 0 then
  begin
    if FMaxTraceCount > 0 then
      TruncateTraceList(FMaxTraceCount - 1);
    FTraceList.Add(TZLoggingEvent.Create(Event.Category, Event.Protocol,
      Event.Message, Event.ErrorCode, Event.Error));
  end;

  { Save the event. }
  if FAutoSave and (FFileName <> '') then
  begin
    if not FileExists(FFileName) then
      Stream := TFileStream.Create(FFileName, fmCreate)
    else
      Stream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareDenyWrite);
    try
      Stream.Seek(0, soFromEnd);
      Temp := Event.AsString + #13#10;
      Buffer := PWideChar(Temp);
      Stream.Write(Buffer^, Length(Temp));
    finally
      Stream.Free;
    end;
  end;

  DoLogTrace(Event);
end;

end.
