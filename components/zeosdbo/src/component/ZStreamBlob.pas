{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                  Blob streams classes                   }
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

unit ZStreamBlob;

interface

{$I ZComponent.inc}

uses Classes, SysUtils, ZDbcIntfs, DB;

type
  {** Implements a class for blobs stream. }
  TZBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FBlob: IZBlob;
    FMode: TBlobStreamMode;
  protected
    property Blob: IZBlob read FBlob write FBlob;
    property Mode: TBlobStreamMode read FMode write FMode;
  public
    constructor Create(Field: TBlobField; Blob: IZBlob; Mode: TBlobStreamMode);
    destructor Destroy; override;
  end;

implementation

{ TZBlobStream }

{**
  Constructs this object and assignes the main properties.
  @param Blob
}
constructor TZBlobStream.Create(Field: TBlobField; Blob: IZBlob; Mode: TBlobStreamMode);
var
  TempStream: TStream;
begin
  inherited Create;

  FBlob := Blob;
  FMode := Mode;
  FField := Field;
  if (Mode in [bmRead, bmReadWrite]) and not Blob.IsEmpty then
  begin
    TempStream := Blob.GetStream;
    try
      TempStream.Position := 0;
      CopyFrom(TempStream, TempStream.Size);
      Position := 0;
    finally
      TempStream.Free;
    end;
  end;
end;

type THackedDataset = class(TDataset);

{**
  Destroys this object and cleanups the memory.
}
destructor TZBlobStream.Destroy;
begin
  if Mode in [bmWrite, bmReadWrite] then
  begin
    if Assigned(Self.Memory) then
      Blob.SetStream(Self)
    else Blob.SetStream(nil);
    try
      if Assigned(FField.Dataset) then
        THackedDataset(FField.DataSet).DataEvent(deFieldChange, LongInt(FField));
    except
{$IFNDEF VER130BELOW}
      ApplicationHandleException(Self);
{$ENDIF}
    end;
  end;
  inherited Destroy;
end;

end.

