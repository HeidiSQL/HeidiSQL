{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Utility Classes for Native Libraries           }
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

unit ZPlainLoader;

interface

{$I ZPlain.inc}

{$IFNDEF VER130BELOW}
uses Types;
{$ELSE}
uses ZCompatibility;
{$ENDIF}

type
  {** Implements a loader for native library. }
  TZNativeLibraryLoader = class (TObject)
  private
    FLocations: TStringDynArray;
    FHandle: LongWord;
    FLoaded: Boolean;
  protected
    function LoadNativeLibrary: Boolean; virtual;
    procedure FreeNativeLibrary; virtual;
    function GetAddress(ProcName: PChar): Pointer;
  public
    constructor Create(Locations: array of string);
    destructor Destroy; override;

    function Load: Boolean; virtual;
    procedure LoadIfNeeded; virtual;

//    property Locations: TStringDynArray read FLocations write FLocations;
    property Handle: LongWord read FHandle write FHandle;
    property Loaded: Boolean read FLoaded write FLoaded;
  end;

implementation

uses
{$IFNDEF UNIX}
  Windows,
{$ELSE}
  libc,
{$ENDIF}
  ZMessages, SysUtils;

{ TZNativeLibraryLoader }

{**
  Creates this loader class and assignes main properties.
  @param Locations locations of native library on windows platform.
}
constructor TZNativeLibraryLoader.Create(Locations: array of string);
var
  I: Integer;
begin
  SetLength(FLocations, Length(Locations));
  for I := 0 to High(Locations) do
    FLocations[I] := Locations[I]; 
  FHandle := 0;
  FLoaded := False;
end;

{**
  Destroys the library and cleanups the memory.
}
destructor TZNativeLibraryLoader.Destroy;
begin
  if Loaded then
    FreeNativeLibrary;
  inherited Destroy;
end;

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZNativeLibraryLoader.Load: Boolean;
begin
  Result := LoadNativeLibrary;
end;

{**
  Loads a library if it was not previously loaded.
}
procedure TZNativeLibraryLoader.LoadIfNeeded;
begin
  if not Loaded then
    Load;
end;

{**
  Loads a library module and initializes the handle.
  @return <code>True</code> is library was successfully loaded.
}
function TZNativeLibraryLoader.LoadNativeLibrary: Boolean;
var
  I: Integer;
  Location: string;
  TriedLocations: string;
begin
  Loaded := False;
  Location := '';
  TriedLocations := '';
  if Handle = 0 then
  begin
    for I := 0 to High(FLocations) do
    begin
      Location := FLocations[I];
//      Handle := GetModuleHandle(PChar(Location));
//      if Handle = 0 then
//      begin
{$IFDEF UNIX}
  {$IFDEF FPC}
        Handle := LoadLibrary(PChar(Location));
  {$ELSE}
        Handle := HMODULE(dlopen(PChar(Location), RTLD_GLOBAL));
  {$ENDIF}
{$ELSE}
        Handle := LoadLibrary(PChar(Location));
{$ENDIF}
//      end;
      if Handle <> 0 then
      begin
        Loaded := True;
        Break;
      end;
      if TriedLocations <> '' then
        TriedLocations := TriedLocations + ', ';
      TriedLocations := TriedLocations + Location;
    end;
  end;

  if not Loaded then
    raise Exception.Create(Format(SLibraryNotFound, [TriedLocations]));
  Result := True;
end;

{**
  Frees a previously loaded library.
}
procedure TZNativeLibraryLoader.FreeNativeLibrary;
begin
  if (Handle <> 0) and Loaded then
    FreeLibrary(Handle);
  Handle := 0;
  Loaded := False;
end;

{**
  Gets a procedure address from the loaded library by its name.
  @param ProcName a name of the procedure.
  @return a procedure address.
}
function TZNativeLibraryLoader.GetAddress(ProcName: PChar): Pointer;
begin
  Result := GetProcAddress(Handle, ProcName);
end;

end.


