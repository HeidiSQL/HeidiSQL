{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Compatibility Classes and Functions          }
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

unit ZCompatibility;

interface

{$I ZCore.inc}

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
{$IFDEF UNIX}
  {$IFDEF FPC}
    dl,
  {$ELSE}
    libc,
  {$ENDIF FPC}
{$ENDIF UNIX}
  Contnrs, Classes, SysUtils;

type

{$IFDEF VER130BELOW}
  TIntegerDynArray      = array of Integer;
  TCardinalDynArray     = array of Cardinal;
  TWordDynArray         = array of Word;
  TSmallIntDynArray     = array of SmallInt;

  TShortIntDynArray     = array of ShortInt;
  TInt64DynArray        = array of Int64;
  TLongWordDynArray     = array of LongWord;
  TSingleDynArray       = array of Single;
  TDoubleDynArray       = array of Double;
  TBooleanDynArray      = array of Boolean;
  TStringDynArray       = array of string;
  TWideStringDynArray   = array of WideString;
  TVariantDynArray      = array of Variant;
{$ENDIF}

  TByteDynArray         = array of Byte;
  TObjectDynArray       = array of TObject;

{$IFDEF VER130BELOW}
  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;

  PPointer              = ^Pointer;
  PByte                 = ^Byte;
  PBoolean              = ^Boolean;
  PShortInt             = ^ShortInt;
  PSmallInt             = ^SmallInt;
  PInteger              = ^Integer;
  PLongInt              = ^LongInt;
  PSingle               = ^Single;
  PDouble               = ^Double;
  PWordBool             = ^WordBool;
  PCardinal             = ^Cardinal;
  PInt64                = ^Int64;
  PPChar                = ^PChar;
{$ENDIF}
  PWord                 = ^Word;


{$IFDEF VER130BELOW}
type
  TLoginEvent = procedure(Sender: TObject; Username, Password: string) of object;
{$ENDIF}

{$IFDEF VER130BELOW}
type
  TDBScreenCursor = (dcrDefault, dcrHourGlass, dcrSQLWait, dcrOther);

  IDBScreen = interface
    ['{29A1C508-6ADC-44CD-88DE-4F51B25D5995}']
    function GetCursor: TDBScreenCursor;
    procedure SetCursor(Cursor: TDBScreenCursor);

    property Cursor: TDBScreenCursor read GetCursor write SetCursor;
  end;

var
  LoginDialogProc: function (const ADatabaseName: string; var AUserName,
    APassword: string): Boolean;
  DBScreen: IDBScreen;

function StrToFloatDef(Str: string; Def: Extended): Extended;
{$ENDIF}

{$IFDEF VER130BELOW}
function AnsiDequotedStr(const S: string; AQuote: Char): string;
function BoolToStr(Value: Boolean): string;
function VarIsStr(const V: Variant): Boolean;
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF FPC}
const
  RTLD_GLOBAL = $101;

type
  HMODULE = LongWord;

function LoadLibrary(ModuleName: PChar): HMODULE;
function FreeLibrary(Module: HMODULE): LongBool;
function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
function GetModuleHandle(Location: Pchar): HMODULE;
  {$ENDIF FPC}
{$ENDIF UNIX}

implementation

{$IFDEF VER130BELOW}

function StrToFloatDef(Str: string; Def: Extended): Extended;
begin
  try
    if Str <> '' then
      Result := StrToFloat(Str)
    else Result := Def;
  except
    Result := Def;
  end;
end;

{$ENDIF}

{$IFDEF VER130BELOW}

function AnsiDequotedStr(const S: string; AQuote: Char): string;
var
  LText: PChar;
begin
  LText := PChar(S);
  Result := AnsiExtractQuotedStr(LText, AQuote);
  if Result = '' then
    Result := S;
end;

function BoolToStr(Value: Boolean): string;
begin
  if Value = True then
    Result := 'True'
  else Result := 'False';
end;

function VarIsStr(const V: Variant): Boolean;
begin
  Result := ((TVarData(V).VType and varTypeMask) = varOleStr) or
    ((TVarData(V).VType and varTypeMask) = varString);
end;
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF FPC}
function LoadLibrary(ModuleName: PChar): HMODULE;
begin
  Result := HMODULE(dlopen(Modulename, RTLD_GLOBAL));
end;

function FreeLibrary(Module: HMODULE): LongBool;
begin
  Result := longbool(dlclose(pointer(Module)));
end;

function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
begin
  Result := dlsym(pointer(Module), Proc);
end;

function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
begin
  Result := 0;
end;

function GetModuleHandle(Location: Pchar): HMODULE;
begin
  Result := 0;
end;

  {$ENDIF FPC}
{$ENDIF UNIX}

end.

