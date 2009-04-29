{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Compatibility Classes and Functions          }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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
  PLongWord             = ^LongWord;
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

function StrToFloatDef(const Str: string; Def: Extended): Extended;
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
//function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
//function GetModuleHandle(Location: Pchar): HMODULE;
  {$ENDIF FPC}
{$ENDIF UNIX}

implementation

{$IFDEF VER130BELOW}

function StrToFloatDef(const Str: string; Def: Extended): Extended;
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

{function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
begin
  Result := 0;
end;

function GetModuleHandle(Location: Pchar): HMODULE;
begin
  Result := 0;
end;
}
  {$ENDIF FPC}
{$ENDIF UNIX}

end.

