{==============================================================================|
| Project : Ararat Synapse                                       | 001.001.001 |
|==============================================================================|
| Content: ICONV support for Win32, Linux and .NET                             |
|==============================================================================|
| Copyright (c)2004-2010, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2004-2010.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}
//old Delphi does not have MSWINDOWS define.
{$IFDEF WIN32}
  {$IFNDEF MSWINDOWS}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

{:@abstract(LibIconv support)

This unit is Pascal interface to LibIconv library for charset translations.
LibIconv is loaded dynamicly on-demand. If this library is not found in system,
requested LibIconv function just return errorcode.
}
unit synaicnv;

interface

uses
{$IFDEF CIL}
  System.Runtime.InteropServices,
  System.Text,
{$ENDIF}
  synafpc,
{$IFNDEF MSWINDOWS}
  {$IFNDEF FPC}
  Libc,
  {$ENDIF}
  SysUtils;
{$ELSE}
  Windows;
{$ENDIF}


const
  {$IFNDEF MSWINDOWS}
  DLLIconvName = 'libiconv.so';
  {$ELSE}
  DLLIconvName = 'iconv.dll';
  {$ENDIF}

type
  size_t = Cardinal;
{$IFDEF CIL}
  iconv_t = IntPtr;
{$ELSE}
  iconv_t = Pointer;
{$ENDIF}
  argptr = iconv_t;

var
  iconvLibHandle: TLibHandle = 0;

function SynaIconvOpen(const tocode, fromcode: Ansistring): iconv_t;
function SynaIconvOpenTranslit(const tocode, fromcode: Ansistring): iconv_t;
function SynaIconvOpenIgnore(const tocode, fromcode: Ansistring): iconv_t;
function SynaIconv(cd: iconv_t; inbuf: AnsiString; var outbuf: AnsiString): integer;
function SynaIconvClose(var cd: iconv_t): integer;
function SynaIconvCtl(cd: iconv_t; request: integer; argument: argptr): integer;

function IsIconvloaded: Boolean;
function InitIconvInterface: Boolean;
function DestroyIconvInterface: Boolean;

const
  ICONV_TRIVIALP          = 0;  // int *argument
  ICONV_GET_TRANSLITERATE = 1;  // int *argument
  ICONV_SET_TRANSLITERATE = 2;  // const int *argument
  ICONV_GET_DISCARD_ILSEQ = 3;  // int *argument
  ICONV_SET_DISCARD_ILSEQ = 4;  // const int *argument


implementation

uses SyncObjs;

{$IFDEF CIL}
  [DllImport(DLLIconvName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'libiconv_open')]
    function _iconv_open(tocode: string; fromcode: string): iconv_t; external;

  [DllImport(DLLIconvName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'libiconv')]
    function _iconv(cd: iconv_t; var inbuf: IntPtr; var inbytesleft: size_t;
    var outbuf: IntPtr; var outbytesleft: size_t): size_t; external;

  [DllImport(DLLIconvName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'libiconv_close')]
    function _iconv_close(cd: iconv_t): integer; external;

  [DllImport(DLLIconvName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'libiconvctl')]
    function _iconvctl(cd: iconv_t; request: integer; argument: argptr): integer; external;

{$ELSE}
type
  Ticonv_open = function(tocode: pAnsichar; fromcode: pAnsichar): iconv_t; cdecl;
  Ticonv = function(cd: iconv_t; var inbuf: pointer; var inbytesleft: size_t;
    var outbuf: pointer; var outbytesleft: size_t): size_t; cdecl;
  Ticonv_close = function(cd: iconv_t): integer; cdecl;
  Ticonvctl = function(cd: iconv_t; request: integer; argument: argptr): integer; cdecl;
var
  _iconv_open: Ticonv_open = nil;
  _iconv: Ticonv = nil;
  _iconv_close: Ticonv_close = nil;
  _iconvctl: Ticonvctl = nil;
{$ENDIF}


var
  IconvCS: TCriticalSection;
  Iconvloaded: boolean = false;

function SynaIconvOpen (const tocode, fromcode: Ansistring): iconv_t;
begin
{$IFDEF CIL}
  try
    Result := _iconv_open(tocode, fromcode);
  except
    on Exception do
      Result := iconv_t(-1);
  end;
{$ELSE}
  if InitIconvInterface and Assigned(_iconv_open) then
    Result := _iconv_open(PAnsiChar(tocode), PAnsiChar(fromcode))
  else
    Result := iconv_t(-1);
{$ENDIF}
end;

function SynaIconvOpenTranslit (const tocode, fromcode: Ansistring): iconv_t;
begin
  Result := SynaIconvOpen(tocode + '//IGNORE//TRANSLIT', fromcode);
end;

function SynaIconvOpenIgnore (const tocode, fromcode: Ansistring): iconv_t;
begin
  Result := SynaIconvOpen(tocode + '//IGNORE', fromcode);
end;

function SynaIconv (cd: iconv_t; inbuf: AnsiString; var outbuf: AnsiString): integer;
var
{$IFDEF CIL}
  ib, ob: IntPtr;
  ibsave, obsave: IntPtr;
  l: integer;
{$ELSE}
  ib, ob: Pointer;
{$ENDIF}
  ix, ox: size_t;
begin
{$IFDEF CIL}
  l := Length(inbuf) * 4;
  ibsave := IntPtr.Zero;
  obsave := IntPtr.Zero;
  try
    ibsave := Marshal.StringToHGlobalAnsi(inbuf);
    obsave := Marshal.AllocHGlobal(l);
    ib := ibsave;
    ob := obsave;
    ix := Length(inbuf);
    ox := l;
    _iconv(cd, ib, ix, ob, ox);
    Outbuf := Marshal.PtrToStringAnsi(obsave, l);
    setlength(Outbuf, l - ox);
    Result := Length(inbuf) - ix;
  finally
    Marshal.FreeCoTaskMem(ibsave);
    Marshal.FreeHGlobal(obsave);
  end;
{$ELSE}
  if InitIconvInterface and Assigned(_iconv) then
  begin
    setlength(Outbuf, Length(inbuf) * 4);
    ib := Pointer(inbuf);
    ob := Pointer(Outbuf);
    ix := Length(inbuf);
    ox := Length(Outbuf);
    _iconv(cd, ib, ix, ob, ox);
    setlength(Outbuf, cardinal(Length(Outbuf)) - ox);
    Result := Cardinal(Length(inbuf)) - ix;
  end
  else
  begin
    Outbuf := '';
    Result := 0;
  end;
{$ENDIF}
end;

function SynaIconvClose(var cd: iconv_t): integer;
begin
  if cd = iconv_t(-1) then
  begin
    Result := 0;
    Exit;
  end;
{$IFDEF CIL}
  try;
    Result := _iconv_close(cd)
  except
    on Exception do
      Result := -1;
  end;
  cd := iconv_t(-1);
{$ELSE}
  if InitIconvInterface and Assigned(_iconv_close) then
    Result := _iconv_close(cd)
  else
    Result := -1;
  cd := iconv_t(-1);
{$ENDIF}
end;

function SynaIconvCtl (cd: iconv_t; request: integer; argument: argptr): integer;
begin
{$IFDEF CIL}
  Result := _iconvctl(cd, request, argument)
{$ELSE}
  if InitIconvInterface and Assigned(_iconvctl) then
    Result := _iconvctl(cd, request, argument)
  else
    Result := 0;
{$ENDIF}
end;

function InitIconvInterface: Boolean;
begin
  IconvCS.Enter;
  try
    if not IsIconvloaded then
    begin
{$IFDEF CIL}
      IconvLibHandle := 1;
{$ELSE}
      IconvLibHandle := LoadLibrary(PChar(DLLIconvName));
{$ENDIF}
      if (IconvLibHandle <> 0) then
      begin
{$IFNDEF CIL}
        _iconv_open := GetProcAddress(IconvLibHandle, PAnsiChar(AnsiString('libiconv_open')));
        _iconv := GetProcAddress(IconvLibHandle, PAnsiChar(AnsiString('libiconv')));
        _iconv_close := GetProcAddress(IconvLibHandle, PAnsiChar(AnsiString('libiconv_close')));
        _iconvctl := GetProcAddress(IconvLibHandle, PAnsiChar(AnsiString('libiconvctl')));
{$ENDIF}
        Result := True;
        Iconvloaded := True;
      end
      else
      begin
        //load failed!
        if IconvLibHandle <> 0 then
        begin
{$IFNDEF CIL}
          FreeLibrary(IconvLibHandle);
{$ENDIF}
          IconvLibHandle := 0;
        end;
        Result := False;
      end;
    end
    else
      //loaded before...
      Result := true;
  finally
    IconvCS.Leave;
  end;
end;

function DestroyIconvInterface: Boolean;
begin
  IconvCS.Enter;
  try
    Iconvloaded := false;
    if IconvLibHandle <> 0 then
    begin
{$IFNDEF CIL}
      FreeLibrary(IconvLibHandle);
{$ENDIF}
      IconvLibHandle := 0;
    end;
{$IFNDEF CIL}
    _iconv_open := nil;
    _iconv := nil;
    _iconv_close := nil;
    _iconvctl := nil;
{$ENDIF}
  finally
    IconvCS.Leave;
  end;
  Result := True;
end;

function IsIconvloaded: Boolean;
begin
  Result := IconvLoaded;
end;

 initialization
begin
  IconvCS:= TCriticalSection.Create;
end;

finalization
begin
{$IFNDEF CIL}
  DestroyIconvInterface;
{$ENDIF}
  IconvCS.Free;
end;

end.
