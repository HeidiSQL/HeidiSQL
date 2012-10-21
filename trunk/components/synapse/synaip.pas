{==============================================================================|
| Project : Ararat Synapse                                       | 001.002.001 |
|==============================================================================|
| Content: IP address support procedures and functions                         |
|==============================================================================|
| Copyright (c)2006-2010, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c) 2006-2010.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(IP adress support procedures and functions)}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$R-}
{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
  {$WARN SUSPICIOUS_TYPECAST OFF}
{$ENDIF}

unit synaip;

interface

uses
  SysUtils, SynaUtil;

type
{:binary form of IPv6 adress (for string conversion routines)}
  TIp6Bytes = array [0..15] of Byte;
{:binary form of IPv6 adress (for string conversion routines)}
  TIp6Words = array [0..7] of Word;

{:Returns @TRUE, if "Value" is a valid IPv4 address. Cannot be a symbolic Name!}
function IsIP(const Value: string): Boolean;

{:Returns @TRUE, if "Value" is a valid IPv6 address. Cannot be a symbolic Name!}
function IsIP6(const Value: string): Boolean;

{:Returns a string with the "Host" ip address converted to binary form.}
function IPToID(Host: string): Ansistring;

{:Convert IPv6 address from their string form to binary byte array.}
function StrToIp6(value: string): TIp6Bytes;

{:Convert IPv6 address from binary byte array to string form.}
function Ip6ToStr(value: TIp6Bytes): string;

{:Convert IPv4 address from their string form to binary.}
function StrToIp(value: string): integer;

{:Convert IPv4 address from binary to string form.}
function IpToStr(value: integer): string;

{:Convert IPv4 address to reverse form.}
function ReverseIP(Value: AnsiString): AnsiString;

{:Convert IPv6 address to reverse form.}
function ReverseIP6(Value: AnsiString): AnsiString;

{:Expand short form of IPv6 address to long form.}
function ExpandIP6(Value: AnsiString): AnsiString;


implementation

{==============================================================================}

function IsIP(const Value: string): Boolean;
var
  TempIP: string;
  function ByteIsOk(const Value: string): Boolean;
  var
    x, n: integer;
  begin
    x := StrToIntDef(Value, -1);
    Result := (x >= 0) and (x < 256);
    // X may be in correct range, but value still may not be correct value!
    // i.e. "$80"
    if Result then
      for n := 1 to length(Value) do
        if not (AnsiChar(Value[n]) in ['0'..'9']) then
        begin
          Result := False;
          Break;
        end;
  end;
begin
  TempIP := Value;
  Result := False;
  if not ByteIsOk(Fetch(TempIP, '.')) then
    Exit;
  if not ByteIsOk(Fetch(TempIP, '.')) then
    Exit;
  if not ByteIsOk(Fetch(TempIP, '.')) then
    Exit;
  if ByteIsOk(TempIP) then
    Result := True;
end;

{==============================================================================}

function IsIP6(const Value: string): Boolean;
var
  TempIP: string;
  s,t: string;
  x: integer;
  partcount: integer;
  zerocount: integer;
  First: Boolean;
begin
  TempIP := Value;
  Result := False;
  if Value = '::' then
  begin
    Result := True;
    Exit;
  end;
  partcount := 0;
  zerocount := 0;
  First := True;
  while tempIP <> '' do
  begin
    s := fetch(TempIP, ':');
    if not(First) and (s = '') then
      Inc(zerocount);
    First := False;
    if zerocount > 1 then
      break;
    Inc(partCount);
    if s = '' then
      Continue;
    if partCount > 8 then
      break;
    if tempIP = '' then
    begin
      t := SeparateRight(s, '%');
      s := SeparateLeft(s, '%');
      x := StrToIntDef('$' + t, -1);
      if (x < 0) or (x > $ffff) then
        break;
    end;
    x := StrToIntDef('$' + s, -1);
    if (x < 0) or (x > $ffff) then
      break;
    if tempIP = '' then
      if not((PartCount = 1) and (ZeroCount = 0)) then
        Result := True;
  end;
end;

{==============================================================================}
function IPToID(Host: string): Ansistring;
var
  s: string;
  i, x: Integer;
begin
  Result := '';
  for x := 0 to 3 do
  begin
    s := Fetch(Host, '.');
    i := StrToIntDef(s, 0);
    Result := Result + AnsiChar(i);
  end;
end;

{==============================================================================}

function StrToIp(value: string): integer;
var
  s: string;
  i, x: Integer;
begin
  Result := 0;
  for x := 0 to 3 do
  begin
    s := Fetch(value, '.');
    i := StrToIntDef(s, 0);
    Result := (256 * Result) + i;
  end;
end;

{==============================================================================}

function IpToStr(value: integer): string;
var
  x1, x2: word;
  y1, y2: byte;
begin
  Result := '';
  x1 := value shr 16;
  x2 := value and $FFFF;
  y1 := x1 div $100;
  y2 := x1 mod $100;
  Result := inttostr(y1) + '.' + inttostr(y2) + '.';
  y1 := x2 div $100;
  y2 := x2 mod $100;
  Result := Result + inttostr(y1) + '.' + inttostr(y2);
end;

{==============================================================================}

function ExpandIP6(Value: AnsiString): AnsiString;
var
 n: integer;
 s: ansistring;
 x: integer;
begin
  Result := '';
  if value = '' then
    exit;
  x := countofchar(value, ':');
  if x > 7 then
    exit;
  if value[1] = ':' then
    value := '0' + value;
  if value[length(value)] = ':' then
    value := value + '0';
  x := 8 - x;
  s := '';
  for n := 1 to x do
    s := s + ':0';
  s := s + ':';
  Result := replacestring(value, '::', s);
end;
{==============================================================================}

function StrToIp6(Value: string): TIp6Bytes;
var
 IPv6: TIp6Words;
 Index: Integer;
 n: integer;
 b1, b2: byte;
 s: string;
 x: integer;
begin
  for n := 0 to 15 do
    Result[n] := 0;
  for n := 0 to 7 do
    Ipv6[n] := 0;
  Index := 0;
  Value := ExpandIP6(value);
  if value = '' then
    exit;
  while Value <> '' do
  begin
    if Index > 7 then
      Exit;
    s := fetch(value, ':');
    if s = '@' then
      break;
    if s = '' then
    begin
      IPv6[Index] := 0;
    end
    else
    begin
      x := StrToIntDef('$' + s, -1);
      if (x > 65535) or (x < 0) then
        Exit;
      IPv6[Index] := x;
    end;
    Inc(Index);
  end;
  for n := 0 to 7 do
  begin
    b1 := ipv6[n] div 256;
    b2 := ipv6[n] mod 256;
    Result[n * 2] := b1;
    Result[(n * 2) + 1] := b2;
  end;
end;

{==============================================================================}
//based on routine by the Free Pascal development team
function Ip6ToStr(value: TIp6Bytes): string;
var
  i, x: byte;
  zr1,zr2: set of byte;
  zc1,zc2: byte;
  have_skipped: boolean;
  ip6w: TIp6words;
begin
  zr1 := [];
  zr2 := [];
  zc1 := 0;
  zc2 := 0;
  for i := 0 to 7 do
  begin
    x := i * 2;
    ip6w[i] := value[x] * 256 + value[x + 1];
    if ip6w[i] = 0 then
    begin
      include(zr2, i);
      inc(zc2);
    end
    else
    begin
      if zc1 < zc2 then
      begin
        zc1 := zc2;
        zr1 := zr2;
        zc2 := 0;
        zr2 := [];
      end;
    end;
  end;
  if zc1 < zc2 then
  begin
    zr1 := zr2;
  end;
  SetLength(Result, 8*5-1);
  SetLength(Result, 0);
  have_skipped := false;
  for i := 0 to 7 do
  begin
    if not(i in zr1) then
    begin
      if have_skipped then
      begin
        if Result = '' then
          Result := '::'
        else
          Result := Result + ':';
        have_skipped := false;
      end;
      Result := Result + IntToHex(Ip6w[i], 1) + ':';
    end
    else
    begin
      have_skipped := true;
    end;
  end;
  if have_skipped then
    if Result = '' then
      Result := '::0'
    else
      Result := Result + ':';

  if Result = '' then
    Result := '::0';
  if not (7 in zr1) then
    SetLength(Result, Length(Result)-1);
  Result := LowerCase(result);
end;

{==============================================================================}
function ReverseIP(Value: AnsiString): AnsiString;
var
  x: Integer;
begin
  Result := '';
  repeat
    x := LastDelimiter('.', Value);
    Result := Result + '.' + Copy(Value, x + 1, Length(Value) - x);
    Delete(Value, x, Length(Value) - x + 1);
  until x < 1;
  if Length(Result) > 0 then
    if Result[1] = '.' then
      Delete(Result, 1, 1);
end;

{==============================================================================}
function ReverseIP6(Value: AnsiString): AnsiString;
var
  ip6: TIp6bytes;
  n: integer;
  x, y: integer;
begin
  ip6 := StrToIP6(Value);
  x := ip6[15] div 16;
  y := ip6[15] mod 16;
  Result := IntToHex(y, 1) + '.' + IntToHex(x, 1);
  for n := 14 downto 0 do
  begin
    x := ip6[n] div 16;
    y := ip6[n] mod 16;
    Result := Result + '.' + IntToHex(y, 1) + '.' + IntToHex(x, 1);
  end;
end;

{==============================================================================}
end.
