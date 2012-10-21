{==============================================================================|
| Project : Ararat Synapse                                       | 001.001.011 |
|==============================================================================|
| Content: Inline MIME support procedures and functions                        |
|==============================================================================|
| Copyright (c)1999-2006, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)2000-2006.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(Utilities for inline MIME)
Support for Inline MIME encoding and decoding.

Used RFC: RFC-2047, RFC-2231
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit mimeinln;

interface

uses
  SysUtils, Classes,
  synachar, synacode, synautil;

{:Decodes mime inline encoding (i.e. in headers) uses target characterset "CP".}
function InlineDecode(const Value: string; CP: TMimeChar): string;

{:Encodes string to MIME inline encoding. The source characterset is "CP", and
 the target charset is "MimeP".}
function InlineEncode(const Value: string; CP, MimeP: TMimeChar): string;

{:Returns @true, if "Value" contains characters needed for inline coding.}
function NeedInline(const Value: AnsiString): boolean;

{:Inline mime encoding similar to @link(InlineEncode), but you can specify
 source charset, and the target characterset is automatically assigned.}
function InlineCodeEx(const Value: string; FromCP: TMimeChar): string;

{:Inline MIME encoding similar to @link(InlineEncode), but the source charset
 is automatically set to the system default charset, and the target charset is
 automatically assigned from set of allowed encoding for MIME.}
function InlineCode(const Value: string): string;

{:Converts e-mail address to canonical mime form. You can specify source charset.}
function InlineEmailEx(const Value: string; FromCP: TMimeChar): string;

{:Converts e-mail address to canonical mime form. Source charser it system
 default charset.}
function InlineEmail(const Value: string): string;

implementation

{==============================================================================}

function InlineDecode(const Value: string; CP: TMimeChar): string;
var
  s, su, v: string;
  x, y, z, n: Integer;
  ichar: TMimeChar;
  c: Char;

  function SearchEndInline(const Value: string; be: Integer): Integer;
  var
    n, q: Integer;
  begin
    q := 0;
    Result := 0;
    for n := be + 2 to Length(Value) - 1 do
      if Value[n] = '?' then
      begin
        Inc(q);
        if (q > 2) and (Value[n + 1] = '=') then
        begin
          Result := n;
          Break;
        end;
      end;
  end;

begin
  Result := '';
  v := Value;
  x := Pos('=?', v);
  y := SearchEndInline(v, x);
  //fix for broken coding with begin, but not with end.
  if (x > 0) and (y <= 0) then
    y := Length(Result);
  while (y > x) and (x > 0) do
  begin
    s := Copy(v, 1, x - 1);
    if Trim(s) <> '' then
      Result := Result + s;
    s := Copy(v, x, y - x + 2);
    Delete(v, 1, y + 1);
    su := Copy(s, 3, Length(s) - 4);
    z := Pos('?', su);
    if (Length(su) >= (z + 2)) and (su[z + 2] = '?') then
    begin
      ichar := GetCPFromID(SeparateLeft(Copy(su, 1, z - 1), '*'));
      c := UpperCase(su)[z + 1];
      su := Copy(su, z + 3, Length(su) - z - 2);
      if c = 'B' then
      begin
        s := DecodeBase64(su);
        s := CharsetConversion(s, ichar, CP);
      end;
      if c = 'Q' then
      begin
        s := '';
        for n := 1 to Length(su) do
          if su[n] = '_' then
            s := s + ' '
          else
            s := s + su[n];
        s := DecodeQuotedPrintable(s);
        s := CharsetConversion(s, ichar, CP);
      end;
    end;
    Result := Result + s;
    x := Pos('=?', v);
    y := SearchEndInline(v, x);
  end;
  Result := Result + v;
end;

{==============================================================================}

function InlineEncode(const Value: string; CP, MimeP: TMimeChar): string;
var
  s, s1, e: string;
  n: Integer;
begin
  s := CharsetConversion(Value, CP, MimeP);
  s := EncodeSafeQuotedPrintable(s);
  e := GetIdFromCP(MimeP);
  s1 := '';
  Result := '';
  for n := 1 to Length(s) do
    if s[n] = ' ' then
    begin
//      s1 := s1 + '=20';
      s1 := s1 + '_';
      if Length(s1) > 32 then
      begin
        if Result <> '' then
          Result := Result + ' ';
        Result := Result + '=?' + e + '?Q?' + s1 + '?=';
        s1 := '';
      end;
    end
    else
      s1 := s1 + s[n];
  if s1 <> '' then
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + '=?' + e + '?Q?' + s1 + '?=';
  end;
end;

{==============================================================================}

function NeedInline(const Value: AnsiString): boolean;
var
  n: Integer;
begin
  Result := False;
  for n := 1 to Length(Value) do
    if Value[n] in (SpecialChar + NonAsciiChar - ['_']) then
    begin
      Result := True;
      Break;
    end;
end;

{==============================================================================}

function InlineCodeEx(const Value: string; FromCP: TMimeChar): string;
var
  c: TMimeChar;
begin
  if NeedInline(Value) then
  begin
    c := IdealCharsetCoding(Value, FromCP, IdealCharsets);
    Result := InlineEncode(Value, FromCP, c);
  end
  else
    Result := Value;
end;

{==============================================================================}

function InlineCode(const Value: string): string;
begin
  Result := InlineCodeEx(Value, GetCurCP);
end;

{==============================================================================}

function InlineEmailEx(const Value: string; FromCP: TMimeChar): string;
var
  sd, se: string;
begin
  sd := GetEmailDesc(Value);
  se := GetEmailAddr(Value);
  if sd = '' then
    Result := se
  else
    Result := '"' + InlineCodeEx(sd, FromCP) + '" <' + se + '>';
end;

{==============================================================================}

function InlineEmail(const Value: string): string;
begin
  Result := InlineEmailEx(Value, GetCurCP);
end;

end.
