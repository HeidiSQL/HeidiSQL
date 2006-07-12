{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           System Utility Classes and Functions          }
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

unit ZSysUtils;

interface

{$I ZCore.inc}

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ELSE}
  {$IFDEF FPC}
    Variants,
  {$ENDIF}
{$ENDIF}
  ZMessages, ZCompatibility, Classes, SysUtils;

type
  {** Modified comaprison function. }
  TZListSortCompare = function (Item1, Item2: Pointer): Integer of object;

  {** Modified list of pointers. }
  TZSortedList = class (TList)
  protected
    procedure QuickSort(SortList: PPointerList; L, R: Integer;
      SCompare: TZListSortCompare);
  public
    procedure Sort(Compare: TZListSortCompare);
  end;

{$IFDEF VER130BELOW}
const
  NullAsStringValue: string = '';
{$ENDIF}

{**
  Determines a position of a first delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the first found delimiter or 0 if no delimiters was found.
}
function FirstDelimiter(Delimiters: string; Str: string): Integer;

{**
  Determines a position of a LAST delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the last found delimiter or 0 if no delimiters was found.
}
function LastDelimiter(Delimiters: string; Str: string): Integer;

{**
  Checks is the string starts with substring.
  @param Str a string to be checked.
  @param SubStr a string to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}
function StartsWith(Str: string; SubStr: string): Boolean;

{**
  Checks is the string ends with substring.
  @param Str a string to be checked.
  @param SubStr a string to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(Str: string; SubStr: string): Boolean;

{**
  Converts SQL string into float value.
  @param Str an SQL string with comma delimiter.
  @param Def a default value if the string can not be converted.
  @return a converted value or Def if conversion was failt.
}
function SQLStrToFloatDef(Str: string; Def: Extended): Extended;

{**
  Converts SQL string into float value.
  @param Str an SQL string with comma delimiter.
  @return a converted value or Def if conversion was failt.
}
function SQLStrToFloat(Str: string): Extended;

{**
  Converts a character buffer into pascal string.
  @param Buffer a character buffer pointer.
  @param Length a buffer length.
  @return a string retrived from the buffer.
}
function BufferToStr(Buffer: PChar; Length: LongInt): string;

{**
  Converts a string into boolean value.
  @param Str a string value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/<>0
}
function StrToBoolEx(Str: string): Boolean;

{**
  Checks if the specified string can represent an IP address.
  @param Str a string value.
  @return <code>True</code> if the string can represent an IP address
    or <code>False</code> otherwise.
}
function IsIpAddr(Str: string): Boolean;

{**
  Splits string using the multiple chars.
  @param Str the source string
  @param Delimiters the delimiters string
  @return the result list where plased delimited string
}
function SplitString(Str, Delimiters: string): TStrings;

{**
  Puts to list a splitted string using the multiple chars which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure PutSplitString(List: TStrings; Str, Delimiters: string);

{**
  Appends to list a splitted string using the multiple chars.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitString(List: TStrings; Str, Delimiters: string);

{**
  Composes a string from the specified strings list delimited with
  a special character.
  @param List a list of strings.
  @param Delimiter a delimiter string.
  @return a composed string from the list.
}
function ComposeString(List: TStrings; Delimiter: string): string;

{**
  Converts a float value into SQL string with '.' delimiter.
  @param Value a float value to be converted.
  @return a converted string value.
}
function FloatToSQLStr(Value: Extended): string;

{**
  Puts to list a splitted string using the delimiter string which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiter string
}
procedure PutSplitStringEx(List: TStrings; Str, Delimiter: string);

{**
  Splits string using the delimiter string.
  @param Str the source string
  @param Delimiters the delimiter string
  @return the result list where plased delimited string
}
function SplitStringEx(Str, Delimiter: string): TStrings;

{**
  Appends to list a splitted string using the delimeter string.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitStringEx(List: TStrings; Str, Delimiter: string);

{**
  Converts bytes into a string representation.
  @param Value an array of bytes to be converted.
  @return a converted string.
}
function BytesToStr(Value: TByteDynArray): string;

{**
  Converts string into an array of bytes.
  @param Value a string to be converted.
  @return a converted array of bytes.
}
function StrToBytes(Value: string): TByteDynArray;

{**
  Converts bytes into a variant representation.
  @param Value an array of bytes to be converted.
  @return a converted variant.
}
function BytesToVar(Value: TByteDynArray): Variant;

{**
  Converts variant into an array of bytes.
  @param Value a varaint to be converted.
  @return a converted array of bytes.
}
function VarToBytes(Value: Variant): TByteDynArray;

{$IFDEF VER130BELOW}
{**
  Convert variant value to WideString
  @value Value a variant to be converted
  @return a converted WideString value
}
function VarToWideStr(const Value: Variant): WideString;

{**
  Convert variant value to WideString
  @value Value a variant to be converted
  @value Default a default value if convertion is not possible
  @return a converted WideString value
}
function VarToWideStrDef(const Value: Variant; const Default: WideString): WideString;

{**
  Convert string value to float value
  @value V a string value to be converted
  @value Default a default value if convertion is not possible
  @return a converted Extended value
}
function StrToFloatDef(const S: string; const Default: Extended): Extended;
{$ENDIF}

{**
  Converts Ansi SQL Date/Time to TDateTime
  @param Value a date and time string.
  @return a decoded TDateTime value.
}
function AnsiSQLDateToDateTime(Value: string): TDateTime;

{**
  Converts TDateTime to Ansi SQL Date/Time
  @param Value an encoded TDateTime value.
  @return a  date and time string.
}
function DateTimeToAnsiSQLDate(Value: TDateTime): string;

{**
  Converts an string into escape PostgreSQL format.
  @param Value a regular string.
  @return a string in PostgreSQL escape format.
}
function EncodeCString(Value: string): string;

{**
  Converts an string from escape PostgreSQL format.
  @param Value a string in PostgreSQL escape format.
  @return a regular string.
}
function DecodeCString(Value: string): string;

{**
  Replace chars in the string
  @param Source a char to search.
  @param Target a char to replace.
  @param Str a source string.
  @return a string with replaced chars.
}
function ReplaceChar(const Source, Target: Char; const Str: string): string;

{**
   Copy buffer to the pascal string
   @param Buffer a buffer with data
   @param Length a buffer length
   @return a bufeer content
}
function MemPas(Buffer: PChar; Length: LongInt): string;

implementation

uses ZMatchPattern;

{**
  Determines a position of a first delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the first found delimiter or 0 if no delimiters was found.
}
function FirstDelimiter(Delimiters: string; Str: string): Integer;
var
  I, Index: Integer;
begin
  Result := 0;
  for I := 1 to Length(Delimiters) do
  begin
    Index := Pos(Delimiters[I], Str);
    if (Index > 0) and ((Index < Result) or (Result = 0)) then
      Result := Index;
  end;
end;

{**
  Determines a position of a LAST delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the last found delimiter or 0 if no delimiters was found.
}
function LastDelimiter(Delimiters: string; Str: string): Integer;
var
  I, Index: Integer;
begin
  Result := 0;
  for I := Length(Str) downto 1 do
  begin
    Index := Pos(Str[I], Delimiters);
    if (Index > 0) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{**
  Checks is the string starts with substring.
  @param Str a string to be checked.
  @param SubStr a string to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}
function StartsWith(Str: string; SubStr: string): Boolean;
begin
  Result := Copy(Str, 1, Length(SubStr)) = SubStr;
end;

{**
  Checks is the string ends with substring.
  @param Str a string to be checked.
  @param SubStr a string to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(Str: string; SubStr: string): Boolean;
begin
  if Length(SubStr) <= Length(Str) then
  begin
    Result := Copy(Str, Length(Str) - Length(SubStr) + 1,
    Length(SubStr)) = SubStr;
  end else
    Result := False;
end;

{**
  Converts SQL string into float value.
  @param Str an SQL string with comma delimiter.
  @param Def a default value if the string can not be converted.
  @return a converted value or Def if conversion was failt.
}
function SQLStrToFloatDef(Str: string; Def: Extended): Extended;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  if Pos('$', Str) = 1 then
    Str := Copy(Str, 2, Pred(Length(Str)));
  Result := StrToFloatDef(Str, Def);
  DecimalSeparator := OldDecimalSeparator;
end;

{**
  Converts SQL string into float value.
  @param Str an SQL string with comma delimiter.
  @return a converted value or Def if conversion was failt.
}
function SQLStrToFloat(Str: string): Extended;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    Result := StrToFloat(Str);
  finally
    DecimalSeparator := OldDecimalSeparator;
  end;
end;

{ Convert string buffer into pascal string }
function BufferToStr(Buffer: PChar; Length: LongInt): string;
begin
  Result := '';
  if Assigned(Buffer) then
    SetString(Result, Buffer, Length);
end;

{**
  Converts a string into boolean value.
  @param Str a string value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/<>0
}
function StrToBoolEx(Str: string): Boolean;
begin
  Str := UpperCase(Str);
  Result := (Str = 'Y') or (Str = 'YES') or (Str = 'T') or (Str = 'TRUE')
    or (StrToIntDef(Str, 0) <> 0);
end;

{**
  Checks if the specified string can represent an IP address.
  @param Str a string value.
  @return <code>True</code> if the string can represent an IP address
    or <code>False</code> otherwise.
}
function IsIpAddr(Str: string): Boolean;
var
  I, N, M, Pos: Integer;
begin
  if IsMatch('*.*.*.*', Str) then
  begin
    N := 0;
    M := 0;
    Pos := 1;
    for I := 1 to Length(Str) do
    begin
      if I - Pos > 3 then
        Break;
      if Str[I] = '.' then begin
       if StrToInt(Copy(Str, Pos, I - Pos)) > 255 then
         Break;
       Inc(N);
       Pos := I + 1;
      end;
      if Str[I] in ['0'..'9'] then Inc(M);
    end;
    Result := (M + N = Length(Str)) and (N = 3);
  end else
    Result := False;
end;

{**
  Splits string using the multiple chars.
  @param Str the source string
  @param Delimiters the delimiters string
  @return the result list where plased delimited string
}
function SplitString(Str, Delimiters: string): TStrings;
var
  DelimPos: Integer;
begin
  Result := TStringList.Create;
  repeat
    DelimPos := FirstDelimiter(Delimiters, Str);
    if DelimPos > 0 then
    begin
      if DelimPos > 1 then
        Result.Add(Copy(Str, 1, DelimPos - 1));
      Str := Copy(Str, DelimPos + 1, Length(Str) - DelimPos);
    end else
      Break;
  until DelimPos <= 0;
  if Str <> '' then
    Result.Add(Str);
end;

{**
  Puts to list a splitted string using the multiple chars which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure PutSplitString(List: TStrings; Str, Delimiters: string);
var
  Temp: TStrings;
begin
  Temp := SplitString(Str, Delimiters);
  try
    List.Assign(Temp);
  finally
    Temp.Free;
  end;
end;

{**
  Appends to list a splitted string using the multiple chars.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitString(List: TStrings; Str, Delimiters: string);
var
  Temp: TStrings;
begin
  Temp := SplitString(Str, Delimiters);
  try
    List.AddStrings(Temp);
  finally
    Temp.Free;
  end;
end;

{**
  Composes a string from the specified strings list delimited with
  a special character.
  @param List a list of strings.
  @param Delimiter a delimiter string.
  @return a composed string from the list.
}
function ComposeString(List: TStrings; Delimiter: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + Delimiter;
    Result := Result + List[I];
  end;
end;

{**
  Converts a float value into SQL string with '.' delimiter.
  @param Value a float value to be converted.
  @return a converted string value.
}
function FloatToSQLStr(Value: Extended): string;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  Result := FloatToStr(Value);
  DecimalSeparator := OldDecimalSeparator;
end;

{**
  Puts to list a splitted string using the delimiter string which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiter string
}
procedure PutSplitStringEx(List: TStrings; Str, Delimiter: string);
var
  Temp: TStrings;
begin
  Temp := SplitStringEx(Str, Delimiter);
  try
    List.Clear;
    List.AddStrings(Temp);
  finally
    Temp.Free;
  end;
end;

{**
  Splits string using the delimiter string.
  @param Str the source string
  @param Delimiters the delimiter string
  @return the result list where plased delimited string
}
function SplitStringEx(Str, Delimiter: string): TStrings;
var
 Pos: integer;
 Temp: string;
begin
  Result := TStringList.Create;
  Temp := Str;
  repeat
    Pos := AnsiPos(Delimiter, Temp);
    Result.Add(Copy(Temp, 1, Pos - 1));
    Delete(Temp, 1, Pos + Length(Delimiter) - 1);
  until Pos = 0;
  if Temp <> '' then
    Result.Add(Temp);
end;

{**
  Appends to list a splitted string using the delimeter string.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitStringEx(List: TStrings; Str, Delimiter: string);
var
  Temp: TStrings;
begin
  Temp := SplitStringEx(Str, Delimiter);
  try
    List.AddStrings(Temp);
  finally
    Temp.Free;
  end;
end;

{**
  Converts bytes into a string representation.
  @param Value an array of bytes to be converted.
  @return a converted string.
}
function BytesToStr(Value: TByteDynArray): string;
var
  I: Integer;
begin
  SetLength(Result, Length(Value));
  for I := 1 to Length(Value) do
    Result[I] := Char(Value[I - 1]);
end;

{**
  Converts string into an array of bytes.
  @param Value a string to be converted.
  @return a converted array of bytes.
}
function StrToBytes(Value: string): TByteDynArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Value));
  for I := 1 to Length(Value) do
    Result[I - 1] := Ord(Value[I]);
end;

{**
  Converts bytes into a variant representation.
  @param Value an array of bytes to be converted.
  @return a converted variant.
}
function BytesToVar(Value: TByteDynArray): Variant;
var
  I: Integer;
begin
  Result := VarArrayCreate([0, Length(Value) - 1], varByte);
  for I := 0 to Length(Value) - 1 do
    Result[I] := Value[I];
end;

{**
  Converts variant into an array of bytes.
  @param Value a varaint to be converted.
  @return a converted array of bytes.
}
function VarToBytes(Value: Variant): TByteDynArray;
var
  I: Integer;
begin
  if not (VarIsArray(Value) and (VarArrayDimCount(Value) = 1) and
     ((VarType(Value) and VarTypeMask) = varByte)) then
    raise Exception.Create(SInvalidVarByteArray);

  SetLength(Result, VarArrayHighBound(Value, 1) + 1);
  for I := 0 to VarArrayHighBound(Value, 1) do
    Result[I] := Value[I];
end;

{$IFDEF VER130BELOW}
{**
  Convert variant value to WideString
  @value V a variant to be converted
  @return a converted WideString value
}
function VarToWideStr(const Value: Variant): WideString;
begin
  Result := VarToWideStrDef(Value, NullAsStringValue);
end;

{**
  Convert variant value to WideString
  @value Value a variant to be converted
  @value Default a default value if convertion is not possible
  @return a converted WideString value
}
function VarToWideStrDef(const Value: Variant; const Default: WideString): WideString;
begin
  if not VarIsNull(Value) then
    Result := Value
  else
    Result := Default;
end;

{**
  Convert string value to float value
  @value V a string value to be converted
  @value Default a default value if convertion is not possible
  @return a converted Extended value
}
function StrToFloatDef(const S: string; const Default: Extended): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended) then
    Result := Default;
end;
{$ENDIF}

{**
  Converts Ansi SQL Date/Time to TDateTime
  @param Value a date and time string.
  @return a decoded TDateTime value.
}
function AnsiSQLDateToDateTime(Value: string): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec: Word;
  Temp: string;
begin
  Temp := Value;
  Result := 0;
  if Length(Temp) >= 10 then
  begin
    Year := StrToIntDef(Copy(Temp, 1, 4), 0);
    Month := StrToIntDef(Copy(Temp, 6, 2), 0);
    Day := StrToIntDef(Copy(Temp, 9, 2), 0);

    if (Year <> 0) and (Month <> 0) and (Day <> 0) then
    begin
      try
        Result := EncodeDate(Year, Month, Day);
      except
      end;
    end;
    Temp := Copy(Temp, 12, 8);
  end;
  if Length(Temp) >= 8 then
  begin
    Hour := StrToIntDef(Copy(Temp, 1, 2), 0);
    Min := StrToIntDef(Copy(Temp, 4, 2), 0);
    Sec := StrToIntDef(Copy(Temp, 7, 2), 0);
    try
      if Result >= 0 then
        Result := Result + EncodeTime(Hour, Min, Sec, 0)
      else Result := Result - EncodeTime(Hour, Min, Sec, 0)
    except
    end;
  end;
end;

{**
  Converts TDateTime to Ansi SQL Date/Time
  @param Value an encoded TDateTime value.
  @return a  date and time string.
}
function DateTimeToAnsiSQLDate(Value: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Value);
end;

{ TZSortedList }

{**
  Performs quick sort algorithm for the list.
}
procedure TZSortedList.QuickSort(SortList: PPointerList; L, R: Integer;
  SCompare: TZListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do
        Inc(I);
      while SCompare(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

{**
  Performs sorting for this list.
  @param Compare a comparison function.
}
procedure TZSortedList.Sort(Compare: TZListSortCompare);
begin
  if (List <> nil) and (Count > 0) then
    QuickSort(List, 0, Count - 1, Compare);
end;

{**
  Converts an string into escape PostgreSQL format.
  @param Value a regular string.
  @return a string in PostgreSQL escape format.
}
function EncodeCString(Value: string): string;
var
  I: Integer;
  SrcLength, DestLength: Integer;
  SrcBuffer, DestBuffer: PChar;
begin
  SrcLength := Length(Value);
  SrcBuffer := PChar(Value);
  DestLength := 0;
  for I := 1 to SrcLength do
  begin
    if SrcBuffer^ in [#0] then
      Inc(DestLength, 4)
    else if SrcBuffer^ in ['"', '''', '\'] then
      Inc(DestLength, 2)
    else Inc(DestLength);
    Inc(SrcBuffer);
  end;

  SrcBuffer := PChar(Value);
  SetLength(Result, DestLength);
  DestBuffer := PChar(Result);

  for I := 1 to SrcLength do
  begin
    if SrcBuffer^ in [#0] then
    begin
      DestBuffer[0] := '\';
      DestBuffer[1] := Chr(Ord('0') + (Byte(SrcBuffer^) shr 6));
      DestBuffer[2] := Chr(Ord('0') + ((Byte(SrcBuffer^) shr 3) and $07));
      DestBuffer[3] := Chr(Ord('0') + (Byte(SrcBuffer^) and $07));
      Inc(DestBuffer, 4);
    end
    else if SrcBuffer^ in ['"', '''', '\'] then
    begin
      DestBuffer[0] := '\';
      DestBuffer[1] := SrcBuffer^;
      Inc(DestBuffer, 2);
    end
    else
    begin
      DestBuffer^ := SrcBuffer^;
      Inc(DestBuffer);
    end;
    Inc(SrcBuffer);
  end;
end;

{**
  Converts an string from escape PostgreSQL format.
  @param Value a string in PostgreSQL escape format.
  @return a regular string.
}
function DecodeCString(Value: string): string;
var
  SrcLength, DestLength: Integer;
  SrcBuffer, DestBuffer: PChar;
begin
  SrcLength := Length(Value);
  SrcBuffer := PChar(Value);
  SetLength(Result, SrcLength);
  DestLength := 0;
  DestBuffer := PChar(Result);

  while SrcLength > 0 do
  begin
    if SrcBuffer^ = '\' then
    begin
      Inc(SrcBuffer);
      if SrcBuffer^ in ['0'..'9'] then
      begin
        DestBuffer^ := Chr(((Byte(SrcBuffer[0]) - Ord('0')) shl 6)
          or ((Byte(SrcBuffer[1]) - Ord('0')) shl 3)
          or ((Byte(SrcBuffer[2]) - Ord('0'))));
        Inc(SrcBuffer, 3);
        Dec(SrcLength, 4);
      end
      else
      begin
        case SrcBuffer^ of
          'r': DestBuffer^ := #13;
          'n': DestBuffer^ := #10;
          't': DestBuffer^ := #9;
          else DestBuffer^ := SrcBuffer^;
        end;
        Inc(SrcBuffer);
        Dec(SrcLength, 2);
      end
    end
    else
    begin
      DestBuffer^ := SrcBuffer^;
      Inc(SrcBuffer);
      Dec(SrcLength);
    end;
    Inc(DestBuffer);
    Inc(DestLength);
  end;
  SetLength(Result, DestLength);
end;


{**
  Replace chars in the string
  @param Source a char to search.
  @param Target a char to replace.
  @param Str a source string.
  @return a string with replaced chars.
}
function ReplaceChar(const Source, Target: Char; const Str: string): string;
var
 I: integer;
begin
  Result := Str;
  for i := 1 to Length(Str) do
    if Result[I] = Source then
      Result[I] := Target;
end;

{**
   Copy buffer to the pascal string
   @param Buffer a buffer with data
   @param Length a buffer length
   @return a bufeer content
}
function MemPas(Buffer: PChar; Length: LongInt): string;
begin
  Result := '';
  if Assigned(Buffer) then
    SetString(Result, Buffer, Length);
end;

end.
