
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntSystem;

{$INCLUDE compilers.inc}

{*****************************************************************************}
{  Special thanks go to Francisco Leong for originating the design for        }
{    WideString-enabled resourcestrings.                                      }
{*****************************************************************************}

interface

uses
  Windows;

// These functions should not be used by Delphi code since conversions are implicit.
{TNT-WARN WideCharToString}
{TNT-WARN WideCharLenToString}
{TNT-WARN WideCharToStrVar}
{TNT-WARN WideCharLenToStrVar}
{TNT-WARN StringToWideChar}

// ................ ANSI TYPES ................
{TNT-WARN Char}
{TNT-WARN PChar}
{TNT-WARN String}

{TNT-WARN CP_ACP} // <-- use DefaultSystemCodePage
function DefaultSystemCodePage: Cardinal; // implicitly used when converting AnsiString <--> WideString.

var
  WideCustomLoadResString: function(ResStringRec: PResStringRec; var Value: WideString): Boolean;

{TNT-WARN LoadResString}
function WideLoadResString(ResStringRec: PResStringRec): WideString;
{TNT-WARN ParamCount}
function WideParamCount: Integer;
{TNT-WARN ParamStr}
function WideParamStr(Index: Integer): WideString;

// ......... introduced .........

const
  { Each Unicode stream should begin with the code U+FEFF,  }
  {   which the standard defines as the *byte order mark*.  }
  UNICODE_BOM = WideChar($FEFF);
  UNICODE_BOM_SWAPPED = WideChar($FFFE);
  UTF8_BOM = AnsiString(#$EF#$BB#$BF);

function WideStringToUTF8(const S: WideString): AnsiString;
function UTF8ToWideString(const S: AnsiString): WideString;

function WideStringToUTF7(const W: WideString): AnsiString;
function UTF7ToWideString(const S: AnsiString): WideString;

function StringToWideStringEx(const S: AnsiString; CodePage: Cardinal): WideString;
function WideStringToStringEx(const WS: WideString; CodePage: Cardinal): AnsiString;

function UCS2ToWideString(const Value: AnsiString): WideString;
function WideStringToUCS2(const Value: WideString): AnsiString;

function CharSetToCodePage(ciCharset: UINT): Cardinal;
function LCIDToCodePage(ALcid: LCID): Cardinal;
function KeyboardCodePage: Cardinal;
function KeyUnicode(CharCode: Word): WideChar;

procedure StrSwapByteOrder(Str: PWideChar);

type
  TTntSystemUpdate =
    (tsWideResourceStrings,
     {$IFNDEF COMPILER_9_UP}tsFixImplicitCodePage, tsFixWideStrConcat, tsFixWideFormat, {$ENDIF}
     tsWideExceptions
    );
  TTntSystemUpdateSet = set of TTntSystemUpdate;

const
  AllTntSystemUpdates = [Low(TTntSystemUpdate)..High(TTntSystemUpdate)];

procedure InstallTntSystemUpdates(Updates: TTntSystemUpdateSet = AllTntSystemUpdates);

implementation

uses
  SysUtils, Variants, Forms, TntWindows, TntSysUtils, TntForms;

var
  GDefaultSystemCodePage: Cardinal;

function DefaultSystemCodePage: Cardinal;
begin
  Result := GDefaultSystemCodePage;
end;

var
  IsDebugging: Boolean;

function WideLoadResString(ResStringRec: PResStringRec): WideString;
const
  MAX_RES_STRING_SIZE = 4097; { MSDN documents this as the maximum size of a string in table. }
var
  Buffer: array [0..MAX_RES_STRING_SIZE] of WideChar; { Buffer leaves room for null terminator. }
  PCustom: PAnsiChar;
begin
  if Assigned(WideCustomLoadResString) and WideCustomLoadResString(ResStringRec, Result) then
    exit; { a custom resourcestring has been loaded. }

  if ResStringRec = nil then
    Result := ''
  else if ResStringRec.Identifier < 64*1024 then
    SetString(Result, Buffer,
      Tnt_LoadStringW(FindResourceHInstance(ResStringRec.Module^),
        ResStringRec.Identifier, Buffer, MAX_RES_STRING_SIZE))
  else begin
    // custom string pointer
    PCustom := PAnsiChar(ResStringRec.Identifier); { I would like to use PWideChar, but this would break legacy code. }
    if  (StrLen{TNT-ALLOW StrLen}(PCustom) > Cardinal(Length(UTF8_BOM)))
    and CompareMem(PCustom, PAnsiChar(UTF8_BOM), Length(UTF8_BOM)) then
      // detected UTF8
      Result := UTF8ToWideString(PAnsiChar(PCustom + Length(UTF8_BOM)))
    else
      // normal
      Result := PCustom;
  end;
end;

function WideGetParamStr(P: PWideChar; var Param: WideString): PWideChar;
var
  i, Len: Integer;
  Start, S, Q: PWideChar;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      Inc(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  Start := P;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := P + 1;
        Inc(Len, Q - P);
        P := Q;
      end;
      if P[0] <> #0 then
        Inc(P);
    end
    else
    begin
      Q := P + 1;
      Inc(Len, Q - P);
      P := Q;
    end;
  end;

  SetLength(Param, Len);

  P := Start;
  S := PWideChar(Param);
  i := 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := P + 1;
        while P < Q do
        begin
          S[i] := P^;
          Inc(P);
          Inc(i);
        end;
      end;
      if P[0] <> #0 then Inc(P);
    end
    else
    begin
      Q := P + 1;
      while P < Q do
      begin
        S[i] := P^;
        Inc(P);
        Inc(i);
      end;
    end;
  end;

  Result := P;
end;

function WideParamCount: Integer;
var
  P: PWideChar;
  S: WideString;
begin
  P := WideGetParamStr(GetCommandLineW, S);
  Result := 0;
  while True do
  begin
    P := WideGetParamStr(P, S);
    if S = '' then Break;
    Inc(Result);
  end;
end;

function WideParamStr(Index: Integer): WideString;
var
  P: PWideChar;
begin
  if Index = 0 then
    Result := WideGetModuleFileName(0)
  else
  begin
    P := GetCommandLineW;
    while True do
    begin
      P := WideGetParamStr(P, Result);
      if (Index = 0) or (Result = '') then Break;
      Dec(Index);
    end;
  end;
end;

function WideStringToUTF8(const S: WideString): AnsiString;
begin
  Result := UTF8Encode(S);
end;

function UTF8ToWideString(const S: AnsiString): WideString;
begin
  Result := UTF8Decode(S);
end;

  { ======================================================================= }
  { Original File:   ConvertUTF7.c                                          }
  { Author: David B. Goldsmith                                              }
  { Copyright (C) 1994, 1996 Taligent, Inc. All rights reserved.            }
  {                                                                         }
  { This code is copyrighted. Under the copyright laws, this code may not   }
  { be copied, in whole or part, without prior written consent of Taligent. }
  {                                                                         }
  { Taligent grants the right to use this code as long as this ENTIRE       }
  { copyright notice is reproduced in the code.  The code is provided       }
  { AS-IS, AND TALIGENT DISCLAIMS ALL WARRANTIES, EITHER EXPRESS OR         }
  { IMPLIED, INCLUDING, BUT NOT LIMITED TO IMPLIED WARRANTIES OF            }
  { MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT      }
  { WILL TALIGENT BE LIABLE FOR ANY DAMAGES WHATSOEVER (INCLUDING,          }
  { WITHOUT LIMITATION, DAMAGES FOR LOSS OF BUSINESS PROFITS, BUSINESS      }
  { INTERRUPTION, LOSS OF BUSINESS INFORMATION, OR OTHER PECUNIARY          }
  { LOSS) ARISING OUT OF THE USE OR INABILITY TO USE THIS CODE, EVEN        }
  { IF TALIGENT HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.        }
  { BECAUSE SOME STATES DO NOT ALLOW THE EXCLUSION OR LIMITATION OF         }
  { LIABILITY FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES, THE ABOVE            }
  { LIMITATION MAY NOT APPLY TO YOU.                                        }
  {                                                                         }
  { RESTRICTED RIGHTS LEGEND: Use, duplication, or disclosure by the        }
  { government is subject to restrictions as set forth in subparagraph      }
  { (c)(l)(ii) of the Rights in Technical Data and Computer Software        }
  { clause at DFARS 252.227-7013 and FAR 52.227-19.                         }
  {                                                                         }
  { This code may be protected by one or more U.S. and International        }
  { Patents.                                                                }
  {                                                                         }
  { TRADEMARKS: Taligent and the Taligent Design Mark are registered        }
  { trademarks of Taligent, Inc.                                            }
  { ======================================================================= }

type UCS2 = Word;

const
  _base64: AnsiString = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  _direct: AnsiString = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789''(),-./:?';
  _optional: AnsiString = '!"#$%&*;<=>@[]^_`{|}';
  _spaces: AnsiString = #9#13#10#32;

var
  base64: PAnsiChar;
  invbase64: array[0..127] of SmallInt;
  direct: PAnsiChar;
  optional: PAnsiChar;
  spaces: PAnsiChar;
  mustshiftsafe: array[0..127] of AnsiChar;
  mustshiftopt: array[0..127] of AnsiChar;

var
  needtables: Boolean = True;

procedure Initialize_UTF7_Data;
begin
  base64 := PAnsiChar(_base64);
  direct := PAnsiChar(_direct);
  optional := PAnsiChar(_optional);
  spaces := PAnsiChar(_spaces);
end;

procedure tabinit;
var
  i: Integer;
  limit: Integer;
begin
  i := 0;
  while (i < 128) do
  begin
    mustshiftopt[i] := #1;
    mustshiftsafe[i] := #1;
    invbase64[i] := -1;
    Inc(i);
  end { For };
  limit := Length(_Direct);
  i := 0;
  while (i < limit) do
  begin
    mustshiftopt[Integer(direct[i])] := #0;
    mustshiftsafe[Integer(direct[i])] := #0;
    Inc(i);
  end { For };
  limit := Length(_Spaces);
  i := 0;
  while (i < limit) do
  begin
    mustshiftopt[Integer(spaces[i])] := #0;
    mustshiftsafe[Integer(spaces[i])] := #0;
    Inc(i);
  end { For };
  limit := Length(_Optional);
  i := 0;
  while (i < limit) do
  begin
    mustshiftopt[Integer(optional[i])] := #0;
    Inc(i);
  end { For };
  limit := Length(_Base64);
  i := 0;
  while (i < limit) do
  begin
    invbase64[Integer(base64[i])] := i;
    Inc(i);
  end { For };
  needtables := False;
end; { tabinit }

function WRITE_N_BITS(x: UCS2; n: Integer; var BITbuffer: Cardinal; var bufferbits: Integer): Integer;
begin
  BITbuffer := BITbuffer or (x and (not (-1 shl n))) shl (32 - n - bufferbits);
  bufferbits := bufferbits + n;
  Result := bufferbits;
end; { WRITE_N_BITS }

function READ_N_BITS(n: Integer; var BITbuffer: Cardinal; var bufferbits: Integer): UCS2;
var
  buffertemp: Cardinal;
begin
  buffertemp := BITbuffer shr (32 - n);
  BITbuffer := BITbuffer shl n;
  bufferbits := bufferbits - n;
  Result := UCS2(buffertemp);
end; { READ_N_BITS }

function ConvertUCS2toUTF7(var sourceStart: PWideChar; sourceEnd: PWideChar;
  var targetStart: PAnsiChar; targetEnd: PAnsiChar; optional: Boolean;
    verbose: Boolean): Integer;
var
  r: UCS2;
  target: PAnsiChar;
  source: PWideChar;
  BITbuffer: Cardinal;
  bufferbits: Integer;
  shifted: Boolean;
  needshift: Boolean;
  done: Boolean;
  mustshift: PAnsiChar;
begin
  Initialize_UTF7_Data;
  Result := 0;
  BITbuffer := 0;
  bufferbits := 0;
  shifted := False;
  source := sourceStart;
  target := targetStart;
  r := 0;
  if needtables then
    tabinit;
  if optional then
    mustshift := @mustshiftopt[0]
  else
    mustshift := @mustshiftsafe[0];
  repeat
    done := source >= sourceEnd;
    if not Done then
    begin
      r := Word(source^);
      Inc(Source);
    end { If };
    needshift := (not done) and ((r > $7F) or (mustshift[r] <> #0));
    if needshift and (not shifted) then
    begin
      if (Target >= TargetEnd) then
      begin
        Result := 2;
        break;
      end { If };
      target^ := '+';
      Inc(target);
      { Special case handling of the SHIFT_IN character }
      if (r = UCS2('+')) then
      begin
        if (target >= targetEnd) then
        begin
          Result := 2;
          break;
        end;
        target^ := '-';
        Inc(target);
      end
      else
        shifted := True;
    end { If };
    if shifted then
    begin
      { Either write the character to the bit buffer, or pad }
      { the bit buffer out to a full base64 character. }
      { }
      if needshift then
        WRITE_N_BITS(r, 16, BITbuffer, bufferbits)
      else
        WRITE_N_BITS(0, (6 - (bufferbits mod 6)) mod 6, BITbuffer,
          bufferbits);
      { Flush out as many full base64 characters as possible }
      { from the bit buffer. }
      { }
      while (target < targetEnd) and (bufferbits >= 6) do
      begin
        Target^ := base64[READ_N_BITS(6, BITbuffer, bufferbits)];
        Inc(Target);
      end { While };
      if (bufferbits >= 6) then
      begin
        if (target >= targetEnd) then
        begin
          Result := 2;
          break;
        end { If };
      end { If };
      if (not needshift) then
      begin
        { Write the explicit shift out character if }
        { 1) The caller has requested we always do it, or }
        { 2) The directly encoded character is in the }
        { base64 set, or }
        { 3) The directly encoded character is SHIFT_OUT. }
        { }
        if verbose or ((not done) and ((invbase64[r] >= 0) or (r =
          Integer('-')))) then
        begin
          if (target >= targetEnd) then
          begin
            Result := 2;
            Break;
          end { If };
          Target^ := '-';
          Inc(Target);
        end { If };
        shifted := False;
      end { If };
      { The character can be directly encoded as ASCII. }
    end { If };
    if (not needshift) and (not done) then
    begin
      if (target >= targetEnd) then
      begin
        Result := 2;
        break;
      end { If };
      Target^ := AnsiChar(r);
      Inc(Target);
    end { If };
  until (done);
  sourceStart := source;
  targetStart := target;
end; { ConvertUCS2toUTF7 }

function ConvertUTF7toUCS2(var sourceStart: PAnsiChar; sourceEnd: PAnsiChar;
  var targetStart: PWideChar; targetEnd: PWideChar): Integer;
var
  target: PWideChar { Register };
  source: PAnsiChar { Register };
  BITbuffer: Cardinal { & "Address Of" Used };
  bufferbits: Integer { & "Address Of" Used };
  shifted: Boolean { Used In Boolean Context };
  first: Boolean { Used In Boolean Context };
  wroteone: Boolean;
  base64EOF: Boolean;
  base64value: Integer;
  done: Boolean;
  c: UCS2;
  prevc: UCS2;
  junk: UCS2 { Used In Boolean Context };
begin
  Initialize_UTF7_Data;
  Result := 0;
  BITbuffer := 0;
  bufferbits := 0;
  shifted := False;
  first := False;
  wroteone := False;
  source := sourceStart;
  target := targetStart;
  c := 0;
  if needtables then
    tabinit;
  repeat
    { read an ASCII character c }
    done := Source >= SourceEnd;
    if (not done) then
    begin
      c := Word(Source^);
      Inc(Source);
    end { If };
    if shifted then
    begin
      { We're done with a base64 string if we hit EOF, it's not a valid }
      { ASCII character, or it's not in the base64 set. }
      { }
      base64value := invbase64[c];
      base64EOF := (done or (c > $7F)) or (base64value < 0);
      if base64EOF then
      begin
        shifted := False;
        { If the character causing us to drop out was SHIFT_IN or }
        { SHIFT_OUT, it may be a special escape for SHIFT_IN. The }
        { test for SHIFT_IN is not necessary, but allows an alternate }
        { form of UTF-7 where SHIFT_IN is escaped by SHIFT_IN. This }
        { only works for some values of SHIFT_IN. }
        { }
        if ((not done) and ((c = Integer('+')) or (c = Integer('-')))) then
        begin
          { get another character c }
          prevc := c;
          Done := Source >= SourceEnd;
          if (not Done) then
          begin
            c := Word(Source^);
            Inc(Source);
            { If no base64 characters were encountered, and the }
            { character terminating the shift sequence was }
            { SHIFT_OUT, then it's a special escape for SHIFT_IN. }
            { }
          end;
          if first and (prevc = Integer('-')) then
          begin
            { write SHIFT_IN unicode }
            if (target >= targetEnd) then
            begin
              Result := 2;
              break;
            end { If };
            Target^ := WideChar('+');
            Inc(Target);
          end
          else
          begin
            if (not wroteone) then
            begin
              Result := 1;
            end { If };
          end { Else };
          ;
        end { If }
        else
        begin
          if (not wroteone) then
          begin
            Result := 1;
          end { If };
        end { Else };
      end { If }
      else
      begin
        { Add another 6 bits of base64 to the bit buffer. }
        WRITE_N_BITS(base64value, 6, BITbuffer,
          bufferbits);
        first := False;
      end { Else };
      { Extract as many full 16 bit characters as possible from the }
      { bit buffer. }
      { }
      while (bufferbits >= 16) and (target < targetEnd) do
      begin
        { write a unicode }
        Target^ := WideChar(READ_N_BITS(16, BITbuffer, bufferbits));
        Inc(Target);
        wroteone := True;
      end { While };
      if (bufferbits >= 16) then
      begin
        if (target >= targetEnd) then
        begin
          Result := 2;
          Break;
        end;
      end { If };
      if (base64EOF) then
      begin
        junk := READ_N_BITS(bufferbits, BITbuffer, bufferbits);
        if (junk <> 0) then
        begin
          Result := 1;
        end { If };
      end { If };
    end { If };
    if (not shifted) and (not done) then
    begin
      if (c = Integer('+')) then
      begin
        shifted := True;
        first := True;
        wroteone := False;
      end { If }
      else
      begin
        { It must be a directly encoded character. }
        if (c > $7F) then
        begin
          Result := 1;
        end { If };
        if (target >= targetEnd) then
        begin
          Result := 2;
          break;
        end { If };
        Target^ := WideChar(c);
        Inc(Target);
      end { Else };
    end { If };
  until (done);
  sourceStart := source;
  targetStart := target;
end; { ConvertUTF7toUCS2 }

  {*****************************************************************************}
  { Thanks to Francisco Leong for providing the Pascal conversion of            }
  {   ConvertUTF7.c (by David B. Goldsmith)                                     }
  {*****************************************************************************}

resourcestring
  SBufferOverflow = 'Buffer overflow';
  SInvalidUTF7 = 'Invalid UTF7';

function WideStringToUTF7(const W: WideString): AnsiString;
var
  SourceStart, SourceEnd: PWideChar;
  TargetStart, TargetEnd: PAnsiChar;
begin
  if W = '' then
    Result := ''
  else
  begin
    SetLength(Result, Length(W) * 7); // Assume worst case
    SourceStart := PWideChar(@W[1]);
    SourceEnd := PWideChar(@W[Length(W)]) + 1;
    TargetStart := PAnsiChar(@Result[1]);
    TargetEnd := PAnsiChar(@Result[Length(Result)]) + 1;
    if ConvertUCS2toUTF7(SourceStart, SourceEnd, TargetStart,
      TargetEnd, True, False) <> 0
    then
      raise ETntInternalError.Create(SBufferOverflow);
    SetLength(Result, TargetStart - PAnsiChar(@Result[1]));
  end;
end;

function UTF7ToWideString(const S: AnsiString): WideString;
var
  SourceStart, SourceEnd: PAnsiChar;
  TargetStart, TargetEnd: PWideChar;
begin
  if (S = '') then
    Result := ''
  else
  begin
    SetLength(Result, Length(S)); // Assume Worst case
    SourceStart := PAnsiChar(@S[1]);
    SourceEnd := PAnsiChar(@S[Length(S)]) + 1;
    TargetStart := PWideChar(@Result[1]);
    TargetEnd := PWideChar(@Result[Length(Result)]) + 1;
    case ConvertUTF7toUCS2(SourceStart, SourceEnd, TargetStart,
      TargetEnd) of
      1: raise ETntGeneralError.Create(SInvalidUTF7);
      2: raise ETntInternalError.Create(SBufferOverflow);
    end;
    SetLength(Result, TargetStart - PWideChar(@Result[1]));
  end;
end;

function StringToWideStringEx(const S: AnsiString; CodePage: Cardinal): WideString;
var
  InputLength,
  OutputLength: Integer;
begin
  if CodePage = CP_UTF7 then
    Result := UTF7ToWideString(S) // CP_UTF7 not supported on Windows 95
  else if CodePage = CP_UTF8 then
    Result := UTF8ToWideString(S) // CP_UTF8 not supported on Windows 95
  else begin
    InputLength := Length(S);
    OutputLength := MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength, nil, 0);
    SetLength(Result, OutputLength);
    MultiByteToWideChar(CodePage, 0, PAnsiChar(S), InputLength, PWideChar(Result), OutputLength);
  end;
end;

function WideStringToStringEx(const WS: WideString; CodePage: Cardinal): AnsiString;
var
  InputLength,
  OutputLength: Integer;
begin
  if CodePage = CP_UTF7 then
    Result := WideStringToUTF7(WS) // CP_UTF7 not supported on Windows 95
  else if CodePage = CP_UTF8 then
    Result := WideStringToUTF8(WS) // CP_UTF8 not supported on Windows 95
  else begin
    InputLength := Length(WS);
    OutputLength := WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, nil, 0, nil, nil);
    SetLength(Result, OutputLength);
    WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, PAnsiChar(Result), OutputLength, nil, nil);
  end;
end;

function UCS2ToWideString(const Value: AnsiString): WideString;
begin
  if Length(Value) = 0 then
    Result := ''
  else
    SetString(Result, PWideChar(@Value[1]), Length(Value) div SizeOf(WideChar))
end;

function WideStringToUCS2(const Value: WideString): AnsiString;
begin
  if Length(Value) = 0 then
    Result := ''
  else
    SetString(Result, PAnsiChar(@Value[1]), Length(Value) * SizeOf(WideChar))
end;

{ Windows.pas doesn't declare TranslateCharsetInfo() correctly. }
function TranslateCharsetInfo(lpSrc: PDWORD; var lpCs: TCharsetInfo; dwFlags: DWORD): BOOL; stdcall; external gdi32 name 'TranslateCharsetInfo';

function CharSetToCodePage(ciCharset: UINT): Cardinal;
var
  C: TCharsetInfo;
begin
  TntWin32Check(TranslateCharsetInfo(PDWORD(ciCharset), C, TCI_SRCCHARSET));
  Result := C.ciACP
end;

function LCIDToCodePage(ALcid: LCID): Cardinal;
var
  Buf: array[0..6] of AnsiChar;
begin
  GetLocaleInfo(ALcid, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result := StrToIntDef(Buf, GetACP);
end;

function KeyboardCodePage: Cardinal;
begin
  Result := LCIDToCodePage(GetKeyboardLayout(0) and $FFFF);
end;

function KeyUnicode(CharCode: Word): WideChar;
var
  AChar: AnsiChar;
begin
  // converts the given character (as it comes with a WM_CHAR message) into its
  // corresponding Unicode character depending on the active keyboard layout
  if CharCode <= Word(High(AnsiChar)) then begin
    AChar := AnsiChar(CharCode);
    MultiByteToWideChar(KeyboardCodePage, MB_USEGLYPHCHARS, @AChar, 1, @Result, 1);
  end else
    Result := WideChar(CharCode);
end;

procedure StrSwapByteOrder(Str: PWideChar);
var
  P: PWord;
begin
  P := PWord(Str);
  While (P^ <> 0) do begin
    P^ := MakeWord(HiByte(P^), LoByte(P^));
    Inc(P);
  end;
end;

//--------------------------------------------------------------------
//                LoadResString()
//
//  This system function is used to retrieve a resourcestring and
//   return the result as an AnsiString.  If we believe that the result
//    is only a temporary value, and that it will be immediately
//     assigned to a WideString or a Variant, then we will save the
//      Unicode result as well as a reference to the original Ansi string.
//       WStrFromPCharLen() or VarFromLStr() will return this saved
//        Unicode string if it appears to receive the most recent result
//         of LoadResString.
//--------------------------------------------------------------------


  //===========================================================================================
  //
  //    function CodeMatchesPatternForUnicode(...);
  //
  //    GIVEN:  SomeWideString := SSomeResString;  { WideString := resourcestring }
  //
  //    Delphi will compile this statement into the following:
  //    -------------------------------------------------
  //    TempAnsiString := LoadResString(@SSomeResString);
  //      LINE 1:  lea edx,[SomeTempAnsiString]
  //      LINE 2:  mov eax,[@SomeResString]
  //      LINE 3:  call LoadResString
  //
  //    WStrFromLStr(SomeWideString, TempAnsiString);  { SomeWideString := TempAnsiString }
  //      LINE 4:  mov edx,[SomeTempAnsiString]
  //      LINE 5:  mov/lea eax [@SomeWideString]
  //      LINE 6:  call @WStrFromLStr
  //    -------------------------------------------------
  //
  //    The order in which the parameters are prepared for WStrFromLStr (ie LINE 4 & 5) is
  //      reversed when assigning a non-temporary AnsiString to a WideString.
  //
  //    This code, for example, results in LINE 4 and LINE 5 being swapped.
  //
  //      SomeAnsiString := SSomeResString;
  //      SomeWideString := SomeAnsiString;
  //
  //    Since we know the "signature" used by the compiler, we can detect this pattern.
  //     If we believe it is only temporary, we can save the Unicode results for later
  //      retrieval from WStrFromLStr.
  //
  //    One final note:  When assigning a resourcestring to a Variant, the same patterns exist.
  //===========================================================================================

function CodeMatchesPatternForUnicode(PLine4: PAnsiChar): Boolean;
const
  SIZEOF_OPCODE = 1 {byte};
  MOV_16_OPCODE = AnsiChar($8B); { we'll assume operand size is 16 bits }
  MOV_32_OPCODE = AnsiChar($B8); { we'll assume operand size is 32 bits }
  LEA_OPCODE    = AnsiChar($8D); { operand size can be 16 or 40 bits }
  CALL_OPCODE   = AnsiChar($E8); { assumed operand size is 32 bits }
  BREAK_OPCODE  = AnsiChar($CC); {in a breakpoint}
var
  PLine1: PAnsiChar;
  PLine2: PAnsiChar;
  PLine3: PAnsiChar;
  DataSize: Integer; // bytes in first LEA operand
begin
  Result := False;

  PLine3 := PLine4 - SizeOf(CALL_OPCODE) - 4;
  PLine2 := PLine3 - SizeOf(MOV_32_OPCODE) - 4;

  // figure PLine1 and operand size
  DataSize := 2; { try 16 bit operand for line 1 }
  PLine1 := PLine2 - DataSize - SizeOf(LEA_OPCODE);
  if (PLine1^ <> LEA_OPCODE) and (not (IsDebugging and (PLine1^ = BREAK_OPCODE))) then
  begin
    DataSize := 5; { try 40 bit operand for line 1 }
    PLine1 := PLine2 - DataSize - SizeOf(LEA_OPCODE);
  end;
  if (PLine1^ = LEA_OPCODE) or (IsDebugging and (PLine1^ = BREAK_OPCODE)) then
  begin
    if CompareMem(PLine1 + SIZEOF_OPCODE, PLine4 + SIZEOF_OPCODE, DataSize) then
    begin
      // After this check, it seems to match the WideString <- (temp) AnsiString pattern
      Result := True; // It is probably OK. (The side effects of being wrong aren't very bad.)
    end;
  end;
end;

threadvar
  PLastResString: PAnsiChar;
  LastResStringValue: AnsiString;
  LastWideResString: WideString;

procedure FreeTntSystemThreadVars;
begin
  LastResStringValue := '';
  LastWideResString := '';
end;

procedure Custom_System_EndThread(ExitCode: Integer);
begin
  FreeTntSystemThreadVars;
  {$IFDEF COMPILER_10_UP}
  if Assigned(SystemThreadEndProc) then
    SystemThreadEndProc(ExitCode);
  {$ENDIF}
  ExitThread(ExitCode);
end;

function Custom_System_LoadResString(ResStringRec: PResStringRec): AnsiString;
var
  ReturnAddr: Pointer;
begin
  // get return address
  asm
    PUSH   ECX
    MOV    ECX, [EBP + 4]
    MOV    ReturnAddr, ECX
    POP    ECX
  end;
  // check calling code pattern
  if CodeMatchesPatternForUnicode(ReturnAddr) then begin
    // result will probably be assigned to an intermediate AnsiString
    //   on its way to either a WideString or Variant.
    LastWideResString := WideLoadResString(ResStringRec);
    Result := LastWideResString;
    LastResStringValue := Result;
    if Result = '' then
      PLastResString := nil
    else
      PLastResString := PAnsiChar(Result);
  end else begin
    // result will probably be assigned to an actual AnsiString variable.
    PLastResString := nil;
    Result := WideLoadResString(ResStringRec);
  end;
end;

//--------------------------------------------------------------------
//                WStrFromPCharLen()
//
//  This system function is used to assign an AnsiString to a WideString.
//   It has been modified to assign Unicode results from LoadResString.
//     Another purpose of this function is to specify the code page.
//--------------------------------------------------------------------

procedure Custom_System_WStrFromPCharLen(var Dest: WideString; Source: PAnsiChar; Length: Integer);
var
  DestLen: Integer;
  Buffer: array[0..2047] of WideChar;
  Local_PLastResString: Pointer;
begin
  Local_PLastResString := PLastResString;
  if  (Local_PLastResString <> nil)
  and (Local_PLastResString = Source)
  and (System.Length(LastResStringValue) = Length)
  and (LastResStringValue = Source) then begin
    // use last unicode resource string
    PLastResString := nil; { clear for further use }
    Dest := LastWideResString;
  end else begin
    if Local_PLastResString <> nil then
      PLastResString := nil; { clear for further use }
    if Length <= 0 then
    begin
      Dest := '';
      Exit;
    end;
    if Length + 1 < High(Buffer) then
    begin
      DestLen := MultiByteToWideChar(DefaultSystemCodePage, 0, Source, Length, Buffer,
        High(Buffer));
      if DestLen > 0 then
      begin
        SetLength(Dest, DestLen);
        Move(Pointer(@Buffer[0])^, Pointer(Dest)^, DestLen * SizeOf(WideChar));
        Exit;
      end;
    end;
    DestLen := (Length + 1);
    SetLength(Dest, DestLen); // overallocate, trim later
    DestLen := MultiByteToWideChar(DefaultSystemCodePage, 0, Source, Length, Pointer(Dest),
      DestLen);
    if DestLen < 0 then
      DestLen := 0;
    SetLength(Dest, DestLen);
  end;
end;

{$IFNDEF COMPILER_9_UP}

//--------------------------------------------------------------------
//                LStrFromPWCharLen()
//
//  This system function is used to assign an WideString to an AnsiString.
//   It has not been modified from its original purpose other than to specify the code page.
//--------------------------------------------------------------------

procedure Custom_System_LStrFromPWCharLen(var Dest: AnsiString; Source: PWideChar; Length: Integer);
var
  DestLen: Integer;
  Buffer: array[0..4095] of AnsiChar;
begin
  if Length <= 0 then
  begin
    Dest := '';
    Exit;
  end;
  if Length + 1 < (High(Buffer) div sizeof(WideChar)) then
  begin
    DestLen := WideCharToMultiByte(DefaultSystemCodePage, 0, Source,
      Length, Buffer, High(Buffer),
      nil, nil);
    if DestLen >= 0 then
    begin
      SetLength(Dest, DestLen);
      Move(Pointer(@Buffer[0])^, PAnsiChar(Dest)^, DestLen);
      Exit;
    end;
  end;

  DestLen := (Length + 1) * sizeof(WideChar);
  SetLength(Dest, DestLen); // overallocate, trim later
  DestLen := WideCharToMultiByte(DefaultSystemCodePage, 0, Source, Length, Pointer(Dest), DestLen,
    nil, nil);
  if DestLen < 0 then
    DestLen := 0;
  SetLength(Dest, DestLen);
end;

//--------------------------------------------------------------------
//                WStrToString()
//
//  This system function is used to assign an WideString to an short string.
//   It has not been modified from its original purpose other than to specify the code page.
//--------------------------------------------------------------------

procedure Custom_System_WStrToString(Dest: PShortString; const Source: WideString; MaxLen: Integer);
var
  SourceLen, DestLen: Integer;
  Buffer: array[0..511] of AnsiChar;
begin
  if MaxLen > 255 then MaxLen := 255;
  SourceLen := Length(Source);
  if SourceLen >= MaxLen then SourceLen := MaxLen;
  if SourceLen = 0 then
    DestLen := 0
  else begin
    DestLen := WideCharToMultiByte(DefaultSystemCodePage, 0, Pointer(Source), SourceLen,
      Buffer, SizeOf(Buffer), nil, nil);
    if DestLen > MaxLen then DestLen := MaxLen;
  end;
  Dest^[0] := Chr(DestLen);
  if DestLen > 0 then Move(Buffer, Dest^[1], DestLen);
end;

{$ENDIF}

//--------------------------------------------------------------------
//                VarFromLStr()
//
//  This system function is used to assign an AnsiString to a Variant.
//   It has been modified to assign Unicode results from LoadResString.
//--------------------------------------------------------------------

procedure Custom_System_VarFromLStr(var V: TVarData; const Value: AnsiString);
const
  varDeepData = $BFE8;
var
  Local_PLastResString: Pointer;
begin
  if (V.VType and varDeepData) <> 0 then
    VarClear(PVariant(@V)^);

  Local_PLastResString := PLastResString;
  if  (Local_PLastResString <> nil)
  and (Local_PLastResString = PAnsiChar(Value))
  and (LastResStringValue = Value) then begin
    // use last unicode resource string
    PLastResString := nil; { clear for further use }
    V.VOleStr := nil;
    V.VType := varOleStr;
    WideString(Pointer(V.VOleStr)) := Copy(LastWideResString, 1, MaxInt);
  end else begin
    if Local_PLastResString <> nil then
      PLastResString := nil; { clear for further use }
    V.VString := nil;
    V.VType := varString;
    AnsiString(V.VString) := Value;
  end;
end;

{$IFNDEF COMPILER_9_UP}

//--------------------------------------------------------------------
//                WStrCat3()     A := B + C;
//
//  This system function is used to concatenate two strings into one result.
//    This function is added because A := '' + '' doesn't necessarily result in A = '';
//--------------------------------------------------------------------

procedure Custom_System_WStrCat3(var Dest: WideString; const Source1, Source2: WideString);

  function NewWideString(CharLength: Longint): Pointer;
  var
    _NewWideString: function(CharLength: Longint): Pointer;
  begin
    asm
      PUSH   ECX
      MOV    ECX, offset System.@NewWideString;
      MOV    _NewWideString, ECX
      POP    ECX
    end;
    Result := _NewWideString(CharLength);
  end;

  procedure WStrSet(var S: WideString; P: PWideChar);
  var
    Temp: Pointer;
  begin
    Temp := Pointer(InterlockedExchange(Integer(S), Integer(P)));
    if Temp <> nil then
      WideString(Temp) := '';
  end;

var
  Source1Len, Source2Len: Integer;
  NewStr: PWideChar;
begin
  Source1Len := Length(Source1);
  Source2Len := Length(Source2);
  if (Source1Len <> 0) or (Source2Len <> 0) then
  begin
    NewStr := NewWideString(Source1Len + Source2Len);
    Move(Pointer(Source1)^, Pointer(NewStr)^, Source1Len * sizeof(WideChar));
    Move(Pointer(Source2)^, NewStr[Source1Len], Source2Len * sizeof(WideChar));
    WStrSet(Dest, NewStr);
  end else
    Dest := '';
end;

{$ENDIF}

//--------------------------------------------------------------------
//                System proc replacements
//--------------------------------------------------------------------

type
  POverwrittenData = ^TOverwrittenData;
  TOverwrittenData = record
    Location: Pointer;
    OldCode: array[0..6] of Byte;
  end;

procedure OverwriteProcedure(OldProcedure, NewProcedure: pointer; Data: POverwrittenData = nil);
{ OverwriteProcedure originally from Igor Siticov }
{ Modified by Jacques Garcia Vazquez }
var
  x: PAnsiChar;
  y: integer;
  ov2, ov: cardinal;
  p: pointer;
begin
  if Assigned(Data) and (Data.Location <> nil) then
    exit; { procedure already overwritten }

  // need six bytes in place of 5
  x := PAnsiChar(OldProcedure);
  if not VirtualProtect(Pointer(x), 6, PAGE_EXECUTE_READWRITE, @ov) then
    RaiseLastOSError;

  // if a jump is present then a redirect is found
  // $FF25 = jmp dword ptr [xxx]
  // This redirect is normally present in bpl files, but not in exe files
  p := OldProcedure;

  if Word(p^) = $25FF then
  begin
    Inc(Integer(p), 2); // skip the jump
    // get the jump address p^ and dereference it p^^
    p := Pointer(Pointer(p^)^);

    // release the memory
    if not VirtualProtect(Pointer(x), 6, ov, @ov2) then
      RaiseLastOSError;

    // re protect the correct one
    x := PAnsiChar(p);
    if not VirtualProtect(Pointer(x), 6, PAGE_EXECUTE_READWRITE, @ov) then
      RaiseLastOSError;
  end;

  if Assigned(Data) then
  begin
    Move(x^, Data.OldCode, 6);
    { Assign Location last so that Location <> nil only if OldCode is properly initialized. }
    Data.Location := x;
  end;

  x[0] := AnsiChar($E9);
  y := integer(NewProcedure) - integer(p) - 5;
  x[1] := AnsiChar(y and 255);
  x[2] := AnsiChar((y shr 8) and 255);
  x[3] := AnsiChar((y shr 16) and 255);
  x[4] := AnsiChar((y shr 24) and 255);

  if not VirtualProtect(Pointer(x), 6, ov, @ov2) then
    RaiseLastOSError;
end;

procedure RestoreProcedure(OriginalProc: Pointer; Data: TOverwrittenData);
var
  ov, ov2: Cardinal;
begin
  if Data.Location <> nil then begin
    if not VirtualProtect(Data.Location, 6, PAGE_EXECUTE_READWRITE, @ov) then
      RaiseLastOSError;
    Move(Data.OldCode, Data.Location^, 6);
    if not VirtualProtect(Data.Location, 6, ov, @ov2) then
      RaiseLastOSError;
  end;
end;

function Addr_System_EndThread: Pointer;
begin
  Result := @System.EndThread;
end;

function Addr_System_LoadResString: Pointer;
begin
  Result := @System.LoadResString{TNT-ALLOW LoadResString};
end;

function Addr_System_WStrFromPCharLen: Pointer;
asm
  mov eax, offset System.@WStrFromPCharLen;
end;

{$IFNDEF COMPILER_9_UP}
function Addr_System_LStrFromPWCharLen: Pointer;
asm
  mov eax, offset System.@LStrFromPWCharLen;
end;

function Addr_System_WStrToString: Pointer;
asm
  mov eax, offset System.@WStrToString;
end;
{$ENDIF}

function Addr_System_VarFromLStr: Pointer;
asm
  mov eax, offset System.@VarFromLStr;
end;

function Addr_System_WStrCat3: Pointer;
asm
  mov eax, offset System.@WStrCat3;
end;

var
  System_EndThread_Code,
  System_LoadResString_Code,
  System_WStrFromPCharLen_Code,
  {$IFNDEF COMPILER_9_UP}
  System_LStrFromPWCharLen_Code,
  System_WStrToString_Code,
  {$ENDIF}
  System_VarFromLStr_Code,
  {$IFNDEF COMPILER_9_UP}
  System_WStrCat3_Code,
  SysUtils_WideFmtStr_Code,
  {$ENDIF}
  Forms_TApplication_ShowException_Code,
  SysUtils_RaiseLastOsError_Code: TOverwrittenData;

procedure InstallEndThreadOverride;
begin
  OverwriteProcedure(Addr_System_EndThread,  @Custom_System_EndThread,  @System_EndThread_Code);
end;

procedure InstallStringConversionOverrides;
begin
  OverwriteProcedure(Addr_System_WStrFromPCharLen,  @Custom_System_WStrFromPCharLen,  @System_WStrFromPCharLen_Code);
  {$IFNDEF COMPILER_9_UP}
  OverwriteProcedure(Addr_System_LStrFromPWCharLen, @Custom_System_LStrFromPWCharLen, @System_LStrFromPWCharLen_Code);
  OverwriteProcedure(Addr_System_WStrToString,      @Custom_System_WStrToString,      @System_WStrToString_Code);
  {$ENDIF}
end;

procedure InstallWideResourceStrings;
begin
  OverwriteProcedure(Addr_System_LoadResString,     @Custom_System_LoadResString,     @System_LoadResString_Code);
  OverwriteProcedure(Addr_System_VarFromLStr,       @Custom_System_VarFromLStr,       @System_VarFromLStr_Code);
end;

{$IFNDEF COMPILER_9_UP}
procedure InstallWideStringConcatenationFix;
begin
  OverwriteProcedure(Addr_System_WStrCat3,          @Custom_System_WStrCat3,          @System_WStrCat3_Code);
end;

procedure InstallWideFormatFixes;
begin
  OverwriteProcedure(@SysUtils.WideFmtStr, @TntSysUtils.Tnt_WideFmtStr, @SysUtils_WideFmtStr_Code);
end;
{$ENDIF}

procedure InstallWideExceptions;
begin
  OverwriteProcedure(@Forms.TApplication.ShowException, @TTntApplication.ShowException, @Forms_TApplication_ShowException_Code);
  OverwriteProcedure(@SysUtils.RaiseLastOsError, @TntSysUtils.WideRaiseLastOsError, @SysUtils_RaiseLastOsError_Code);
end;

procedure InstallTntSystemUpdates(Updates: TTntSystemUpdateSet = AllTntSystemUpdates);
begin
  InstallEndThreadOverride;
  if tsWideResourceStrings in Updates then begin
    InstallStringConversionOverrides;
    InstallWideResourceStrings;
  end;
  {$IFNDEF COMPILER_9_UP}
    if tsFixImplicitCodePage in Updates then begin
      InstallStringConversionOverrides;
      { CP_ACP is the code page used by the non-Unicode Windows API. }
      GDefaultSystemCodePage := CP_ACP{TNT-ALLOW CP_ACP};
    end;
    if tsFixWideStrConcat in Updates then begin
      InstallWideStringConcatenationFix;
    end;
    if tsFixWideFormat in Updates then begin
      InstallWideFormatFixes;
    end;
  {$ENDIF}
  if tsWideExceptions in Updates then begin
    InstallWideExceptions
  end;
end;

{$IFNDEF COMPILER_9_UP}
var
  StartupDefaultUserCodePage: Cardinal;
{$ENDIF}

procedure UninstallSystemOverrides;
begin
  RestoreProcedure(Addr_System_EndThread,  System_EndThread_Code);
  // String Conversion
  RestoreProcedure(Addr_System_WStrFromPCharLen,  System_WStrFromPCharLen_Code);
  {$IFNDEF COMPILER_9_UP}
  RestoreProcedure(Addr_System_LStrFromPWCharLen, System_LStrFromPWCharLen_Code);
  RestoreProcedure(Addr_System_WStrToString,      System_WStrToString_Code);
  GDefaultSystemCodePage := StartupDefaultUserCodePage;
  {$ENDIF}
  // Wide resourcestring
  RestoreProcedure(Addr_System_LoadResString,     System_LoadResString_Code);
  RestoreProcedure(Addr_System_VarFromLStr,       System_VarFromLStr_Code);
  {$IFNDEF COMPILER_9_UP}
  // WideString concat fix
  RestoreProcedure(Addr_System_WStrCat3,          System_WStrCat3_Code);
  // WideFormat fixes
  RestoreProcedure(@SysUtils.WideFmtStr, SysUtils_WideFmtStr_Code);
  {$ENDIF}
  // Wide exception
  RestoreProcedure(@Forms.TApplication.ShowException, Forms_TApplication_ShowException_Code);
  RestoreProcedure(@SysUtils.RaiseLastOsError, SysUtils_RaiseLastOsError_Code);
end;

initialization
  {$IFDEF COMPILER_9_UP}
  GDefaultSystemCodePage := GetACP;
  {$ELSE}
    {$IFDEF COMPILER_7_UP}
    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
      GDefaultSystemCodePage := CP_THREAD_ACP // Win 2K/XP/...
    else
      GDefaultSystemCodePage := LCIDToCodePage(GetThreadLocale); // Win NT4/95/98/ME
    {$ELSE}
    GDefaultSystemCodePage := CP_ACP{TNT-ALLOW CP_ACP};
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF COMPILER_9_UP}
  StartupDefaultUserCodePage := DefaultSystemCodePage;
  {$ENDIF}
  {$WARN SYMBOL_PLATFORM OFF}
  IsDebugging := DebugHook > 0;
  {$WARN SYMBOL_PLATFORM ON}

finalization
  UninstallSystemOverrides;
  FreeTntSystemThreadVars; { Make MemorySleuth happy. }

end.
