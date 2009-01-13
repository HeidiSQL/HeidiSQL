
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntWideStrUtils;

{$INCLUDE compilers.inc}

interface

{ Wide string manipulation functions }

{$IFNDEF COMPILER_9_UP}
function WStrAlloc(Size: Cardinal): PWideChar;
function WStrBufSize(const Str: PWideChar): Cardinal;
{$ENDIF}
{$IFNDEF COMPILER_10_UP}
function WStrMove(Dest: PWideChar; const Source: PWideChar; Count: Cardinal): PWideChar;
{$ENDIF}
{$IFNDEF COMPILER_9_UP}
function WStrNew(const Str: PWideChar): PWideChar;
procedure WStrDispose(Str: PWideChar);
{$ENDIF}
//---------------------------------------------------------------------------------------------
{$IFNDEF COMPILER_9_UP}
function WStrLen(Str: PWideChar): Cardinal;
function WStrEnd(Str: PWideChar): PWideChar;
{$ENDIF}
{$IFNDEF COMPILER_10_UP}
function WStrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;
{$ENDIF}
{$IFNDEF COMPILER_9_UP}
function WStrCopy(Dest, Source: PWideChar): PWideChar;
function WStrLCopy(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
function WStrPCopy(Dest: PWideChar; const Source: WideString): PWideChar;
function WStrPLCopy(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar;
{$ENDIF}
{$IFNDEF COMPILER_10_UP}
function WStrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
// WStrComp and WStrPos were introduced as broken in Delphi 2006, but fixed in Delphi 2006 Update 2
function WStrComp(Str1, Str2: PWideChar): Integer;
function WStrPos(Str, SubStr: PWideChar): PWideChar;
{$ENDIF}
function Tnt_WStrComp(Str1, Str2: PWideChar): Integer; deprecated;
function Tnt_WStrPos(Str, SubStr: PWideChar): PWideChar; deprecated;

{ ------------ introduced --------------- }
function WStrECopy(Dest, Source: PWideChar): PWideChar;
function WStrLComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
function WStrLIComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
function WStrIComp(Str1, Str2: PWideChar): Integer;
function WStrLower(Str: PWideChar): PWideChar;
function WStrUpper(Str: PWideChar): PWideChar;
function WStrRScan(const Str: PWideChar; Chr: WideChar): PWideChar;
function WStrLCat(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
function WStrPas(const Str: PWideChar): WideString;

{ SysUtils.pas } //-------------------------------------------------------------------------

{$IFNDEF COMPILER_10_UP}
function WideLastChar(const S: WideString): PWideChar;
function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
{$ENDIF}
{$IFNDEF COMPILER_9_UP}
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): Widestring;
{$ENDIF}
{$IFNDEF COMPILER_10_UP}
function WideDequotedStr(const S: WideString; AQuote: WideChar): WideString;
{$ENDIF}

implementation

uses
  {$IFDEF COMPILER_9_UP} WideStrUtils, {$ENDIF} Math, Windows, TntWindows;

{$IFNDEF COMPILER_9_UP}
function WStrAlloc(Size: Cardinal): PWideChar;
begin
  Size := SizeOf(Cardinal) + (Size * SizeOf(WideChar));
  GetMem(Result, Size);
  PCardinal(Result)^ := Size;
  Inc(PAnsiChar(Result), SizeOf(Cardinal));
end;

function WStrBufSize(const Str: PWideChar): Cardinal;
var
  P: PWideChar;
begin
  P := Str;
  Dec(PAnsiChar(P), SizeOf(Cardinal));
  Result := PCardinal(P)^ - SizeOf(Cardinal);
  Result := Result div SizeOf(WideChar);
end;
{$ENDIF}

{$IFNDEF COMPILER_10_UP}
function WStrMove(Dest: PWideChar; const Source: PWideChar; Count: Cardinal): PWideChar;
var
  Length: Integer;
begin
  Result := Dest;
  Length := Count * SizeOf(WideChar);
  Move(Source^, Dest^, Length);
end;
{$ENDIF}

{$IFNDEF COMPILER_9_UP}
function WStrNew(const Str: PWideChar): PWideChar;
var
  Size: Cardinal;
begin
  if Str = nil then Result := nil else
  begin
    Size := WStrLen(Str) + 1;
    Result := WStrMove(WStrAlloc(Size), Str, Size);
  end;
end;

procedure WStrDispose(Str: PWideChar);
begin
  if Str <> nil then
  begin
    Dec(PAnsiChar(Str), SizeOf(Cardinal));
    FreeMem(Str, Cardinal(Pointer(Str)^));
  end;
end;
{$ENDIF}

//---------------------------------------------------------------------------------------------

{$IFNDEF COMPILER_9_UP}
function WStrLen(Str: PWideChar): Cardinal;
begin
  Result := WStrEnd(Str) - Str;
end;

function WStrEnd(Str: PWideChar): PWideChar;
begin
  // returns a pointer to the end of a null terminated string
  Result := Str;
  While Result^ <> #0 do
    Inc(Result);
end;
{$ENDIF}

{$IFNDEF COMPILER_10_UP}
function WStrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;
begin
  Result := Dest;
  WStrCopy(WStrEnd(Dest), Source);
end;
{$ENDIF}

{$IFNDEF COMPILER_9_UP}
function WStrCopy(Dest, Source: PWideChar): PWideChar;
begin
  Result := WStrLCopy(Dest, Source, MaxInt);
end;

function WStrLCopy(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
var
  Count: Cardinal;
begin
  // copies a specified maximum number of characters from Source to Dest
  Result := Dest;
  Count := 0;
  While (Count < MaxLen) and (Source^ <> #0) do begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
    Inc(Count);
  end;
  Dest^ := #0;
end;

function WStrPCopy(Dest: PWideChar; const Source: WideString): PWideChar;
begin
  Result := WStrLCopy(Dest, PWideChar(Source), Length(Source));
end;

function WStrPLCopy(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar;
begin
  Result := WStrLCopy(Dest, PWideChar(Source), MaxLen);
end;
{$ENDIF}

{$IFNDEF COMPILER_10_UP}
function WStrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function WStrComp(Str1, Str2: PWideChar): Integer;
begin
  Result := WStrLComp(Str1, Str2, MaxInt);
end;

function WStrPos(Str, SubStr: PWideChar): PWideChar;
var
  PSave: PWideChar;
  P: PWideChar;
  PSub: PWideChar;
begin
  // returns a pointer to the first occurance of SubStr in Str
  Result := nil;
  if (Str <> nil) and (Str^ <> #0) and (SubStr <> nil) and (SubStr^ <> #0) then begin
    P := Str;
    While P^ <> #0 do begin
      if P^ = SubStr^ then begin
        // investigate possibility here
        PSave := P;
        PSub := SubStr;
        While (P^ = PSub^) do begin
          Inc(P);
          Inc(PSub);
          if (PSub^ = #0) then begin
            Result := PSave;
            exit; // found a match
          end;
          if (P^ = #0) then
            exit; // no match, hit end of string
        end;
        P := PSave;
      end;
      Inc(P);
    end;
  end;
end;
{$ENDIF}

function Tnt_WStrComp(Str1, Str2: PWideChar): Integer; deprecated;
begin
  Result := WStrComp(Str1, Str2);
end;

function Tnt_WStrPos(Str, SubStr: PWideChar): PWideChar; deprecated;
begin
  Result := WStrPos(Str, SubStr);
end;

//------------------------------------------------------------------------------

function WStrECopy(Dest, Source: PWideChar): PWideChar;
begin
  Result := WStrEnd(WStrCopy(Dest, Source));
end;

function WStrComp_EX(Str1, Str2: PWideChar; MaxLen: Cardinal; dwCmpFlags: Cardinal): Integer;
var
  Len1, Len2: Integer;
begin
  if MaxLen = Cardinal(MaxInt) then begin
    Len1 := -1;
    Len2 := -1;
  end else begin
    Len1 := Min(WStrLen(Str1), MaxLen);
    Len2 := Min(WStrLen(Str2), MaxLen);
  end;
  Result := Tnt_CompareStringW(GetThreadLocale, dwCmpFlags, Str1, Len1, Str2, Len2) - 2;
end;

function WStrLComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
begin
  Result := WStrComp_EX(Str1, Str2, MaxLen, 0);
end;

function WStrLIComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
begin
  Result := WStrComp_EX(Str1, Str2, MaxLen, NORM_IGNORECASE);
end;

function WStrIComp(Str1, Str2: PWideChar): Integer;
begin
  Result := WStrLIComp(Str1, Str2, MaxInt);
end;

function WStrLower(Str: PWideChar): PWideChar;
begin
  Result := Str;
  Tnt_CharLowerBuffW(Str, WStrLen(Str))
end;

function WStrUpper(Str: PWideChar): PWideChar;
begin
  Result := Str;
  Tnt_CharUpperBuffW(Str, WStrLen(Str))
end;

function WStrRScan(const Str: PWideChar; Chr: WideChar): PWideChar;
var
  MostRecentFound: PWideChar;
begin
  if Chr = #0 then
    Result := WStrEnd(Str)
  else
  begin
    Result := nil;
    MostRecentFound := Str;
    while True do
    begin
      while MostRecentFound^ <> Chr do
      begin
        if MostRecentFound^ = #0 then
          Exit;
        Inc(MostRecentFound);
      end;
      Result := MostRecentFound;
      Inc(MostRecentFound);
    end;
  end;
end;

function WStrLCat(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
begin
  Result := Dest;
  WStrLCopy(WStrEnd(Dest), Source, MaxLen - WStrLen(Dest));
end;

function WStrPas(const Str: PWideChar): WideString;
begin
  Result := Str;
end;

//---------------------------------------------------------------------------------------------

{$IFNDEF COMPILER_10_UP}
function WideLastChar(const S: WideString): PWideChar;
begin
  if S = '' then
    Result := nil
  else
    Result := @S[Length(S)];
end;

function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
var
  P, Src,
  Dest: PWideChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := WStrScan(PWideChar(S), Quote);
  while (P <> nil) do
  begin
    Inc(P);
    Inc(AddCount);
    P := WStrScan(P, Quote);
  end;

  if AddCount = 0 then
    Result := Quote + S + Quote
  else
  begin
    SetLength(Result, Length(S) + AddCount + 2);
    Dest := PWideChar(Result);
    Dest^ := Quote;
    Inc(Dest);
    Src := PWideChar(S);
    P := WStrScan(Src, Quote);
    repeat
      Inc(P);
      Move(Src^, Dest^, 2 * (P - Src));
      Inc(Dest, P - Src);
      Dest^ := Quote;
      Inc(Dest);
      Src := P;
      P := WStrScan(Src, Quote);
    until P = nil;
    P := WStrEnd(Src);
    Move(Src^, Dest^, 2 * (P - Src));
    Inc(Dest, P - Src);
    Dest^ := Quote;
  end;
end;
{$ENDIF}

{$IFNDEF COMPILER_9_UP}
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): Widestring;
var
  P, Dest: PWideChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then Exit;
  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := WStrScan(Src, Quote);
  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then Break;
    Inc(Src);
    Inc(DropCount);
    Src := WStrScan(Src, Quote);
  end;
  if Src = nil then Src := WStrEnd(P);
  if ((Src - P) <= 1) then Exit;
  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PWideChar(Result);
    Src := WStrScan(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then Break;
      Move(P^, Dest^, (Src - P) * SizeOf(WideChar));
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := WStrScan(Src, Quote);
    end;
    if Src = nil then Src := WStrEnd(P);
    Move(P^, Dest^, (Src - P - 1) * SizeOf(WideChar));
  end;
end;
{$ENDIF}

{$IFNDEF COMPILER_10_UP}
function WideDequotedStr(const S: WideString; AQuote: WideChar): WideString;
var
  LText : PWideChar;
begin
  LText := PWideChar(S);
  Result := WideExtractQuotedStr(LText, AQuote);
  if Result = '' then
    Result := S;
end;
{$ENDIF}


end.
