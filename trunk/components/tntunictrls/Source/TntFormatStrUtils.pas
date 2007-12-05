
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntFormatStrUtils;

{$INCLUDE compilers.inc}

interface

// this unit provides functions to work with format strings

uses
  TntSysUtils;

function GetCanonicalFormatStr(const _FormatString: WideString): WideString;
{$IFNDEF COMPILER_9_UP}
function ReplaceFloatingArgumentsInFormatString(const _FormatString: WideString;
  const Args: array of const
    {$IFDEF COMPILER_7_UP}; FormatSettings: PFormatSettings{$ENDIF}): WideString;
{$ENDIF}
procedure CompareFormatStrings(FormatStr1, FormatStr2: WideString);
function FormatStringsAreCompatible(FormatStr1, FormatStr2: WideString): Boolean;

type
  EFormatSpecError = class(ETntGeneralError);

implementation

uses
  SysUtils, Math, TntClasses;

resourcestring
  SInvalidFormatSpecifier = 'Invalid Format Specifier: %s';
  SMismatchedArgumentTypes = 'Argument types for index %d do not match. (%s <> %s)';
  SMismatchedArgumentCounts = 'Number of format specifiers do not match.';

type
  TFormatSpecifierType = (fstInteger, fstFloating, fstPointer, fstString);

function GetFormatSpecifierType(const FormatSpecifier: WideString): TFormatSpecifierType;
var
  LastChar: WideChar;
begin
  LastChar := TntWideLastChar(FormatSpecifier);
  case LastChar of
    'd', 'D', 'u', 'U', 'x', 'X':
      result := fstInteger;
    'e', 'E', 'f', 'F', 'g', 'G', 'n', 'N', 'm', 'M':
      result := fstFloating;
    'p', 'P':
      result := fstPointer;
    's', 'S':
      result := fstString
    else
      raise ETntInternalError.CreateFmt('Internal Error: Unexpected format type (%s)', [LastChar]);
  end;
end;

type
  TFormatStrParser = class(TObject)
  private
    ParsedString: TBufferedWideString;
    PFormatString: PWideChar;
    LastIndex: Integer;
    ExplicitCount: Integer;
    ImplicitCount: Integer;
    procedure RaiseInvalidFormatSpecifier;
    function ParseChar(c: WideChar): Boolean;
    procedure ForceParseChar(c: WideChar);
    function ParseDigit: Boolean;
    function ParseInteger: Boolean;
    procedure ForceParseType;
    function PeekDigit: Boolean;
    function PeekIndexSpecifier(out Index: Integer): Boolean;
  public
    constructor Create(const _FormatString: WideString);
    destructor Destroy; override;
    function ParseFormatSpecifier: Boolean;
  end;

constructor TFormatStrParser.Create(const _FormatString: WideString);
begin
  inherited Create;
  PFormatString := PWideChar(_FormatString);
  ExplicitCount := 0;
  ImplicitCount := 0;
  LastIndex := -1;
  ParsedString := TBufferedWideString.Create;
end;

destructor TFormatStrParser.Destroy;
begin
  FreeAndNil(ParsedString);
  inherited;
end;

procedure TFormatStrParser.RaiseInvalidFormatSpecifier;
begin
  raise EFormatSpecError.CreateFmt(SInvalidFormatSpecifier, [ParsedString.Value + PFormatString]);
end;

function TFormatStrParser.ParseChar(c: WideChar): Boolean;
begin
  result := False;
  if PFormatString^ = c then begin
    result := True;
    ParsedString.AddChar(c);
    Inc(PFormatString);
  end;
end;

procedure TFormatStrParser.ForceParseChar(c: WideChar);
begin
  if not ParseChar(c) then
    RaiseInvalidFormatSpecifier;
end;

function TFormatStrParser.PeekDigit: Boolean;
begin
  result := False;
  if  (PFormatString^ <> #0)
  and (PFormatString^ >= '0')
  and (PFormatString^ <= '9') then
    result := True;
end;

function TFormatStrParser.ParseDigit: Boolean;
begin
  result := False;
  if PeekDigit then begin
    result := True;
    ForceParseChar(PFormatString^);
  end;
end;

function TFormatStrParser.ParseInteger: Boolean;
const
  MAX_INT_DIGITS = 6;
var
  digitcount: integer;
begin
  digitcount := 0;
  While ParseDigit do begin
    inc(digitcount);
  end;
  result := (digitcount > 0);
  if digitcount > MAX_INT_DIGITS then
    RaiseInvalidFormatSpecifier;
end;

procedure TFormatStrParser.ForceParseType;
begin
  if PFormatString^ = #0 then
    RaiseInvalidFormatSpecifier;

  case PFormatString^ of
    'd', 'u', 'x', 'e', 'f', 'g', 'n', 'm', 'p', 's',
    'D', 'U', 'X', 'E', 'F', 'G', 'N', 'M', 'P', 'S':
  begin
    // do nothing
  end
  else
    RaiseInvalidFormatSpecifier;
  end;
  ForceParseChar(PFormatString^);
end;

function TFormatStrParser.PeekIndexSpecifier(out Index: Integer): Boolean;
var
  SaveParsedString: WideString;
  SaveFormatString: PWideChar;
begin
  SaveParsedString := ParsedString.Value;
  SaveFormatString := PFormatString;
  try
    ParsedString.Clear;
    Result := False;
    Index := -1;
    if ParseInteger then begin
      Index := StrToInt(ParsedString.Value);
      if ParseChar(':') then
        Result := True;
    end;
  finally
    ParsedString.Clear;
    ParsedString.AddString(SaveParsedString);
    PFormatString := SaveFormatString;
  end;
end;

function TFormatStrParser.ParseFormatSpecifier: Boolean;
var
  ExplicitIndex: Integer;
begin
  Result := False;
  // Parse entire format specifier
  ForceParseChar('%');
  if (PFormatString^ <> #0)
  and (not ParseChar(' '))
  and (not ParseChar('%')) then begin
    if PeekIndexSpecifier(ExplicitIndex) then begin
      Inc(ExplicitCount);
      LastIndex := Max(LastIndex, ExplicitIndex);
    end else begin
      Inc(ImplicitCount);
      Inc(LastIndex);
      ParsedString.AddString(IntToStr(LastIndex));
      ParsedString.AddChar(':');
    end;
    if ParseChar('*') then
    begin
      Inc(ImplicitCount);
      Inc(LastIndex);
      ParseChar(':');
    end else if ParseInteger then
      ParseChar(':');
    ParseChar('-');
    if ParseChar('*') then begin
      Inc(ImplicitCount);
      Inc(LastIndex);
    end else
      ParseInteger;
    if ParseChar('.') then begin
      if not ParseChar('*') then
        ParseInteger;
    end;
    ForceParseType;
    Result := True;
  end;
end;

//-----------------------------------

function GetCanonicalFormatStr(const _FormatString: WideString): WideString;
var
  PosSpec: Integer;
begin
  with TFormatStrParser.Create(_FormatString) do
  try
    // loop until no more '%'
    PosSpec := Pos('%', PFormatString);
    While PosSpec <> 0 do begin
      try
        // delete everything up until '%'
        ParsedString.AddBuffer(PFormatString, PosSpec - 1);
        Inc(PFormatString, PosSpec - 1);
        // parse format specifier
        ParseFormatSpecifier;
      finally
        PosSpec := Pos('%', PFormatString);
      end;
    end;
    if ((ExplicitCount = 0) and (ImplicitCount = 1)) {simple expression}
    or ((ExplicitCount > 0) and (ImplicitCount = 0)) {nothing converted} then
      result := _FormatString {original}
    else
      result := ParsedString.Value + PFormatString;
  finally
    Free;
  end;
end;

{$IFNDEF COMPILER_9_UP}
function ReplaceFloatingArgumentsInFormatString(const _FormatString: WideString;
  const Args: array of const
    {$IFDEF COMPILER_7_UP}; FormatSettings: PFormatSettings{$ENDIF}): WideString;
{ This function replaces floating point format specifiers with their actual formatted values.
  It also adds index specifiers so that the other format specifiers don't lose their place.
  The reason for this is that WideFormat doesn't correctly format floating point specifiers.
  See QC#4254. }
var
  Parser: TFormatStrParser;
  PosSpec: Integer;
  Output: TBufferedWideString;
begin
  Output := TBufferedWideString.Create;
  try
    Parser := TFormatStrParser.Create(_FormatString);
    with Parser do
    try
      // loop until no more '%'
      PosSpec := Pos('%', PFormatString);
      While PosSpec <> 0 do begin
        try
          // delete everything up until '%'
          Output.AddBuffer(PFormatString, PosSpec - 1);
          Inc(PFormatString, PosSpec - 1);
          // parse format specifier
          ParsedString.Clear;
          if (not ParseFormatSpecifier)
          or (GetFormatSpecifierType(ParsedString.Value) <> fstFloating) then
            Output.AddBuffer(ParsedString.BuffPtr, MaxInt)
          {$IFDEF COMPILER_7_UP}
          else if Assigned(FormatSettings) then
            Output.AddString(Format{TNT-ALLOW Format}(ParsedString.Value, Args, FormatSettings^))
          {$ENDIF}
          else
            Output.AddString(Format{TNT-ALLOW Format}(ParsedString.Value, Args));
        finally
          PosSpec := Pos('%', PFormatString);
        end;
      end;
      Output.AddString(PFormatString);
    finally
      Free;
    end;
    Result := Output.Value;
  finally
    Output.Free;
  end;
end;
{$ENDIF}

procedure GetFormatArgs(const _FormatString: WideString; FormatArgs: TTntStrings);
var
  PosSpec: Integer;
begin
  with TFormatStrParser.Create(_FormatString) do
  try
    FormatArgs.Clear;
    // loop until no more '%'
    PosSpec := Pos('%', PFormatString);
    While PosSpec <> 0 do begin
      try
        // delete everything up until '%'
        Inc(PFormatString, PosSpec - 1);
        // add format specifier to list
        ParsedString.Clear;
        if ParseFormatSpecifier then
          FormatArgs.Add(ParsedString.Value);
      finally
        PosSpec := Pos('%', PFormatString);
      end;
    end;
  finally
    Free;
  end;
end;

function GetExplicitIndex(const FormatSpecifier: WideString): Integer;
var
  IndexStr: WideString;
  PosColon: Integer;
begin
  result := -1;
  PosColon := Pos(':', FormatSpecifier);
  if PosColon <> 0 then begin
    IndexStr := Copy(FormatSpecifier, 2, PosColon - 2);
    result := StrToInt(IndexStr);
  end;
end;

function GetMaxIndex(FormatArgs: TTntStrings): Integer;
var
  i: integer;
  RunningIndex: Integer;
  ExplicitIndex: Integer;
begin
  result := -1;
  RunningIndex := -1;
  for i := 0 to FormatArgs.Count - 1 do begin
    ExplicitIndex := GetExplicitIndex(FormatArgs[i]);
    if ExplicitIndex <> -1 then
      RunningIndex := ExplicitIndex
    else
      inc(RunningIndex);
    result := Max(result, RunningIndex);
  end;
end;

procedure UpdateTypeList(FormatArgs, TypeList: TTntStrings);
var
  i: integer;
  f: WideString;
  SpecType: TFormatSpecifierType;
  ExplicitIndex: Integer;
  MaxIndex: Integer;
  RunningIndex: Integer;
begin
  // set count of TypeList to accomodate maximum index
  MaxIndex := GetMaxIndex(FormatArgs);
  TypeList.Clear;
  for i := 0 to MaxIndex do
    TypeList.Add('');

  // for each arg...
  RunningIndex := -1;
  for i := 0 to FormatArgs.Count - 1 do begin
    f := FormatArgs[i];
    ExplicitIndex := GetExplicitIndex(f);
    SpecType := GetFormatSpecifierType(f);

    // determine running arg index
    if ExplicitIndex <> -1 then
      RunningIndex := ExplicitIndex
    else
      inc(RunningIndex);

    if TypeList[RunningIndex] <> '' then begin
      // already exists in list, check for compatibility
      if TypeList.Objects[RunningIndex] <> TObject(SpecType) then
        raise EFormatSpecError.CreateFmt(SMismatchedArgumentTypes,
          [RunningIndex, TypeList[RunningIndex], f]);
    end else begin
      // not in list so update it
      TypeList[RunningIndex] := f;
      TypeList.Objects[RunningIndex] := TObject(SpecType);
    end;
  end;
end;

procedure CompareFormatStrings(FormatStr1, FormatStr2: WideString);
var
  ArgList1: TTntStringList;
  ArgList2: TTntStringList;
  TypeList1: TTntStringList;
  TypeList2: TTntStringList;
  i: integer;
begin
  ArgList1 := nil;
  ArgList2 := nil;
  TypeList1 := nil;
  TypeList2 := nil;
  try
    ArgList1 := TTntStringList.Create;
    ArgList2 := TTntStringList.Create;
    TypeList1 := TTntStringList.Create;
    TypeList2 := TTntStringList.Create;

    GetFormatArgs(FormatStr1, ArgList1);
    UpdateTypeList(ArgList1, TypeList1);

    GetFormatArgs(FormatStr2, ArgList2);
    UpdateTypeList(ArgList2, TypeList2);

    if TypeList1.Count <> TypeList2.Count then
      raise EFormatSpecError.Create(SMismatchedArgumentCounts + CRLF + CRLF + '> ' + FormatStr1 + CRLF + '> ' + FormatStr2);

    for i := 0 to TypeList1.Count - 1 do begin
      if TypeList1.Objects[i] <> TypeList2.Objects[i] then begin
        raise EFormatSpecError.CreateFmt(SMismatchedArgumentTypes,
          [i, TypeList1[i], TypeList2[i]]);
      end;
    end;

  finally
    ArgList1.Free;
    ArgList2.Free;
    TypeList1.Free;
    TypeList2.Free;
  end;
end;

function FormatStringsAreCompatible(FormatStr1, FormatStr2: WideString): Boolean;
var
  ArgList1: TTntStringList;
  ArgList2: TTntStringList;
  TypeList1: TTntStringList;
  TypeList2: TTntStringList;
  i: integer;
begin
  ArgList1 := nil;
  ArgList2 := nil;
  TypeList1 := nil;
  TypeList2 := nil;
  try
    ArgList1 := TTntStringList.Create;
    ArgList2 := TTntStringList.Create;
    TypeList1 := TTntStringList.Create;
    TypeList2 := TTntStringList.Create;

    GetFormatArgs(FormatStr1, ArgList1);
    UpdateTypeList(ArgList1, TypeList1);

    GetFormatArgs(FormatStr2, ArgList2);
    UpdateTypeList(ArgList2, TypeList2);

    Result := (TypeList1.Count = TypeList2.Count);
    if Result then begin
      for i := 0 to TypeList1.Count - 1 do begin
        if TypeList1.Objects[i] <> TypeList2.Objects[i] then begin
          Result := False;
          break;
        end;
      end;
    end;
  finally
    ArgList1.Free;
    ArgList2.Free;
    TypeList1.Free;
    TypeList2.Free;
  end;
end;

end.
