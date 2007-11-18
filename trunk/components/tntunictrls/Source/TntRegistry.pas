
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntRegistry;

{$INCLUDE TntCompilers.inc}

interface

uses
  Registry, Windows, TntClasses;

{TNT-WARN TRegistry}
type
  TTntRegistry = class(TRegistry{TNT-ALLOW TRegistry})
  private
    procedure WriteStringEx(dwType: DWORD; const Name, Value: WideString);
  public
    procedure GetKeyNames(Strings: TTntStrings);
    procedure GetValueNames(Strings: TTntStrings);
    function ReadString(const Name: WideString): WideString;
    procedure WriteString(const Name, Value: WideString);
    procedure WriteExpandString(const Name, Value: WideString);
  end;

implementation

uses
  RTLConsts, SysUtils, TntSysUtils;

{ TTntRegistry }

procedure TTntRegistry.GetKeyNames(Strings: TTntStrings);
var
  Len: DWORD;
  I: Integer;
  Info: TRegKeyInfo;
  S: WideString;
begin
  if (not Win32PlatformIsUnicode) then
    inherited GetKeyNames(Strings.AnsiStrings)
  else begin
    Strings.Clear;
    if GetKeyInfo(Info) then
    begin
      SetLength(S, (Info.MaxSubKeyLen + 1) * 2);
      for I := 0 to Info.NumSubKeys - 1 do
      begin
        Len := (Info.MaxSubKeyLen + 1) * 2;
        if RegEnumKeyExW(CurrentKey, I, PWideChar(S), Len, nil, nil, nil, nil) = ERROR_SUCCESS then
          Strings.Add(PWideChar(S));
      end;
    end;
  end;
end;

{$IFNDEF COMPILER_9_UP} // fix declaration for RegEnumValueW (lpValueName is a PWideChar)
function RegEnumValueW(hKey: HKEY; dwIndex: DWORD; lpValueName: PWideChar;
  var lpcbValueName: DWORD; lpReserved: Pointer; lpType: PDWORD;
  lpData: PByte; lpcbData: PDWORD): Longint; stdcall; external advapi32 name 'RegEnumValueW';
{$ENDIF}

procedure TTntRegistry.GetValueNames(Strings: TTntStrings);
var
  Len: DWORD;
  I: Integer;
  Info: TRegKeyInfo;
  S: WideString;
begin
  if (not Win32PlatformIsUnicode) then
    inherited GetValueNames(Strings.AnsiStrings)
  else begin
    Strings.Clear;
    if GetKeyInfo(Info) then
    begin
      SetLength(S, Info.MaxValueLen + 1);
      for I := 0 to Info.NumValues - 1 do
      begin
        Len := Info.MaxValueLen + 1;
        RegEnumValueW(CurrentKey, I, PWideChar(S), Len, nil, nil, nil, nil);
        Strings.Add(PWideChar(S));
      end;
    end;
  end;
end;

function TTntRegistry.ReadString(const Name: WideString): WideString;
var
  DataType: Cardinal;
  BufSize: Cardinal;
begin
  if (not Win32PlatformIsUnicode) then
    result := inherited ReadString(Name)
  else begin
    // get length and type
    DataType := REG_NONE;
    if RegQueryValueExW(CurrentKey, PWideChar(Name), nil,
      @DataType, nil, @BufSize) <> ERROR_SUCCESS then
        Result := ''
    else begin
      // check type
      if not (DataType in [REG_SZ, REG_EXPAND_SZ]) then
        raise ERegistryException.CreateFmt(SInvalidRegType, [Name]);
      if BufSize = 1 then
        BufSize := SizeOf(WideChar); // sometimes this occurs for single character values!
      SetLength(Result, BufSize div SizeOf(WideChar));
      if RegQueryValueExW(CurrentKey, PWideChar(Name), nil,
        @DataType, PByte(PWideChar(Result)), @BufSize) <> ERROR_SUCCESS then
          raise ERegistryException.CreateFmt(SRegGetDataFailed, [Name]);
      Result := PWideChar(Result);
    end
  end
end;

procedure TTntRegistry.WriteStringEx(dwType: DWORD; const Name, Value: WideString);
begin
  Assert(dwType in [REG_SZ, REG_EXPAND_SZ]);
  if (not Win32PlatformIsUnicode) then begin
    if dwType = REG_SZ then
      inherited WriteString(Name, Value)
    else
      inherited WriteExpandString(Name, Value);
  end else begin
    if RegSetValueExW(CurrentKey, PWideChar(Name), 0, dwType,
      PWideChar(Value), (Length(Value) + 1) * SizeOf(WideChar)) <> ERROR_SUCCESS then
        raise ERegistryException.CreateFmt(SRegSetDataFailed, [Name]);
  end;
end;

procedure TTntRegistry.WriteString(const Name, Value: WideString);
begin
  WriteStringEx(REG_SZ, Name, Value);
end;

procedure TTntRegistry.WriteExpandString(const Name, Value: WideString);
begin
  WriteStringEx(REG_EXPAND_SZ, Name, Value);
end;

end.
