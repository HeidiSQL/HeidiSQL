{==============================================================================|
| Project : Ararat Synapse                                       | 001.003.001 |
|==============================================================================|
| Content: misc. procedures and functions                                      |
|==============================================================================|
| Copyright (c)1999-2010, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c) 2002-2010.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(Misc. network based utilities)}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$H+}

//Kylix does not known UNIX define
{$IFDEF LINUX}
  {$IFNDEF UNIX}
    {$DEFINE UNIX}
  {$ENDIF}
{$ENDIF}

{$TYPEDADDRESS OFF}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit synamisc;

interface

{$IFDEF VER125}
  {$DEFINE BCB}
{$ENDIF}
{$IFDEF BCB}
  {$ObjExportAll On}
  {$HPPEMIT '#pragma comment( lib , "wininet.lib" )'}
{$ENDIF}

uses
  synautil, blcksock, SysUtils, Classes
{$IFDEF UNIX}
  {$IFNDEF FPC}
  , Libc
  {$ENDIF}
{$ELSE}
  , Windows
{$ENDIF}
;

Type
  {:@abstract(This record contains information about proxy setting.)}
  TProxySetting = record
    Host: string;
    Port: string;
    Bypass: string;
  end;

{:By this function you can turn-on computer on network, if this computer
 supporting Wake-on-lan feature. You need MAC number (network card indentifier)
 of computer for turn-on. You can also assign target IP addres. If you not
 specify it, then is used broadcast for delivery magic wake-on packet. However
 broadcasts workinh only on your local network. When you need to wake-up
 computer on another network, you must specify any existing IP addres on same
 network segment as targeting computer.}
procedure WakeOnLan(MAC, IP: string);

{:Autodetect current DNS servers used by system. If is defined more then one DNS
 server, then result is comma-delimited.}
function GetDNS: string;

{:Autodetect InternetExplorer proxy setting for given protocol. This function
working only on windows!}
function GetIEProxy(protocol: string): TProxySetting;

{:Return all known IP addresses on local system. Addresses are divided by comma.}
function GetLocalIPs: string;

implementation

{==============================================================================}
procedure WakeOnLan(MAC, IP: string);
var
  sock: TUDPBlockSocket;
  HexMac: Ansistring;
  data: Ansistring;
  n: integer;
  b: Byte;
begin
  if MAC <> '' then
  begin
    MAC := ReplaceString(MAC, '-', '');
    MAC := ReplaceString(MAC, ':', '');
    if Length(MAC) < 12 then
      Exit;
    HexMac := '';
    for n := 0 to 5 do
    begin
      b := StrToIntDef('$' + MAC[n * 2 + 1] + MAC[n * 2 + 2], 0);
      HexMac := HexMac + char(b);
    end;
    if IP = '' then
      IP := cBroadcast;
    sock := TUDPBlockSocket.Create;
    try
      sock.CreateSocket;
      sock.EnableBroadcast(true);
      sock.Connect(IP, '9');
      data := #$FF + #$FF + #$FF + #$FF + #$FF + #$FF;
      for n := 1 to 16 do
        data := data + HexMac;
      sock.SendString(data);
    finally
      sock.Free;
    end;
  end;
end;

{==============================================================================}

{$IFNDEF UNIX}
function GetDNSbyIpHlp: string;
type
  PTIP_ADDRESS_STRING = ^TIP_ADDRESS_STRING;
  TIP_ADDRESS_STRING = array[0..15] of Ansichar;
  PTIP_ADDR_STRING = ^TIP_ADDR_STRING;
  TIP_ADDR_STRING = packed record
    Next: PTIP_ADDR_STRING;
    IpAddress: TIP_ADDRESS_STRING;
    IpMask: TIP_ADDRESS_STRING;
    Context: DWORD;
  end;
  PTFixedInfo = ^TFixedInfo;
  TFixedInfo = packed record
    HostName: array[1..128 + 4] of Ansichar;
    DomainName: array[1..128 + 4] of Ansichar;
    CurrentDNSServer: PTIP_ADDR_STRING;
    DNSServerList: TIP_ADDR_STRING;
    NodeType: UINT;
    ScopeID: array[1..256 + 4] of Ansichar;
    EnableRouting: UINT;
    EnableProxy: UINT;
    EnableDNS: UINT;
  end;
const
  IpHlpDLL = 'IPHLPAPI.DLL';
var
  IpHlpModule: THandle;
  FixedInfo: PTFixedInfo;
  InfoSize: Longint;
  PDnsServer: PTIP_ADDR_STRING;
  err: integer;
  GetNetworkParams: function(FixedInfo: PTFixedInfo; pOutPutLen: PULONG): DWORD; stdcall;
begin
  InfoSize := 0;
  Result := '...';
  IpHlpModule := LoadLibrary(IpHlpDLL);
  if IpHlpModule = 0 then
    exit;
  try
    GetNetworkParams := GetProcAddress(IpHlpModule,PAnsiChar(AnsiString('GetNetworkParams')));
    if @GetNetworkParams = nil then
      Exit;
    err := GetNetworkParams(Nil, @InfoSize);
    if err <> ERROR_BUFFER_OVERFLOW then
      Exit;
    Result := '';
    GetMem (FixedInfo, InfoSize);
    try
      err := GetNetworkParams(FixedInfo, @InfoSize);
      if err <> ERROR_SUCCESS then
        exit;
      with FixedInfo^ do
      begin
        Result := DnsServerList.IpAddress;
        PDnsServer := DnsServerList.Next;
        while PDnsServer <> Nil do
        begin
          if Result <> '' then
            Result := Result + ',';
          Result := Result + PDnsServer^.IPAddress;
          PDnsServer := PDnsServer.Next;
        end;
    end;
    finally
      FreeMem(FixedInfo);
    end;
  finally
    FreeLibrary(IpHlpModule);
  end;
end;

function ReadReg(SubKey, Vn: PChar): string;
var
 OpenKey: HKEY;
 DataType, DataSize: integer;
 Temp: array [0..2048] of char;
begin
  Result := '';
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, SubKey, REG_OPTION_NON_VOLATILE,
    KEY_READ, OpenKey) = ERROR_SUCCESS then
  begin
    DataType := REG_SZ;
    DataSize := SizeOf(Temp);
    if RegQueryValueEx(OpenKey, Vn, nil, @DataType, @Temp, @DataSize) = ERROR_SUCCESS then
      SetString(Result, Temp, DataSize div SizeOf(Char) - 1);
    RegCloseKey(OpenKey);
   end;
end ;
{$ENDIF}

function GetDNS: string;
{$IFDEF UNIX}
var
  l: TStringList;
  n: integer;
begin
  Result := '';
  l := TStringList.Create;
  try
    l.LoadFromFile('/etc/resolv.conf');
    for n := 0 to l.Count - 1 do
      if Pos('NAMESERVER', uppercase(l[n])) = 1 then
      begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + SeparateRight(l[n], ' ');
      end;
  finally
    l.Free;
  end;
end;
{$ELSE}
const
  NTdyn = 'System\CurrentControlSet\Services\Tcpip\Parameters\Temporary';
  NTfix = 'System\CurrentControlSet\Services\Tcpip\Parameters';
  W9xfix = 'System\CurrentControlSet\Services\MSTCP';
begin
  Result := GetDNSbyIpHlp;
  if Result = '...' then
  begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      Result := ReadReg(NTdyn, 'NameServer');
      if result = '' then
        Result := ReadReg(NTfix, 'NameServer');
      if result = '' then
        Result := ReadReg(NTfix, 'DhcpNameServer');
    end
    else
      Result := ReadReg(W9xfix, 'NameServer');
    Result := ReplaceString(trim(Result), ' ', ',');
  end;
end;
{$ENDIF}

{==============================================================================}

function GetIEProxy(protocol: string): TProxySetting;
{$IFDEF UNIX}
begin
  Result.Host := '';
  Result.Port := '';
  Result.Bypass := '';
end;
{$ELSE}
type
  PInternetProxyInfo = ^TInternetProxyInfo;
  TInternetProxyInfo = packed record
    dwAccessType: DWORD;
    lpszProxy: LPCSTR;
    lpszProxyBypass: LPCSTR;
  end;
const
  INTERNET_OPTION_PROXY = 38;
  INTERNET_OPEN_TYPE_PROXY = 3;
  WininetDLL = 'WININET.DLL';
var
  WininetModule: THandle;
  ProxyInfo: PInternetProxyInfo;
  Err: Boolean;
  Len: DWORD;
  Proxy: string;
  DefProxy: string;
  ProxyList: TStringList;
  n: integer;
  InternetQueryOption: function (hInet: Pointer; dwOption: DWORD;
    lpBuffer: Pointer; var lpdwBufferLength: DWORD): BOOL; stdcall;
begin
  Result.Host := '';
  Result.Port := '';
  Result.Bypass := '';
  WininetModule := LoadLibrary(WininetDLL);
  if WininetModule = 0 then
    exit;
  try
    InternetQueryOption := GetProcAddress(WininetModule,PAnsiChar(AnsiString('InternetQueryOptionA')));
    if @InternetQueryOption = nil then
      Exit;

    if protocol = '' then
      protocol := 'http';
    Len := 4096;
    GetMem(ProxyInfo, Len);
    ProxyList := TStringList.Create;
    try
      Err := InternetQueryOption(nil, INTERNET_OPTION_PROXY, ProxyInfo, Len);
      if Err then
        if ProxyInfo^.dwAccessType = INTERNET_OPEN_TYPE_PROXY then
        begin
          ProxyList.CommaText := ReplaceString(ProxyInfo^.lpszProxy, ' ', ',');
          Proxy := '';
          DefProxy := '';
          for n := 0 to ProxyList.Count -1 do
          begin
            if Pos(lowercase(protocol) + '=', lowercase(ProxyList[n])) = 1 then
            begin
              Proxy := SeparateRight(ProxyList[n], '=');
              break;
            end;
            if Pos('=', ProxyList[n]) < 1 then
              DefProxy := ProxyList[n];
          end;
          if Proxy = '' then
            Proxy := DefProxy;
          if Proxy <> '' then
          begin
            Result.Host := Trim(SeparateLeft(Proxy, ':'));
            Result.Port := Trim(SeparateRight(Proxy, ':'));
          end;
          Result.Bypass := ReplaceString(ProxyInfo^.lpszProxyBypass, ' ', ',');
        end;
    finally
      ProxyList.Free;
      FreeMem(ProxyInfo);
    end;
  finally
    FreeLibrary(WininetModule);
  end;
end;
{$ENDIF}

{==============================================================================}

function GetLocalIPs: string;
var
  TcpSock: TTCPBlockSocket;
  ipList: TStringList;
begin
  Result := '';
  ipList := TStringList.Create;
  try
    TcpSock := TTCPBlockSocket.create;
    try
      TcpSock.ResolveNameToIP(TcpSock.LocalName, ipList);
      Result := ipList.CommaText;
    finally
      TcpSock.Free;
    end;
  finally
    ipList.Free;
  end;
end;

{==============================================================================}

end.
