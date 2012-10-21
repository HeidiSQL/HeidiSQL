{==============================================================================|
| Project : Ararat Synapse                                       | 004.000.002 |
|==============================================================================|
| Content: PING sender                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2000-2010.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(ICMP PING implementation.)
Allows create PING and TRACEROUTE. Or you can diagnose your network.

This unit using IpHlpApi (on WinXP or higher) if available. Otherwise it trying
 to use RAW sockets.

Warning: For use of RAW sockets you must have some special rights on some
 systems. So, it working allways when you have administator/root rights.
 Otherwise you can have problems!

Note: This unit is NOT portable to .NET!
  Use native .NET classes for Ping instead.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$R-}
{$H+}

{$IFDEF CIL}
  Sorry, this unit is not for .NET!
{$ENDIF}
//old Delphi does not have MSWINDOWS define.
{$IFDEF WIN32}
  {$IFNDEF MSWINDOWS}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit pingsend;

interface

uses
  SysUtils,
  synsock, blcksock, synautil, synafpc, synaip
{$IFDEF MSWINDOWS}
  , windows
{$ENDIF}
  ;

const
  ICMP_ECHO = 8;
  ICMP_ECHOREPLY = 0;
  ICMP_UNREACH = 3;
  ICMP_TIME_EXCEEDED = 11;
//rfc-2292
  ICMP6_ECHO = 128;
  ICMP6_ECHOREPLY = 129;
  ICMP6_UNREACH = 1;
  ICMP6_TIME_EXCEEDED = 3;

type
  {:List of possible ICMP reply packet types.}
  TICMPError = (
    IE_NoError,
    IE_Other,
    IE_TTLExceed,
    IE_UnreachOther,
    IE_UnreachRoute,
    IE_UnreachAdmin,
    IE_UnreachAddr,
    IE_UnreachPort
    );

  {:@abstract(Implementation of ICMP PING and ICMPv6 PING.)}
  TPINGSend = class(TSynaClient)
  private
    FSock: TICMPBlockSocket;
    FBuffer: Ansistring;
    FSeq: Integer;
    FId: Integer;
    FPacketSize: Integer;
    FPingTime: Integer;
    FIcmpEcho: Byte;
    FIcmpEchoReply: Byte;
    FIcmpUnreach: Byte;
    FReplyFrom: string;
    FReplyType: byte;
    FReplyCode: byte;
    FReplyError: TICMPError;
    FReplyErrorDesc: string;
    FTTL: Byte;
    Fsin: TVarSin;
    function Checksum(Value: AnsiString): Word;
    function Checksum6(Value: AnsiString): Word;
    function ReadPacket: Boolean;
    procedure TranslateError;
    procedure TranslateErrorIpHlp(value: integer);
    function InternalPing(const Host: string): Boolean;
    function InternalPingIpHlp(const Host: string): Boolean;
    function IsHostIP6(const Host: string): Boolean;
    procedure GenErrorDesc;
  public
    {:Send ICMP ping to host and count @link(pingtime). If ping OK, result is
     @true.}
    function Ping(const Host: string): Boolean;
    constructor Create;
    destructor Destroy; override;
  published
    {:Size of PING packet. Default size is 32 bytes.}
    property PacketSize: Integer read FPacketSize Write FPacketSize;

    {:Time between request and reply.}
    property PingTime: Integer read FPingTime;

    {:From this address is sended reply for your PING request. It maybe not your
     requested destination, when some error occured!}
    property ReplyFrom: string read FReplyFrom;

    {:ICMP type of PING reply. Each protocol using another values! For IPv4 and
     IPv6 are used different values!}
    property ReplyType: byte read FReplyType;

    {:ICMP code of PING reply. Each protocol using another values! For IPv4 and
     IPv6 are used different values! For protocol independent value look to
     @link(ReplyError)}
    property ReplyCode: byte read FReplyCode;

    {:Return type of returned ICMP message. This value is independent on used
     protocol!}
    property ReplyError: TICMPError read FReplyError;

    {:Return human readable description of returned packet type.}
    property ReplyErrorDesc: string read FReplyErrorDesc;

    {:Socket object used for TCP/IP operation. Good for seting OnStatus hook, etc.}
    property Sock: TICMPBlockSocket read FSock;

    {:TTL value for ICMP query}
    property TTL: byte read FTTL write FTTL;
  end;

{:A very useful function and example of its use would be found in the TPINGSend
 object. Use it to ping to any host. If successful, returns the ping time in
 milliseconds.  Returns -1 if an error occurred.}
function PingHost(const Host: string): Integer;

{:A very useful function and example of its use would be found in the TPINGSend
 object. Use it to TraceRoute to any host.}
function TraceRouteHost(const Host: string): string;

implementation

type
  {:Record for ICMP ECHO packet header.}
  TIcmpEchoHeader = packed record
    i_type: Byte;
    i_code: Byte;
    i_checkSum: Word;
    i_Id: Word;
    i_seq: Word;
    TimeStamp: integer;
  end;

  {:record used internally by TPingSend for compute checksum of ICMPv6 packet
   pseudoheader.}
  TICMP6Packet = packed record
    in_source: TInAddr6;
    in_dest: TInAddr6;
    Length: integer;
    free0: Byte;
    free1: Byte;
    free2: Byte;
    proto: Byte;
  end;

{$IFDEF MSWINDOWS}
const
  DLLIcmpName = 'iphlpapi.dll';
type
  TIP_OPTION_INFORMATION = record
    TTL: Byte;
    TOS: Byte;
    Flags: Byte;
    OptionsSize: Byte;
    OptionsData: PAnsiChar;
  end;
  PIP_OPTION_INFORMATION = ^TIP_OPTION_INFORMATION;

  TICMP_ECHO_REPLY = record
    Address: TInAddr;
    Status: integer;
    RoundTripTime: integer;
    DataSize: Word;
    Reserved: Word;
    Data: pointer;
    Options: TIP_OPTION_INFORMATION;
  end;
  PICMP_ECHO_REPLY = ^TICMP_ECHO_REPLY;

  TICMPV6_ECHO_REPLY = record
    Address: TSockAddrIn6;
    Status: integer;
    RoundTripTime: integer;
  end;
  PICMPV6_ECHO_REPLY = ^TICMPV6_ECHO_REPLY;

  TIcmpCreateFile = function: integer; stdcall;
  TIcmpCloseHandle = function(handle: integer): boolean; stdcall;
  TIcmpSendEcho2 = function(handle: integer; Event: pointer; ApcRoutine: pointer;
    ApcContext: pointer; DestinationAddress: TInAddr; RequestData: pointer;
    RequestSize: integer; RequestOptions: PIP_OPTION_INFORMATION;
    ReplyBuffer: pointer; ReplySize: integer; Timeout: Integer): integer; stdcall;
  TIcmp6CreateFile = function: integer; stdcall;
  TIcmp6SendEcho2 = function(handle: integer; Event: pointer; ApcRoutine: pointer;
    ApcContext: pointer; SourceAddress: PSockAddrIn6; DestinationAddress: PSockAddrIn6;
    RequestData: pointer; RequestSize: integer; RequestOptions: PIP_OPTION_INFORMATION;
    ReplyBuffer: pointer; ReplySize: integer; Timeout: Integer): integer; stdcall;

var
  IcmpDllHandle: TLibHandle = 0;
  IcmpHelper4: boolean = false;
  IcmpHelper6: boolean = false;
  IcmpCreateFile: TIcmpCreateFile = nil;
  IcmpCloseHandle: TIcmpCloseHandle = nil;
  IcmpSendEcho2: TIcmpSendEcho2 = nil;
  Icmp6CreateFile: TIcmp6CreateFile = nil;
  Icmp6SendEcho2: TIcmp6SendEcho2 = nil;
{$ENDIF}
{==============================================================================}

constructor TPINGSend.Create;
begin
  inherited Create;
  FSock := TICMPBlockSocket.Create;
  FSock.Owner := self;
  FTimeout := 5000;
  FPacketSize := 32;
  FSeq := 0;
  Randomize;
  FTTL := 128;
end;

destructor TPINGSend.Destroy;
begin
  FSock.Free;
  inherited Destroy;
end;

function TPINGSend.ReadPacket: Boolean;
begin
  FBuffer := FSock.RecvPacket(Ftimeout);
  Result := FSock.LastError = 0;
end;

procedure TPINGSend.GenErrorDesc;
begin
  case FReplyError of
    IE_NoError:
      FReplyErrorDesc := '';
    IE_Other:
      FReplyErrorDesc := 'Unknown error';
    IE_TTLExceed:
      FReplyErrorDesc := 'TTL Exceeded';
    IE_UnreachOther:
      FReplyErrorDesc := 'Unknown unreachable';
    IE_UnreachRoute:
      FReplyErrorDesc := 'No route to destination';
    IE_UnreachAdmin:
      FReplyErrorDesc := 'Administratively prohibited';
    IE_UnreachAddr:
      FReplyErrorDesc := 'Address unreachable';
    IE_UnreachPort:
      FReplyErrorDesc := 'Port unreachable';
  end;
end;

function TPINGSend.IsHostIP6(const Host: string): Boolean;
var
  f: integer;
begin
  f := AF_UNSPEC;
  if IsIp(Host) then
    f := AF_INET
  else
    if IsIp6(Host) then
      f := AF_INET6;
  synsock.SetVarSin(Fsin, host, '0', f,
    IPPROTO_UDP, SOCK_DGRAM, Fsock.PreferIP4);
  result := Fsin.sin_family = AF_INET6;
end;

function TPINGSend.Ping(const Host: string): Boolean;
var
  b: boolean;
begin
  FPingTime := -1;
  FReplyFrom := '';
  FReplyType := 0;
  FReplyCode := 0;
  FReplyError := IE_Other;
  GenErrorDesc;
  FBuffer := StringOfChar(#55, SizeOf(TICMPEchoHeader) + FPacketSize);
{$IFDEF MSWINDOWS}
  b := IsHostIP6(host);
  if not(b) and IcmpHelper4 then
    result := InternalPingIpHlp(host)
  else
    if b and IcmpHelper6 then
      result := InternalPingIpHlp(host)
    else
      result := InternalPing(host);
{$ELSE}
   result := InternalPing(host);
{$ENDIF}
end;

function TPINGSend.InternalPing(const Host: string): Boolean;
var
  IPHeadPtr: ^TIPHeader;
  IpHdrLen: Integer;
  IcmpEchoHeaderPtr: ^TICMPEchoHeader;
  t: Boolean;
  x: cardinal;
  IcmpReqHead: string;
begin
  Result := False;
  FSock.TTL := FTTL;
  FSock.Bind(FIPInterface, cAnyPort);
  FSock.Connect(Host, '0');
  if FSock.LastError <> 0 then
    Exit;
  FSock.SizeRecvBuffer := 60 * 1024;
  if FSock.IP6used then
  begin
    FIcmpEcho := ICMP6_ECHO;
    FIcmpEchoReply := ICMP6_ECHOREPLY;
    FIcmpUnreach := ICMP6_UNREACH;
  end
  else
  begin
    FIcmpEcho := ICMP_ECHO;
    FIcmpEchoReply := ICMP_ECHOREPLY;
    FIcmpUnreach := ICMP_UNREACH;
  end;
  IcmpEchoHeaderPtr := Pointer(FBuffer);
  with IcmpEchoHeaderPtr^ do
  begin
    i_type := FIcmpEcho;
    i_code := 0;
    i_CheckSum := 0;
    FId := System.Random(32767);
    i_Id := FId;
    TimeStamp := GetTick;
    Inc(FSeq);
    i_Seq := FSeq;
    if fSock.IP6used then
      i_CheckSum := CheckSum6(FBuffer)
    else
      i_CheckSum := CheckSum(FBuffer);
  end;
  FSock.SendString(FBuffer);
  // remember first 8 bytes of ICMP packet
  IcmpReqHead := Copy(FBuffer, 1, 8);
  x := GetTick;
  repeat
    t := ReadPacket;
    if not t then
      break;
    if fSock.IP6used then
    begin
{$IFNDEF MSWINDOWS}
      IcmpEchoHeaderPtr := Pointer(FBuffer);
{$ELSE}
//WinXP SP1 with networking update doing this think by another way ;-O
//      FBuffer := StringOfChar(#0, 4) + FBuffer;
      IcmpEchoHeaderPtr := Pointer(FBuffer);
//      IcmpEchoHeaderPtr^.i_type := FIcmpEchoReply;
{$ENDIF}
    end
    else
    begin
      IPHeadPtr := Pointer(FBuffer);
      IpHdrLen := (IPHeadPtr^.VerLen and $0F) * 4;
      IcmpEchoHeaderPtr := @FBuffer[IpHdrLen + 1];
    end;
  //check for timeout
    if TickDelta(x, GetTick) > FTimeout then
    begin
      t := false;
      Break;
    end;
  //it discard sometimes possible 'echoes' of previosly sended packet
  //or other unwanted ICMP packets...
  until (IcmpEchoHeaderPtr^.i_type <> FIcmpEcho)
    and ((IcmpEchoHeaderPtr^.i_id = FId)
    or (Pos(IcmpReqHead, FBuffer) > 0));
  if t then
    begin
      FPingTime := TickDelta(x, GetTick);
      FReplyFrom := FSock.GetRemoteSinIP;
      FReplyType := IcmpEchoHeaderPtr^.i_type;
      FReplyCode := IcmpEchoHeaderPtr^.i_code;
      TranslateError;
      Result := True;
    end;
end;

function TPINGSend.Checksum(Value: AnsiString): Word;
var
  CkSum: integer;
  Num, Remain: Integer;
  n, i: Integer;
begin
  Num := Length(Value) div 2;
  Remain := Length(Value) mod 2;
  CkSum := 0;
  i := 1;
  for n := 0 to Num - 1 do
  begin
    CkSum := CkSum + Synsock.HtoNs(DecodeInt(Value, i));
    inc(i, 2);
  end;
  if Remain <> 0 then
    CkSum := CkSum + Ord(Value[Length(Value)]);
  CkSum := (CkSum shr 16) + (CkSum and $FFFF);
  CkSum := CkSum + (CkSum shr 16);
  Result := Word(not CkSum);
end;

function TPINGSend.Checksum6(Value: AnsiString): Word;
const
  IOC_OUT = $40000000;
  IOC_IN = $80000000;
  IOC_INOUT = (IOC_IN or IOC_OUT);
  IOC_WS2 = $08000000;
  SIO_ROUTING_INTERFACE_QUERY = 20 or IOC_WS2 or IOC_INOUT;
var
  ICMP6Ptr: ^TICMP6Packet;
  s: AnsiString;
  b: integer;
  ip6: TSockAddrIn6;
  x: integer;
begin
  Result := 0;
{$IFDEF MSWINDOWS}
  s := StringOfChar(#0, SizeOf(TICMP6Packet)) + Value;
  ICMP6Ptr := Pointer(s);
  x := synsock.WSAIoctl(FSock.Socket, SIO_ROUTING_INTERFACE_QUERY,
    @FSock.RemoteSin, SizeOf(FSock.RemoteSin),
    @ip6, SizeOf(ip6), @b, nil, nil);
  if x <> -1 then
    ICMP6Ptr^.in_dest := ip6.sin6_addr
  else
    ICMP6Ptr^.in_dest := FSock.LocalSin.sin6_addr;
  ICMP6Ptr^.in_source := FSock.RemoteSin.sin6_addr;
  ICMP6Ptr^.Length := synsock.htonl(Length(Value));
  ICMP6Ptr^.proto := IPPROTO_ICMPV6;
  Result := Checksum(s);
{$ENDIF}
end;

procedure TPINGSend.TranslateError;
begin
  if fSock.IP6used then
  begin
    case FReplyType of
      ICMP6_ECHOREPLY:
        FReplyError := IE_NoError;
      ICMP6_TIME_EXCEEDED:
        FReplyError := IE_TTLExceed;
      ICMP6_UNREACH:
        case FReplyCode of
          0:
            FReplyError := IE_UnreachRoute;
          3:
            FReplyError := IE_UnreachAddr;
          4:
            FReplyError := IE_UnreachPort;
          1:
            FReplyError := IE_UnreachAdmin;
        else
          FReplyError := IE_UnreachOther;
        end;
    else
      FReplyError := IE_Other;
    end;
  end
  else
  begin
    case FReplyType of
      ICMP_ECHOREPLY:
        FReplyError := IE_NoError;
      ICMP_TIME_EXCEEDED:
        FReplyError := IE_TTLExceed;
      ICMP_UNREACH:
        case FReplyCode of
          0:
            FReplyError := IE_UnreachRoute;
          1:
            FReplyError := IE_UnreachAddr;
          3:
            FReplyError := IE_UnreachPort;
          13:
            FReplyError := IE_UnreachAdmin;
        else
          FReplyError := IE_UnreachOther;
        end;
    else
      FReplyError := IE_Other;
    end;
  end;
  GenErrorDesc;
end;

procedure TPINGSend.TranslateErrorIpHlp(value: integer);
begin
  case value of
    11000, 0:
      FReplyError := IE_NoError;
    11013:
      FReplyError := IE_TTLExceed;
    11002:
      FReplyError := IE_UnreachRoute;
    11003:
      FReplyError := IE_UnreachAddr;
    11005:
      FReplyError := IE_UnreachPort;
    11004:
      FReplyError := IE_UnreachAdmin;
  else
    FReplyError := IE_Other;
  end;
  GenErrorDesc;
end;

function TPINGSend.InternalPingIpHlp(const Host: string): Boolean;
{$IFDEF MSWINDOWS}
var
  PingIp6: boolean;
  PingHandle: integer;
  r: integer;
  ipo: TIP_OPTION_INFORMATION;
  RBuff: Ansistring;
  ip4reply: PICMP_ECHO_REPLY;
  ip6reply: PICMPV6_ECHO_REPLY;
  ip6: TSockAddrIn6;
begin
  Result := False;
  PingIp6 := Fsin.sin_family = AF_INET6;
  if pingIp6 then
    PingHandle := Icmp6CreateFile
  else
    PingHandle := IcmpCreateFile;
  if PingHandle <> -1 then
  begin
    try
      ipo.TTL := FTTL;
      ipo.TOS := 0;
      ipo.Flags := 0;
      ipo.OptionsSize := 0;
      ipo.OptionsData := nil;
      setlength(RBuff, 4096);
      if pingIp6 then
      begin
        FillChar(ip6, sizeof(ip6), 0);
        r := Icmp6SendEcho2(PingHandle, nil, nil, nil, @ip6, @Fsin,
          PAnsichar(FBuffer), length(FBuffer), @ipo, pAnsichar(RBuff), length(RBuff), FTimeout);
        if r > 0 then
        begin
          RBuff := #0 + #0 + RBuff;
          ip6reply := PICMPV6_ECHO_REPLY(pointer(RBuff));
          FPingTime := ip6reply^.RoundTripTime;
          ip6reply^.Address.sin6_family := AF_INET6;
          FReplyFrom := GetSinIp(TVarSin(ip6reply^.Address));
          TranslateErrorIpHlp(ip6reply^.Status);
          Result := True;
        end;
      end
      else
      begin
        r := IcmpSendEcho2(PingHandle, nil, nil, nil, Fsin.sin_addr,
          PAnsichar(FBuffer), length(FBuffer), @ipo, pAnsichar(RBuff), length(RBuff), FTimeout);
        if r > 0 then
        begin
          ip4reply := PICMP_ECHO_REPLY(pointer(RBuff));
          FPingTime := ip4reply^.RoundTripTime;
          FReplyFrom := IpToStr(swapbytes(ip4reply^.Address.S_addr));
          TranslateErrorIpHlp(ip4reply^.Status);
          Result := True;
        end;
      end
    finally
      IcmpCloseHandle(PingHandle);
    end;
  end;
end;
{$ELSE}
begin
  result := false;
end;
{$ENDIF}

{==============================================================================}

function PingHost(const Host: string): Integer;
begin
  with TPINGSend.Create do
  try
    Result := -1;
    if Ping(Host) then
      if ReplyError = IE_NoError then
        Result := PingTime;
  finally
    Free;
  end;
end;

function TraceRouteHost(const Host: string): string;
var
  Ping: TPingSend;
  ttl : byte;
begin
  Result := '';
  Ping := TPINGSend.Create;
  try
    ttl := 1;
    repeat
      ping.TTL := ttl;
      inc(ttl);
      if ttl > 30 then
        Break;
      if not ping.Ping(Host) then
      begin
        Result := Result + cAnyHost+ ' Timeout' + CRLF;
        continue;
      end;
      if (ping.ReplyError <> IE_NoError)
        and (ping.ReplyError <> IE_TTLExceed) then
      begin
        Result := Result + Ping.ReplyFrom + ' ' + Ping.ReplyErrorDesc + CRLF;
        break;
      end;
      Result := Result + Ping.ReplyFrom + ' ' + IntToStr(Ping.PingTime) + CRLF;
    until ping.ReplyError = IE_NoError;
  finally
    Ping.Free;
  end;
end;

{$IFDEF MSWINDOWS}
initialization
begin
  IcmpHelper4 := false;
  IcmpHelper6 := false;
  IcmpDllHandle := LoadLibrary(DLLIcmpName);
  if IcmpDllHandle <> 0 then
  begin
    IcmpCreateFile := GetProcAddress(IcmpDLLHandle, 'IcmpCreateFile');
    IcmpCloseHandle := GetProcAddress(IcmpDLLHandle, 'IcmpCloseHandle');
    IcmpSendEcho2 := GetProcAddress(IcmpDLLHandle, 'IcmpSendEcho2');
    Icmp6CreateFile := GetProcAddress(IcmpDLLHandle, 'Icmp6CreateFile');
    Icmp6SendEcho2 := GetProcAddress(IcmpDLLHandle, 'Icmp6SendEcho2');
    IcmpHelper4 := assigned(IcmpCreateFile)
      and assigned(IcmpCloseHandle)
      and assigned(IcmpSendEcho2);
    IcmpHelper6 := assigned(Icmp6CreateFile)
      and assigned(Icmp6SendEcho2);
  end;
end;

finalization
begin
  FreeLibrary(IcmpDllHandle);
end;
{$ENDIF}

end.
