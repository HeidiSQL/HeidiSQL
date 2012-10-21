{==============================================================================|
| Project : Ararat Synapse                                       | 001.001.004 |
|==============================================================================|
| Content: Socket Independent Platform Layer - FreePascal definition include   |
|==============================================================================|
| Copyright (c)2006-2011, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)2006-2011.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@exclude}

{$IFDEF FPC}
{For FreePascal 2.x.x}

//{$DEFINE FORCEOLDAPI}
{Note about define FORCEOLDAPI:
If you activate this compiler directive, then is allways used old socket API
for name resolution. If you leave this directive inactive, then the new API
is used, when running system allows it.

For IPv6 support you must have new API!
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

{$ifdef FreeBSD}
{$DEFINE SOCK_HAS_SINLEN}               // BSD definition of scoketaddr
{$endif}
{$ifdef darwin}
{$DEFINE SOCK_HAS_SINLEN}               // BSD definition of scoketaddr
{$endif}

interface

uses
  SyncObjs, SysUtils, Classes,
  synafpc, BaseUnix, Unix, termio, sockets, netdb;

function InitSocketInterface(stack: string): Boolean;
function DestroySocketInterface: Boolean;

const
  DLLStackName = '';
  WinsockLevel = $0202;

  cLocalHost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  c6AnyHost = '::0';
  c6Localhost = '::1';
  cLocalHostStr = 'localhost';

type
  TSocket = longint;
  TAddrFamily = integer;

  TMemory = pointer;


type
  TFDSet = Baseunix.TFDSet;
  PFDSet = ^TFDSet;
  Ptimeval = Baseunix.ptimeval;
  Ttimeval = Baseunix.ttimeval;

const
  FIONREAD        = termio.FIONREAD;
  FIONBIO         = termio.FIONBIO;
  FIOASYNC        = termio.FIOASYNC;

const
  IPPROTO_IP     =   0;		{ Dummy					}
  IPPROTO_ICMP   =   1;		{ Internet Control Message Protocol }
  IPPROTO_IGMP   =   2;		{ Internet Group Management Protocol}
  IPPROTO_TCP    =   6;		{ TCP           			}
  IPPROTO_UDP    =   17;	{ User Datagram Protocol		}
  IPPROTO_IPV6   =   41;
  IPPROTO_ICMPV6 =   58;
  IPPROTO_RM     =  113;

  IPPROTO_RAW    =   255;
  IPPROTO_MAX    =   256;

type
  PInAddr = ^TInAddr;
  TInAddr = sockets.in_addr;

  PSockAddrIn = ^TSockAddrIn;
  TSockAddrIn = sockets.TInetSockAddr;


  TIP_mreq =  record
    imr_multiaddr: TInAddr;     // IP multicast address of group
    imr_interface: TInAddr;     // local IP address of interface
  end;


  PInAddr6 = ^TInAddr6;
  TInAddr6 = sockets.Tin6_addr;

  PSockAddrIn6 = ^TSockAddrIn6;
  TSockAddrIn6 = sockets.TInetSockAddr6;


  TIPv6_mreq = record
    ipv6mr_multiaddr: TInAddr6; // IPv6 multicast address.
    ipv6mr_interface: integer;   // Interface index.
  end;

const
  INADDR_ANY       = $00000000;
  INADDR_LOOPBACK  = $7F000001;
  INADDR_BROADCAST = $FFFFFFFF;
  INADDR_NONE      = $FFFFFFFF;
  ADDR_ANY		 = INADDR_ANY;
  INVALID_SOCKET		= TSocket(NOT(0));
  SOCKET_ERROR			= -1;

Const
  IP_TOS             = sockets.IP_TOS;             { int; IP type of service and precedence.  }
  IP_TTL             = sockets.IP_TTL;             { int; IP time to live.  }
  IP_HDRINCL         = sockets.IP_HDRINCL;         { int; Header is included with data.  }
  IP_OPTIONS         = sockets.IP_OPTIONS;         { ip_opts; IP per-packet options.  }
//  IP_ROUTER_ALERT    = sockets.IP_ROUTER_ALERT;    { bool }
  IP_RECVOPTS        = sockets.IP_RECVOPTS;        { bool }
  IP_RETOPTS         = sockets.IP_RETOPTS;         { bool }
//  IP_PKTINFO         = sockets.IP_PKTINFO;         { bool }
//  IP_PKTOPTIONS      = sockets.IP_PKTOPTIONS;
//  IP_PMTUDISC        = sockets.IP_PMTUDISC;        { obsolete name? }
//  IP_MTU_DISCOVER    = sockets.IP_MTU_DISCOVER;    { int; see below }
//  IP_RECVERR         = sockets.IP_RECVERR;         { bool }
//  IP_RECVTTL         = sockets.IP_RECVTTL;         { bool }
//  IP_RECVTOS         = sockets.IP_RECVTOS;         { bool }
  IP_MULTICAST_IF    = sockets.IP_MULTICAST_IF;    { in_addr; set/get IP multicast i/f }
  IP_MULTICAST_TTL   = sockets.IP_MULTICAST_TTL;   { u_char; set/get IP multicast ttl }
  IP_MULTICAST_LOOP  = sockets.IP_MULTICAST_LOOP;  { i_char; set/get IP multicast loopback }
  IP_ADD_MEMBERSHIP  = sockets.IP_ADD_MEMBERSHIP;  { ip_mreq; add an IP group membership }
  IP_DROP_MEMBERSHIP = sockets.IP_DROP_MEMBERSHIP; { ip_mreq; drop an IP group membership }

  SOL_SOCKET    = sockets.SOL_SOCKET;

  SO_DEBUG      = sockets.SO_DEBUG;
  SO_REUSEADDR  = sockets.SO_REUSEADDR;
  SO_TYPE       = sockets.SO_TYPE;
  SO_ERROR      = sockets.SO_ERROR;
  SO_DONTROUTE  = sockets.SO_DONTROUTE;
  SO_BROADCAST  = sockets.SO_BROADCAST;
  SO_SNDBUF     = sockets.SO_SNDBUF;
  SO_RCVBUF     = sockets.SO_RCVBUF;
  SO_KEEPALIVE  = sockets.SO_KEEPALIVE;
  SO_OOBINLINE  = sockets.SO_OOBINLINE;
//  SO_NO_CHECK   = sockets.SO_NO_CHECK;
//  SO_PRIORITY   = sockets.SO_PRIORITY;
  SO_LINGER     = sockets.SO_LINGER;
//  SO_BSDCOMPAT  = sockets.SO_BSDCOMPAT;
//  SO_REUSEPORT  = sockets.SO_REUSEPORT;
//  SO_PASSCRED   = sockets.SO_PASSCRED;
//  SO_PEERCRED   = sockets.SO_PEERCRED;
  SO_RCVLOWAT   = sockets.SO_RCVLOWAT;
  SO_SNDLOWAT   = sockets.SO_SNDLOWAT;
  SO_RCVTIMEO   = sockets.SO_RCVTIMEO;
  SO_SNDTIMEO   = sockets.SO_SNDTIMEO;
{ Security levels - as per NRL IPv6 - don't actually do anything }
//  SO_SECURITY_AUTHENTICATION       = sockets.SO_SECURITY_AUTHENTICATION;
//  SO_SECURITY_ENCRYPTION_TRANSPORT = sockets.SO_SECURITY_ENCRYPTION_TRANSPORT;
//  SO_SECURITY_ENCRYPTION_NETWORK   = sockets.SO_SECURITY_ENCRYPTION_NETWORK;
//  SO_BINDTODEVICE                  = sockets.SO_BINDTODEVICE;
{ Socket filtering }
//  SO_ATTACH_FILTER = sockets.SO_ATTACH_FILTER;
//  SO_DETACH_FILTER = sockets.SO_DETACH_FILTER;

  SOMAXCONN       = 1024;

  IPV6_UNICAST_HOPS     = sockets.IPV6_UNICAST_HOPS;
  IPV6_MULTICAST_IF     = sockets.IPV6_MULTICAST_IF;
  IPV6_MULTICAST_HOPS   = sockets.IPV6_MULTICAST_HOPS;
  IPV6_MULTICAST_LOOP   = sockets.IPV6_MULTICAST_LOOP;
  IPV6_JOIN_GROUP       = sockets.IPV6_JOIN_GROUP;
  IPV6_LEAVE_GROUP      = sockets.IPV6_LEAVE_GROUP;

const
  SOCK_STREAM     = 1;               { stream socket }
  SOCK_DGRAM      = 2;               { datagram socket }
  SOCK_RAW        = 3;               { raw-protocol interface }
  SOCK_RDM        = 4;               { reliably-delivered message }
  SOCK_SEQPACKET  = 5;               { sequenced packet stream }

{ TCP options. }
  TCP_NODELAY     = $0001;

{ Address families. }

  AF_UNSPEC       = 0;               { unspecified }
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }
  AF_INET6        = 10;              { Internetwork Version 6 }
  AF_MAX          = 24;

{ Protocol families, same as address families for now. }
  PF_UNSPEC       = AF_UNSPEC;
  PF_INET         = AF_INET;
  PF_INET6        = AF_INET6;
  PF_MAX          = AF_MAX;

type
{ Structure used for manipulating linger option. }
  PLinger = ^TLinger;
  TLinger = packed record
    l_onoff: integer;
    l_linger: integer;
  end;

const

  MSG_OOB       = sockets.MSG_OOB;      // Process out-of-band data.
  MSG_PEEK      = sockets.MSG_PEEK;     // Peek at incoming messages.
  {$ifdef DARWIN}
  MSG_NOSIGNAL  = $20000;  // Do not generate SIGPIPE.
                           // Works under MAC OS X, but is undocumented,
                           // So FPC doesn't include it
  {$else}
   MSG_NOSIGNAL  = sockets.MSG_NOSIGNAL; // Do not generate SIGPIPE.
  {$endif}

const
  WSAEINTR = ESysEINTR;
  WSAEBADF = ESysEBADF;
  WSAEACCES = ESysEACCES;
  WSAEFAULT = ESysEFAULT;
  WSAEINVAL = ESysEINVAL;
  WSAEMFILE = ESysEMFILE;
  WSAEWOULDBLOCK = ESysEWOULDBLOCK;
  WSAEINPROGRESS = ESysEINPROGRESS;
  WSAEALREADY = ESysEALREADY;
  WSAENOTSOCK = ESysENOTSOCK;
  WSAEDESTADDRREQ = ESysEDESTADDRREQ;
  WSAEMSGSIZE = ESysEMSGSIZE;
  WSAEPROTOTYPE = ESysEPROTOTYPE;
  WSAENOPROTOOPT = ESysENOPROTOOPT;
  WSAEPROTONOSUPPORT = ESysEPROTONOSUPPORT;
  WSAESOCKTNOSUPPORT = ESysESOCKTNOSUPPORT;
  WSAEOPNOTSUPP = ESysEOPNOTSUPP;
  WSAEPFNOSUPPORT = ESysEPFNOSUPPORT;
  WSAEAFNOSUPPORT = ESysEAFNOSUPPORT;
  WSAEADDRINUSE = ESysEADDRINUSE;
  WSAEADDRNOTAVAIL = ESysEADDRNOTAVAIL;
  WSAENETDOWN = ESysENETDOWN;
  WSAENETUNREACH = ESysENETUNREACH;
  WSAENETRESET = ESysENETRESET;
  WSAECONNABORTED = ESysECONNABORTED;
  WSAECONNRESET = ESysECONNRESET;
  WSAENOBUFS = ESysENOBUFS;
  WSAEISCONN = ESysEISCONN;
  WSAENOTCONN = ESysENOTCONN;
  WSAESHUTDOWN = ESysESHUTDOWN;
  WSAETOOMANYREFS = ESysETOOMANYREFS;
  WSAETIMEDOUT = ESysETIMEDOUT;
  WSAECONNREFUSED = ESysECONNREFUSED;
  WSAELOOP = ESysELOOP;
  WSAENAMETOOLONG = ESysENAMETOOLONG;
  WSAEHOSTDOWN = ESysEHOSTDOWN;
  WSAEHOSTUNREACH = ESysEHOSTUNREACH;
  WSAENOTEMPTY = ESysENOTEMPTY;
  WSAEPROCLIM = -1;
  WSAEUSERS = ESysEUSERS;
  WSAEDQUOT = ESysEDQUOT;
  WSAESTALE = ESysESTALE;
  WSAEREMOTE = ESysEREMOTE;
  WSASYSNOTREADY = -2;
  WSAVERNOTSUPPORTED = -3;
  WSANOTINITIALISED = -4;
  WSAEDISCON = -5;
  WSAHOST_NOT_FOUND = 1;
  WSATRY_AGAIN = 2;
  WSANO_RECOVERY = 3;
  WSANO_DATA = -6;

const
  WSADESCRIPTION_LEN     =   256;
  WSASYS_STATUS_LEN      =   128;
type
  PWSAData = ^TWSAData;
  TWSAData = packed record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..WSADESCRIPTION_LEN] of Char;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of Char;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PChar;
  end;

  function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
  function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6):boolean;
  procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
  procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);

var
  in6addr_any, in6addr_loopback : TInAddr6;

procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
procedure FD_ZERO(var FDSet: TFDSet);

{=============================================================================}

var
  SynSockCS: SyncObjs.TCriticalSection;
  SockEnhancedApi: Boolean;
  SockWship6Api: Boolean;

type
  TVarSin = packed record
  {$ifdef SOCK_HAS_SINLEN}
     sin_len     : cuchar;
  {$endif}
    case integer of
      0: (AddressFamily: sa_family_t);
      1: (
        case sin_family: sa_family_t of
          AF_INET: (sin_port: word;
                    sin_addr: TInAddr;
                    sin_zero: array[0..7] of Char);
          AF_INET6: (sin6_port:     word;
                		sin6_flowinfo: longword;
      	    	      sin6_addr:     TInAddr6;
      		          sin6_scope_id: longword);
          );
  end;

function SizeOfVarSin(sin: TVarSin): integer;

  function WSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer;
  function WSACleanup: Integer;
  function WSAGetLastError: Integer;
  function GetHostName: string;
  function Shutdown(s: TSocket; how: Integer): Integer;
  function SetSockOpt(s: TSocket; level, optname: Integer; optval: TMemory;
    optlen: Integer): Integer;
  function GetSockOpt(s: TSocket; level, optname: Integer; optval: TMemory;
    var optlen: Integer): Integer;
  function Send(s: TSocket; Buf: TMemory; len, flags: Integer): Integer;
  function Recv(s: TSocket; Buf: TMemory; len, flags: Integer): Integer;
  function SendTo(s: TSocket; Buf: TMemory; len, flags: Integer; addrto: TVarSin): Integer;
  function RecvFrom(s: TSocket; Buf: TMemory; len, flags: Integer; var from: TVarSin): Integer;
  function ntohs(netshort: word): word;
  function ntohl(netlong: longword): longword;
  function Listen(s: TSocket; backlog: Integer): Integer;
  function IoctlSocket(s: TSocket; cmd: DWORD; var arg: integer): Integer;
  function htons(hostshort: word): word;
  function htonl(hostlong: longword): longword;
  function GetSockName(s: TSocket; var name: TVarSin): Integer;
  function GetPeerName(s: TSocket; var name: TVarSin): Integer;
  function Connect(s: TSocket; const name: TVarSin): Integer;
  function CloseSocket(s: TSocket): Integer;
  function Bind(s: TSocket; const addr: TVarSin): Integer;
  function Accept(s: TSocket; var addr: TVarSin): TSocket;
  function Socket(af, Struc, Protocol: Integer): TSocket;
  function Select(nfds: Integer; readfds, writefds, exceptfds: PFDSet;
    timeout: PTimeVal): Longint;

function IsNewApi(Family: integer): Boolean;
function SetVarSin(var Sin: TVarSin; IP, Port: string; Family, SockProtocol, SockType: integer; PreferIP4: Boolean): integer;
function GetSinIP(Sin: TVarSin): string;
function GetSinPort(Sin: TVarSin): Integer;
procedure ResolveNameToIP(Name: string;  Family, SockProtocol, SockType: integer; const IPList: TStrings);
function ResolveIPToName(IP: string; Family, SockProtocol, SockType: integer): string;
function ResolvePort(Port: string; Family, SockProtocol, SockType: integer): Word;


{==============================================================================}
implementation


function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
begin
  Result := ((a^.u6_addr32[0] = 0) and (a^.u6_addr32[1] = 0) and
             (a^.u6_addr32[2] = 0) and (a^.u6_addr32[3] = 0));
end;

function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
begin
  Result := ((a^.u6_addr32[0] = 0) and (a^.u6_addr32[1] = 0) and
             (a^.u6_addr32[2] = 0) and
             (a^.u6_addr8[12] = 0) and (a^.u6_addr8[13] = 0) and
             (a^.u6_addr8[14] = 0) and (a^.u6_addr8[15] = 1));
end;

function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
begin
  Result := ((a^.u6_addr8[0] = $FE) and (a^.u6_addr8[1] = $80));
end;

function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
begin
  Result := ((a^.u6_addr8[0] = $FE) and (a^.u6_addr8[1] = $C0));
end;

function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
begin
  Result := (a^.u6_addr8[0] = $FF);
end;

function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6): boolean;
begin
  Result := (CompareMem( a, b, sizeof(TInAddr6)));
end;

procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
end;

procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
  a^.u6_addr8[15] := 1;
end;

{=============================================================================}

function WSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer;
begin
  with WSData do
  begin
    wVersion := wVersionRequired;
    wHighVersion := $202;
    szDescription := 'Synsock - Synapse Platform Independent Socket Layer';
    szSystemStatus := 'Running on Unix/Linux by FreePascal';
    iMaxSockets := 32768;
    iMaxUdpDg := 8192;
  end;
  Result := 0;
end;

function WSACleanup: Integer;
begin
  Result := 0;
end;

function WSAGetLastError: Integer;
begin
  Result := fpGetErrno; 
end;

function FD_ISSET(Socket: TSocket; var fdset: TFDSet): Boolean;
begin
  Result := fpFD_ISSET(socket, fdset) <> 0;
end;

procedure FD_SET(Socket: TSocket; var fdset: TFDSet);
begin
  fpFD_SET(Socket, fdset);
end;

procedure FD_CLR(Socket: TSocket; var fdset: TFDSet);
begin
  fpFD_CLR(Socket, fdset);
end;

procedure FD_ZERO(var fdset: TFDSet);
begin
  fpFD_ZERO(fdset);
end;

{=============================================================================}

function SizeOfVarSin(sin: TVarSin): integer;
begin
  case sin.sin_family of
    AF_INET:
            Result := SizeOf(TSockAddrIn);
    AF_INET6:
            Result := SizeOf(TSockAddrIn6);
  else
    Result := 0;
  end;
end;

{=============================================================================}

function Bind(s: TSocket; const addr: TVarSin): Integer;
begin
  if fpBind(s, @addr, SizeOfVarSin(addr)) = 0 then
    Result := 0
  else
    Result := SOCKET_ERROR;
end;

function Connect(s: TSocket; const name: TVarSin): Integer;
begin
  if fpConnect(s, @name, SizeOfVarSin(name)) = 0 then
    Result := 0
  else
    Result := SOCKET_ERROR;
end;

function GetSockName(s: TSocket; var name: TVarSin): Integer;
var
  len: integer;
begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  Result := fpGetSockName(s, @name, @Len);
end;

function GetPeerName(s: TSocket; var name: TVarSin): Integer;
var
  len: integer;
begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  Result := fpGetPeerName(s, @name, @Len);
end;

function GetHostName: string;
begin
  Result := unix.GetHostName;
end;

function Send(s: TSocket; Buf: TMemory; len, flags: Integer): Integer;
begin
  Result := fpSend(s, pointer(Buf), len, flags);
end;

function Recv(s: TSocket; Buf: TMemory; len, flags: Integer): Integer;
begin
  Result := fpRecv(s, pointer(Buf), len, flags);
end;

function SendTo(s: TSocket; Buf: TMemory; len, flags: Integer; addrto: TVarSin): Integer;
begin
  Result := fpSendTo(s, pointer(Buf), len, flags, @addrto, SizeOfVarSin(addrto));
end;

function RecvFrom(s: TSocket; Buf: TMemory; len, flags: Integer; var from: TVarSin): Integer;
var
  x: integer;
begin
  x := SizeOf(from);
  Result := fpRecvFrom(s, pointer(Buf), len, flags, @from, @x);
end;

function Accept(s: TSocket; var addr: TVarSin): TSocket;
var
  x: integer;
begin
  x := SizeOf(addr);
  Result := fpAccept(s, @addr, @x);
end;

function Shutdown(s: TSocket; how: Integer): Integer;
begin
  Result := fpShutdown(s, how);
end;

function SetSockOpt(s: TSocket; level, optname: Integer; optval: Tmemory;
  optlen: Integer): Integer;
begin
  Result := fpsetsockopt(s, level, optname, pointer(optval), optlen);
end;

function GetSockOpt(s: TSocket; level, optname: Integer; optval: Tmemory;
  var optlen: Integer): Integer;
begin
  Result := fpgetsockopt(s, level, optname, pointer(optval), @optlen);
end;

function  ntohs(netshort: word): word;
begin
  Result := sockets.ntohs(NetShort);
end;

function  ntohl(netlong: longword): longword;
begin
  Result := sockets.ntohl(NetLong);
end;

function  Listen(s: TSocket; backlog: Integer): Integer;
begin
  if fpListen(s, backlog) = 0 then
    Result := 0
  else
    Result := SOCKET_ERROR;
end;

function  IoctlSocket(s: TSocket; cmd: DWORD; var arg: integer): Integer;
begin
  Result := fpIoctl(s, cmd, @arg);
end;

function  htons(hostshort: word): word;
begin
  Result := sockets.htons(Hostshort);
end;

function  htonl(hostlong: longword): longword;
begin
  Result := sockets.htonl(HostLong);
end;

function CloseSocket(s: TSocket): Integer;
begin
  Result := sockets.CloseSocket(s);
end;

function Socket(af, Struc, Protocol: Integer): TSocket;
begin
  Result := fpSocket(af, struc, protocol);
end;

function Select(nfds: Integer; readfds, writefds, exceptfds: PFDSet;
  timeout: PTimeVal): Longint;
begin
  Result := fpSelect(nfds, readfds, writefds, exceptfds, timeout);
end;

{=============================================================================}
function IsNewApi(Family: integer): Boolean;
begin
  Result := SockEnhancedApi;
  if not Result then
    Result := (Family = AF_INET6) and SockWship6Api;
end;

function SetVarSin(var Sin: TVarSin; IP, Port: string; Family, SockProtocol, SockType: integer; PreferIP4: Boolean): integer;
var
  TwoPass: boolean;
  f1, f2: integer;

  function GetAddr(f:integer): integer;
  var
    a4: array [1..1] of in_addr;
    a6: array [1..1] of Tin6_addr;
    he: THostEntry;
  begin
    Result := WSAEPROTONOSUPPORT;
    case f of
      AF_INET:
        begin
          if IP = cAnyHost then
          begin
            Sin.sin_family := AF_INET;
            Result := 0;
          end
          else
          begin
            if lowercase(IP) = cLocalHostStr then
              a4[1].s_addr := htonl(INADDR_LOOPBACK)
            else
            begin
              a4[1].s_addr := 0;
              Result := WSAHOST_NOT_FOUND;
              a4[1] := StrTonetAddr(IP);
              if a4[1].s_addr = INADDR_ANY then
                if GetHostByName(ip, he) then
                  a4[1]:=HostToNet(he.Addr)
                else
                  Resolvename(ip, a4);
            end;
            if a4[1].s_addr <> INADDR_ANY then
            begin
              Sin.sin_family := AF_INET;
              sin.sin_addr := a4[1];
              Result := 0;
            end;
          end;
        end;
      AF_INET6:
        begin
          if IP = c6AnyHost then
          begin
            Sin.sin_family := AF_INET6;
            Result := 0;
          end
          else
          begin
            if lowercase(IP) = cLocalHostStr then
              SET_LOOPBACK_ADDR6(@a6[1])
            else
            begin
              Result := WSAHOST_NOT_FOUND;
              SET_IN6_IF_ADDR_ANY(@a6[1]);
              a6[1] := StrTonetAddr6(IP);
              if IN6_IS_ADDR_UNSPECIFIED(@a6[1]) then
                Resolvename6(ip, a6);
            end;
            if not IN6_IS_ADDR_UNSPECIFIED(@a6[1]) then
            begin
              Sin.sin_family := AF_INET6;
              sin.sin6_addr := a6[1];
              Result := 0;
            end;
          end;
        end;
    end;
  end;
begin
  Result := 0;
  FillChar(Sin, Sizeof(Sin), 0);
  Sin.sin_port := Resolveport(port, family, SockProtocol, SockType);
  TwoPass := False;
  if Family = AF_UNSPEC then
  begin
    if PreferIP4 then
    begin
      f1 := AF_INET;
      f2 := AF_INET6;
      TwoPass := True;
    end
    else
    begin
      f2 := AF_INET;
      f1 := AF_INET6;
      TwoPass := True;
    end;
  end
  else
    f1 := Family;
  Result := GetAddr(f1);
  if Result <> 0 then
    if TwoPass then
      Result := GetAddr(f2);
end;

function GetSinIP(Sin: TVarSin): string;
begin
  Result := '';
  case sin.AddressFamily of
    AF_INET:
      begin
        result := NetAddrToStr(sin.sin_addr);
      end;
    AF_INET6:
      begin
        result := NetAddrToStr6(sin.sin6_addr);
      end;
  end;
end;

function GetSinPort(Sin: TVarSin): Integer;
begin
  if (Sin.sin_family = AF_INET6) then
    Result := synsock.ntohs(Sin.sin6_port)
  else
    Result := synsock.ntohs(Sin.sin_port);
end;

procedure ResolveNameToIP(Name: string; Family, SockProtocol, SockType: integer; const IPList: TStrings);
var
  x, n: integer;
  a4: array [1..255] of in_addr;
  a6: array [1..255] of Tin6_addr;
  he: THostEntry;
begin
  IPList.Clear;
  if (family = AF_INET) or (family = AF_UNSPEC) then
  begin
    if lowercase(name) = cLocalHostStr then
      IpList.Add(cLocalHost)
    else
    begin
      a4[1] := StrTonetAddr(name);
      if a4[1].s_addr = INADDR_ANY then
        if GetHostByName(name, he) then
        begin
          a4[1]:=HostToNet(he.Addr);
          x := 1;
        end
        else
          x := Resolvename(name, a4)
      else
        x := 1;
      for n := 1  to x do
        IpList.Add(netaddrToStr(a4[n]));
    end;
  end;

  if (family = AF_INET6) or (family = AF_UNSPEC) then
  begin
    if lowercase(name) = cLocalHostStr then
      IpList.Add(c6LocalHost)
    else
    begin
      a6[1] := StrTonetAddr6(name);
      if IN6_IS_ADDR_UNSPECIFIED(@a6[1]) then
        x := Resolvename6(name, a6)
      else
        x := 1;
      for n := 1  to x do
        IpList.Add(netaddrToStr6(a6[n]));
    end;
  end;

  if IPList.Count = 0 then
    IPList.Add(cLocalHost);
end;

function ResolvePort(Port: string; Family, SockProtocol, SockType: integer): Word;
var
  ProtoEnt: TProtocolEntry;
  ServEnt: TServiceEntry;
begin
  Result := synsock.htons(StrToIntDef(Port, 0));
  if Result = 0 then
  begin
    ProtoEnt.Name := '';
    GetProtocolByNumber(SockProtocol, ProtoEnt);
    ServEnt.port := 0;
    GetServiceByName(Port, ProtoEnt.Name, ServEnt);
    Result := ServEnt.port;  
  end;
end;

function ResolveIPToName(IP: string; Family, SockProtocol, SockType: integer): string;
var
  n: integer;
  a4: array [1..1] of in_addr;
  a6: array [1..1] of Tin6_addr;
  a: array [1..1] of string;
begin
  Result := IP;
  a4[1] := StrToNetAddr(IP);
  if a4[1].s_addr <> INADDR_ANY then
  begin
//why ResolveAddress need address in HOST order? :-O 
    n := ResolveAddress(nettohost(a4[1]), a);
    if n > 0 then
      Result := a[1];
  end
  else
  begin
    a6[1] := StrToNetAddr6(IP);
    n := ResolveAddress6(a6[1], a);
    if n > 0 then
      Result := a[1];
  end;
end;

{=============================================================================}

function InitSocketInterface(stack: string): Boolean;
begin
  SockEnhancedApi := False;
  SockWship6Api := False;
//  Libc.Signal(Libc.SIGPIPE, TSignalHandler(Libc.SIG_IGN));
  Result := True;
end;

function DestroySocketInterface: Boolean;
begin
  Result := True;
end;

initialization
begin
  SynSockCS := SyncObjs.TCriticalSection.Create;
  SET_IN6_IF_ADDR_ANY (@in6addr_any);
  SET_LOOPBACK_ADDR6  (@in6addr_loopback);
end;

finalization
begin
  SynSockCS.Free;
end;

{$ENDIF}

