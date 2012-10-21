{==============================================================================|
| Project : Ararat Synapse                                       | 001.000.002 |
|==============================================================================|
| Content: Socket Independent Platform Layer - .NET definition include         |
|==============================================================================|
| Copyright (c)2004, Lukas Gebauer                                             |
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
| Portions created by Lukas Gebauer are Copyright (c)2004.                     |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@exclude}

{$IFDEF CIL}

interface

uses
  SyncObjs, SysUtils, Classes,
  System.Net,
  System.Net.Sockets;

const
  DLLStackName = '';
  WinsockLevel = $0202;

function InitSocketInterface(stack: string): Boolean;
function DestroySocketInterface: Boolean;

type
  u_char = Char;
  u_short = Word;
  u_int = Integer;
  u_long = Longint;
  pu_long = ^u_long;
  pu_short = ^u_short;
  PSockAddr = IPEndPoint;
  DWORD = integer;
  ULong = cardinal;
  TMemory = Array of byte;
  TLinger = LingerOption;
  TSocket = socket;
  TAddrFamily = AddressFamily;

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
//    lpVendorInfo: PChar;
  end;

const
  MSG_NOSIGNAL = 0;
  INVALID_SOCKET = nil;
  AF_UNSPEC = AddressFamily.Unspecified;
  AF_INET = AddressFamily.InterNetwork;
  AF_INET6 = AddressFamily.InterNetworkV6;
  SOCKET_ERROR = integer(-1);

  FIONREAD     = integer($4004667f);
  FIONBIO      = integer($8004667e);
  FIOASYNC     = integer($8004667d);

  SOMAXCONN       = integer($7fffffff);

  IPPROTO_IP = ProtocolType.IP;
  IPPROTO_ICMP = ProtocolType.Icmp;
  IPPROTO_IGMP = ProtocolType.Igmp;
  IPPROTO_TCP = ProtocolType.Tcp;
  IPPROTO_UDP = ProtocolType.Udp;
  IPPROTO_RAW = ProtocolType.Raw;
  IPPROTO_IPV6 = ProtocolType.IPV6;
//
  IPPROTO_ICMPV6 = ProtocolType.Icmp; //??

  SOCK_STREAM     = SocketType.Stream;
  SOCK_DGRAM      = SocketType.Dgram;
  SOCK_RAW        = SocketType.Raw;
  SOCK_RDM        = SocketType.Rdm;
  SOCK_SEQPACKET  = SocketType.Seqpacket;

  SOL_SOCKET = SocketOptionLevel.Socket;
  SOL_IP = SocketOptionLevel.Ip;


  IP_OPTIONS          = SocketOptionName.IPOptions;
  IP_HDRINCL          = SocketOptionName.HeaderIncluded;
  IP_TOS              = SocketOptionName.TypeOfService;           { set/get IP Type Of Service       }
  IP_TTL              = SocketOptionName.IpTimeToLive;           { set/get IP Time To Live          }
  IP_MULTICAST_IF     = SocketOptionName.MulticastInterface;           { set/get IP multicast interface   }
  IP_MULTICAST_TTL    = SocketOptionName.MulticastTimeToLive;           { set/get IP multicast timetolive  }
  IP_MULTICAST_LOOP   = SocketOptionName.MulticastLoopback;           { set/get IP multicast loopback    }
  IP_ADD_MEMBERSHIP   = SocketOptionName.AddMembership;           { add  an IP group membership      }
  IP_DROP_MEMBERSHIP  = SocketOptionName.DropMembership;           { drop an IP group membership      }
  IP_DONTFRAGMENT     = SocketOptionName.DontFragment;           { set/get IP Don't Fragment flag   }

  IPV6_UNICAST_HOPS      = 8;  // TTL
  IPV6_MULTICAST_IF      = 9;  // set/get IP multicast i/f
  IPV6_MULTICAST_HOPS    = 10; // set/get IP multicast ttl
  IPV6_MULTICAST_LOOP    = 11; // set/get IP multicast loopback
  IPV6_JOIN_GROUP        = 12; // add an IP group membership
  IPV6_LEAVE_GROUP       = 13; // drop an IP group membership

  SO_DEBUG        = SocketOptionName.Debug;          { turn on debugging info recording }
  SO_ACCEPTCONN   = SocketOptionName.AcceptConnection;          { socket has had listen() }
  SO_REUSEADDR    = SocketOptionName.ReuseAddress;          { allow local address reuse }
  SO_KEEPALIVE    = SocketOptionName.KeepAlive;          { keep connections alive }
  SO_DONTROUTE    = SocketOptionName.DontRoute;          { just use interface addresses }
  SO_BROADCAST    = SocketOptionName.Broadcast;          { permit sending of broadcast msgs }
  SO_USELOOPBACK  = SocketOptionName.UseLoopback;          { bypass hardware when possible }
  SO_LINGER       = SocketOptionName.Linger;          { linger on close if data present }
  SO_OOBINLINE    = SocketOptionName.OutOfBandInline;          { leave received OOB data in line }
  SO_DONTLINGER   = SocketOptionName.DontLinger;
{ Additional options. }
  SO_SNDBUF       = SocketOptionName.SendBuffer;          { send buffer size }
  SO_RCVBUF       = SocketOptionName.ReceiveBuffer;          { receive buffer size }
  SO_SNDLOWAT     = SocketOptionName.SendLowWater;          { send low-water mark }
  SO_RCVLOWAT     = SocketOptionName.ReceiveLowWater;          { receive low-water mark }
  SO_SNDTIMEO     = SocketOptionName.SendTimeout;          { send timeout }
  SO_RCVTIMEO     = SocketOptionName.ReceiveTimeout;          { receive timeout }
  SO_ERROR        = SocketOptionName.Error;          { get error status and clear }
  SO_TYPE         = SocketOptionName.Type;          { get socket type }

{ WinSock 2 extension -- new options }
//  SO_GROUP_ID       = $2001; { ID of a socket group}
//  SO_GROUP_PRIORITY = $2002; { the relative priority within a group}
//  SO_MAX_MSG_SIZE   = $2003; { maximum message size }
//  SO_PROTOCOL_INFOA = $2004; { WSAPROTOCOL_INFOA structure }
//  SO_PROTOCOL_INFOW = $2005; { WSAPROTOCOL_INFOW structure }
//  SO_PROTOCOL_INFO  = SO_PROTOCOL_INFOA;
//  PVD_CONFIG        = $3001; {configuration info for service provider }
{ Option for opening sockets for synchronous access. }
//  SO_OPENTYPE     = $7008;
//  SO_SYNCHRONOUS_ALERT    = $10;
//  SO_SYNCHRONOUS_NONALERT = $20;
{ Other NT-specific options. }
//  SO_MAXDG        = $7009;
//  SO_MAXPATHDG    = $700A;
//  SO_UPDATE_ACCEPT_CONTEXT     = $700B;
//  SO_CONNECT_TIME = $700C;


  { All Windows Sockets error constants are biased by WSABASEERR from the "normal" }
  WSABASEERR              = 10000;

{ Windows Sockets definitions of regular Microsoft C error constants }

  WSAEINTR                = (WSABASEERR+4);
  WSAEBADF                = (WSABASEERR+9);
  WSAEACCES               = (WSABASEERR+13);
  WSAEFAULT               = (WSABASEERR+14);
  WSAEINVAL               = (WSABASEERR+22);
  WSAEMFILE               = (WSABASEERR+24);

{ Windows Sockets definitions of regular Berkeley error constants }

  WSAEWOULDBLOCK          = (WSABASEERR+35);
  WSAEINPROGRESS          = (WSABASEERR+36);
  WSAEALREADY             = (WSABASEERR+37);
  WSAENOTSOCK             = (WSABASEERR+38);
  WSAEDESTADDRREQ         = (WSABASEERR+39);
  WSAEMSGSIZE             = (WSABASEERR+40);
  WSAEPROTOTYPE           = (WSABASEERR+41);
  WSAENOPROTOOPT          = (WSABASEERR+42);
  WSAEPROTONOSUPPORT      = (WSABASEERR+43);
  WSAESOCKTNOSUPPORT      = (WSABASEERR+44);
  WSAEOPNOTSUPP           = (WSABASEERR+45);
  WSAEPFNOSUPPORT         = (WSABASEERR+46);
  WSAEAFNOSUPPORT         = (WSABASEERR+47);
  WSAEADDRINUSE           = (WSABASEERR+48);
  WSAEADDRNOTAVAIL        = (WSABASEERR+49);
  WSAENETDOWN             = (WSABASEERR+50);
  WSAENETUNREACH          = (WSABASEERR+51);
  WSAENETRESET            = (WSABASEERR+52);
  WSAECONNABORTED         = (WSABASEERR+53);
  WSAECONNRESET           = (WSABASEERR+54);
  WSAENOBUFS              = (WSABASEERR+55);
  WSAEISCONN              = (WSABASEERR+56);
  WSAENOTCONN             = (WSABASEERR+57);
  WSAESHUTDOWN            = (WSABASEERR+58);
  WSAETOOMANYREFS         = (WSABASEERR+59);
  WSAETIMEDOUT            = (WSABASEERR+60);
  WSAECONNREFUSED         = (WSABASEERR+61);
  WSAELOOP                = (WSABASEERR+62);
  WSAENAMETOOLONG         = (WSABASEERR+63);
  WSAEHOSTDOWN            = (WSABASEERR+64);
  WSAEHOSTUNREACH         = (WSABASEERR+65);
  WSAENOTEMPTY            = (WSABASEERR+66);
  WSAEPROCLIM             = (WSABASEERR+67);
  WSAEUSERS               = (WSABASEERR+68);
  WSAEDQUOT               = (WSABASEERR+69);
  WSAESTALE               = (WSABASEERR+70);
  WSAEREMOTE              = (WSABASEERR+71);

{ Extended Windows Sockets error constant definitions }

  WSASYSNOTREADY          = (WSABASEERR+91);
  WSAVERNOTSUPPORTED      = (WSABASEERR+92);
  WSANOTINITIALISED       = (WSABASEERR+93);
  WSAEDISCON              = (WSABASEERR+101);
  WSAENOMORE              = (WSABASEERR+102);
  WSAECANCELLED           = (WSABASEERR+103);
  WSAEEINVALIDPROCTABLE   = (WSABASEERR+104);
  WSAEINVALIDPROVIDER     = (WSABASEERR+105);
  WSAEPROVIDERFAILEDINIT  = (WSABASEERR+106);
  WSASYSCALLFAILURE       = (WSABASEERR+107);
  WSASERVICE_NOT_FOUND    = (WSABASEERR+108);
  WSATYPE_NOT_FOUND       = (WSABASEERR+109);
  WSA_E_NO_MORE           = (WSABASEERR+110);
  WSA_E_CANCELLED         = (WSABASEERR+111);
  WSAEREFUSED             = (WSABASEERR+112);

{ Error return codes from gethostbyname() and gethostbyaddr()
  (when using the resolver). Note that these errors are
  retrieved via WSAGetLastError() and must therefore follow
  the rules for avoiding clashes with error numbers from
  specific implementations or language run-time systems.
  For this reason the codes are based at WSABASEERR+1001.
  Note also that [WSA]NO_ADDRESS is defined only for
  compatibility purposes. }

{ Authoritative Answer: Host not found }
  WSAHOST_NOT_FOUND       = (WSABASEERR+1001);
  HOST_NOT_FOUND          = WSAHOST_NOT_FOUND;
{ Non-Authoritative: Host not found, or SERVERFAIL }
  WSATRY_AGAIN            = (WSABASEERR+1002);
  TRY_AGAIN               = WSATRY_AGAIN;
{ Non recoverable errors, FORMERR, REFUSED, NOTIMP }
  WSANO_RECOVERY          = (WSABASEERR+1003);
  NO_RECOVERY             = WSANO_RECOVERY;
{ Valid name, no data record of requested type }
  WSANO_DATA              = (WSABASEERR+1004);
  NO_DATA                 = WSANO_DATA;
{ no address, look for MX record }
  WSANO_ADDRESS           = WSANO_DATA;
  NO_ADDRESS              = WSANO_ADDRESS;

  EWOULDBLOCK        =  WSAEWOULDBLOCK;
  EINPROGRESS        =  WSAEINPROGRESS;
  EALREADY           =  WSAEALREADY;
  ENOTSOCK           =  WSAENOTSOCK;
  EDESTADDRREQ       =  WSAEDESTADDRREQ;
  EMSGSIZE           =  WSAEMSGSIZE;
  EPROTOTYPE         =  WSAEPROTOTYPE;
  ENOPROTOOPT        =  WSAENOPROTOOPT;
  EPROTONOSUPPORT    =  WSAEPROTONOSUPPORT;
  ESOCKTNOSUPPORT    =  WSAESOCKTNOSUPPORT;
  EOPNOTSUPP         =  WSAEOPNOTSUPP;
  EPFNOSUPPORT       =  WSAEPFNOSUPPORT;
  EAFNOSUPPORT       =  WSAEAFNOSUPPORT;
  EADDRINUSE         =  WSAEADDRINUSE;
  EADDRNOTAVAIL      =  WSAEADDRNOTAVAIL;
  ENETDOWN           =  WSAENETDOWN;
  ENETUNREACH        =  WSAENETUNREACH;
  ENETRESET          =  WSAENETRESET;
  ECONNABORTED       =  WSAECONNABORTED;
  ECONNRESET         =  WSAECONNRESET;
  ENOBUFS            =  WSAENOBUFS;
  EISCONN            =  WSAEISCONN;
  ENOTCONN           =  WSAENOTCONN;
  ESHUTDOWN          =  WSAESHUTDOWN;
  ETOOMANYREFS       =  WSAETOOMANYREFS;
  ETIMEDOUT          =  WSAETIMEDOUT;
  ECONNREFUSED       =  WSAECONNREFUSED;
  ELOOP              =  WSAELOOP;
  ENAMETOOLONG       =  WSAENAMETOOLONG;
  EHOSTDOWN          =  WSAEHOSTDOWN;
  EHOSTUNREACH       =  WSAEHOSTUNREACH;
  ENOTEMPTY          =  WSAENOTEMPTY;
  EPROCLIM           =  WSAEPROCLIM;
  EUSERS             =  WSAEUSERS;
  EDQUOT             =  WSAEDQUOT;
  ESTALE             =  WSAESTALE;
  EREMOTE            =  WSAEREMOTE;


type
  TVarSin = IPEndpoint;

{  function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
  function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6):boolean;
  procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
  procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);
var
  in6addr_any, in6addr_loopback : TInAddr6;
}

{procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
procedure FD_ZERO(var FDSet: TFDSet);
}
{=============================================================================}

  function WSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer;
  function WSACleanup: Integer;
  function WSAGetLastError: Integer;
  function WSAGetLastErrorDesc: String;
  function GetHostName: string;
  function Shutdown(s: TSocket; how: Integer): Integer;
//  function SetSockOpt(s: TSocket; level, optname: Integer; optval: PChar;
//    optlen: Integer): Integer;
  function SetSockOpt(s: TSocket; level, optname: Integer; optval: TMemory;
    optlen: Integer): Integer;
  function SetSockOptObj(s: TSocket; level, optname: Integer; optval: TObject): Integer;
  function GetSockOpt(s: TSocket; level, optname: Integer; optval: TMemory;
    var optlen: Integer): Integer;
//  function SendTo(s: TSocket; const Buf; len, flags: Integer; addrto: PSockAddr;
//    tolen: Integer): Integer;
///  function SendTo(s: TSocket; const Buf; len, flags: Integer; addrto: TVarSin): Integer;
///  function Send(s: TSocket; const Buf; len, flags: Integer): Integer;
///  function  Recv(s: TSocket; var Buf; len, flags: Integer): Integer;
//  function  RecvFrom(s: TSocket; var Buf; len, flags: Integer; from: PSockAddr;
//    var fromlen: Integer): Integer;
///  function  RecvFrom(s: TSocket; var Buf; len, flags: Integer; from: TVarSin): Integer;
function Send(s: TSocket; Buf: TMemory; len, flags: Integer): Integer;
function Recv(s: TSocket; Buf: TMemory; len, flags: Integer): Integer;
function SendTo(s: TSocket; Buf: TMemory; len, flags: Integer; addrto: TVarSin): Integer;
function RecvFrom(s: TSocket; Buf: TMemory; len, flags: Integer; var from: TVarSin): Integer;
  function  ntohs(netshort: u_short): u_short;
  function  ntohl(netlong: u_long): u_long;
  function  Listen(s: TSocket; backlog: Integer): Integer;
  function  IoctlSocket(s: TSocket; cmd: DWORD; var arg: integer): Integer;
  function  htons(hostshort: u_short): u_short;
  function  htonl(hostlong: u_long): u_long;
//  function  GetSockName(s: TSocket; name: PSockAddr; var namelen: Integer): Integer;
  function  GetSockName(s: TSocket; var name: TVarSin): Integer;
//  function  GetPeerName(s: TSocket; name: PSockAddr; var namelen: Integer): Integer;
  function  GetPeerName(s: TSocket; var name: TVarSin): Integer;
//  function  Connect(s: TSocket; name: PSockAddr; namelen: Integer): Integer;
  function  Connect(s: TSocket; const name: TVarSin): Integer;
  function  CloseSocket(s: TSocket): Integer;
//  function  Bind(s: TSocket; addr: PSockAddr; namelen: Integer): Integer;
  function  Bind(s: TSocket; const addr: TVarSin): Integer;
//  function  Accept(s: TSocket; addr: PSockAddr; var addrlen: Integer): TSocket;
  function  Accept(s: TSocket; var addr: TVarSin): TSocket;
  function  Socket(af, Struc, Protocol: Integer): TSocket;
//  Select = function(nfds: Integer; readfds, writefds, exceptfds: PFDSet;
//    timeout: PTimeVal): Longint;
//    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF};

//  TWSAIoctl = function (s: TSocket; dwIoControlCode: DWORD; lpvInBuffer: Pointer;
//    cbInBuffer: DWORD; lpvOutBuffer: Pointer; cbOutBuffer: DWORD;
//    lpcbBytesReturned: PDWORD; lpOverlapped: Pointer;
//    lpCompletionRoutine: pointer): u_int;
//    stdcall;

  function GetPortService(value: string): integer;

function IsNewApi(Family: TAddrFamily): Boolean;
function SetVarSin(var Sin: TVarSin; IP, Port: string; Family: TAddrFamily; SockProtocol, SockType: integer; PreferIP4: Boolean): integer;
function GetSinIP(Sin: TVarSin): string;
function GetSinPort(Sin: TVarSin): Integer;
procedure ResolveNameToIP(Name: string; Family: TAddrFamily; SockProtocol, SockType: integer; const IPList: TStrings);
function ResolveIPToName(IP: string; Family: TAddrFamily; SockProtocol, SockType: integer): string;
function ResolvePort(Port: string; Family: TAddrFamily; SockProtocol, SockType: integer): Word;

var
  SynSockCS: SyncObjs.TCriticalSection;
  SockEnhancedApi: Boolean;
  SockWship6Api: Boolean;

{==============================================================================}
implementation

threadvar
  WSALastError: integer;
  WSALastErrorDesc: string;

var
  services: Array [0..139, 0..1] of string =
  (
    ('echo', '7'),
    ('discard', '9'),
    ('sink', '9'),
    ('null', '9'),
    ('systat', '11'),
    ('users', '11'),
    ('daytime', '13'),
    ('qotd', '17'),
    ('quote', '17'),
    ('chargen', '19'),
    ('ttytst', '19'),
    ('source', '19'),
    ('ftp-data', '20'),
    ('ftp', '21'),
    ('telnet', '23'),
    ('smtp', '25'),
    ('mail', '25'),
    ('time', '37'),
    ('timeserver', '37'),
    ('rlp', '39'),
    ('nameserver', '42'),
    ('name', '42'),
    ('nickname', '43'),
    ('whois', '43'),
    ('domain', '53'),
    ('bootps', '67'),
    ('dhcps', '67'),
    ('bootpc', '68'),
    ('dhcpc', '68'),
    ('tftp', '69'),
    ('gopher', '70'),
    ('finger', '79'),
    ('http', '80'),
    ('www', '80'),
    ('www-http', '80'),
    ('kerberos', '88'),
    ('hostname', '101'),
    ('hostnames', '101'),
    ('iso-tsap', '102'),
    ('rtelnet', '107'),
    ('pop2', '109'),
    ('postoffice', '109'),
    ('pop3', '110'),
    ('sunrpc', '111'),
    ('rpcbind', '111'),
    ('portmap', '111'),
    ('auth', '113'),
    ('ident', '113'),
    ('tap', '113'),
    ('uucp-path', '117'),
    ('nntp', '119'),
    ('usenet', '119'),
    ('ntp', '123'),
    ('epmap', '135'),
    ('loc-srv', '135'),
    ('netbios-ns', '137'),
    ('nbname', '137'),
    ('netbios-dgm', '138'),
    ('nbdatagram', '138'),
    ('netbios-ssn', '139'),
    ('nbsession', '139'),
    ('imap', '143'),
    ('imap4', '143'),
    ('pcmail-srv', '158'),
    ('snmp', '161'),
    ('snmptrap', '162'),
    ('snmp-trap', '162'),
    ('print-srv', '170'),
    ('bgp', '179'),
    ('irc', '194'),
    ('ipx', '213'),
    ('ldap', '389'),
    ('https', '443'),
    ('mcom', '443'),
    ('microsoft-ds', '445'),
    ('kpasswd', '464'),
    ('isakmp', '500'),
    ('ike', '500'),
    ('exec', '512'),
    ('biff', '512'),
    ('comsat', '512'),
    ('login', '513'),
    ('who', '513'),
    ('whod', '513'),
    ('cmd', '514'),
    ('shell', '514'),
    ('syslog', '514'),
    ('printer', '515'),
    ('spooler', '515'),
    ('talk', '517'),
    ('ntalk', '517'),
    ('efs', '520'),
    ('router', '520'),
    ('route', '520'),
    ('routed', '520'),
    ('timed', '525'),
    ('timeserver', '525'),
    ('tempo', '526'),
    ('newdate', '526'),
    ('courier', '530'),
    ('rpc', '530'),
    ('conference', '531'),
    ('chat', '531'),
    ('netnews', '532'),
    ('readnews', '532'),
    ('netwall', '533'),
    ('uucp', '540'),
    ('uucpd', '540'),
    ('klogin', '543'),
    ('kshell', '544'),
    ('krcmd', '544'),
    ('new-rwho', '550'),
    ('new-who', '550'),
    ('remotefs', '556'),
    ('rfs', '556'),
    ('rfs_server', '556'),
    ('rmonitor', '560'),
    ('rmonitord', '560'),
    ('monitor', '561'),
    ('ldaps', '636'),
    ('sldap', '636'),
    ('doom', '666'),
    ('kerberos-adm', '749'),
    ('kerberos-iv', '750'),
    ('kpop', '1109'),
    ('phone', '1167'),
    ('ms-sql-s', '1433'),
    ('ms-sql-m', '1434'),
    ('wins', '1512'),
    ('ingreslock', '1524'),
    ('ingres', '1524'),
    ('l2tp', '1701'),
    ('pptp', '1723'),
    ('radius', '1812'),
    ('radacct', '1813'),
    ('nfsd', '2049'),
    ('nfs', '2049'),
    ('knetd', '2053'),
    ('gds_db', '3050'),
    ('man', '9535')
    );

{function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
begin
  Result := ((a^.s_un_dw.s_dw1 = 0) and (a^.s_un_dw.s_dw2 = 0) and
             (a^.s_un_dw.s_dw3 = 0) and (a^.s_un_dw.s_dw4 = 0));
end;

function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
begin
  Result := ((a^.s_un_dw.s_dw1 = 0) and (a^.s_un_dw.s_dw2 = 0) and
             (a^.s_un_dw.s_dw3 = 0) and
             (a^.s_un_b.s_b13 = char(0)) and (a^.s_un_b.s_b14 = char(0)) and
             (a^.s_un_b.s_b15 = char(0)) and (a^.s_un_b.s_b16 = char(1)));
end;

function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
begin
  Result := ((a^.s_un_b.s_b1 = u_char($FE)) and (a^.s_un_b.s_b2 = u_char($80)));
end;

function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
begin
  Result := ((a^.s_un_b.s_b1 = u_char($FE)) and (a^.s_un_b.s_b2 = u_char($C0)));
end;

function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
begin
  Result := (a^.s_un_b.s_b1 = char($FF));
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
  a^.s_un_b.s_b16 := char(1);
end;
}

{=============================================================================}

procedure NullErr;
begin
  WSALastError := 0;
  WSALastErrorDesc := '';
end;

procedure GetErrCode(E: System.Exception);
var
  SE: System.Net.Sockets.SocketException;
begin
  if E is System.Net.Sockets.SocketException then
  begin
    SE := E as System.Net.Sockets.SocketException;
    WSALastError := SE.ErrorCode;
    WSALastErrorDesc := SE.Message;
  end
end;

function WSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer;
begin
  NullErr;
  with WSData do
  begin
    wVersion := wVersionRequired;
    wHighVersion := $202;
    szDescription := 'Synsock - Synapse Platform Independent Socket Layer';
    szSystemStatus := 'Running on .NET';
    iMaxSockets := 32768;
    iMaxUdpDg := 8192;
  end;
  Result := 0;
end;

function WSACleanup: Integer;
begin
  NullErr;
  Result := 0;
end;

function WSAGetLastError: Integer;
begin
  Result := WSALastError;
end;

function WSAGetLastErrorDesc: String;
begin
  Result := WSALastErrorDesc;
end;

function GetHostName: string;
begin
  Result := System.Net.DNS.GetHostName;
end;

function Shutdown(s: TSocket; how: Integer): Integer;
begin
  Result := 0;
  NullErr;
  try
    s.ShutDown(SocketShutdown(how));
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

function SetSockOpt(s: TSocket; level, optname: Integer; optval: Tmemory;
  optlen: Integer): Integer;
begin
  Result := 0;
  NullErr;
  try
    s.SetSocketOption(SocketOptionLevel(level), SocketOptionName(optname), optval);
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

function SetSockOptObj(s: TSocket; level, optname: Integer; optval: TObject): Integer;
begin
  Result := 0;
  NullErr;
  try
    s.SetSocketOption(SocketOptionLevel(level), SocketOptionName(optname), optval);
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

function GetSockOpt(s: TSocket; level, optname: Integer; optval: Tmemory;
  var optlen: Integer): Integer;
begin
  Result := 0;
  NullErr;
  try
    s.GetSocketOption(SocketOptionLevel(level), SocketOptionName(optname), optval);
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

function SendTo(s: TSocket; Buf: TMemory; len, flags: Integer; addrto: TVarSin): Integer;
//function SendTo(s: TSocket; const Buf; len, flags: Integer; addrto: TVarSin): Integer;
begin
  NullErr;
  try
    result := s.SendTo(Buf, len, SocketFlags(flags), addrto);
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

function Send(s: TSocket; Buf: TMemory; len, flags: Integer): Integer;
//function Send(s: TSocket; const Buf; len, flags: Integer): Integer;
begin
  NullErr;
  try
    result := s.Send(Buf, len, SocketFlags(flags));
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

function Recv(s: TSocket; Buf: TMemory; len, flags: Integer): Integer;
//function  Recv(s: TSocket; var Buf; len, flags: Integer): Integer;
begin
  NullErr;
  try
    result := s.Receive(Buf, len, SocketFlags(flags));
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

//function  RecvFrom(s: TSocket; var Buf; len, flags: Integer; from: PSockAddr;
//  var fromlen: Integer): Integer;
function RecvFrom(s: TSocket; Buf: TMemory; len, flags: Integer; var from: TVarSin): Integer;
//function  RecvFrom(s: TSocket; var Buf; len, flags: Integer; from: TVarSin): Integer;
var
  EP: EndPoint;
begin
  NullErr;
  try
    EP := from;
    result := s.ReceiveFrom(Buf, len, SocketFlags(flags), EndPoint(EP));
    from := EP as IPEndPoint;
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

function  ntohs(netshort: u_short): u_short;
begin
  Result := IPAddress.NetworkToHostOrder(NetShort);
end;

function  ntohl(netlong: u_long): u_long;
begin
  Result := IPAddress.NetworkToHostOrder(NetLong);
end;

function  Listen(s: TSocket; backlog: Integer): Integer;
begin
  Result := 0;
  NullErr;
  try
    s.Listen(backlog);
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

function  IoctlSocket(s: TSocket; cmd: DWORD; var arg: integer): Integer;
var
  inv, outv: TMemory;
begin
  Result := 0;
  NullErr;
  try
    if cmd = DWORD(FIONBIO) then
      s.Blocking := arg = 0
    else
    begin
      inv := BitConverter.GetBytes(arg);
      outv := BitConverter.GetBytes(integer(0));
      s.IOControl(cmd, inv, outv);
      arg := BitConverter.ToInt32(outv, 0);
    end;
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

function  htons(hostshort: u_short): u_short;
begin
  Result := IPAddress.HostToNetworkOrder(Hostshort);
end;

function  htonl(hostlong: u_long): u_long;
begin
  Result := IPAddress.HostToNetworkOrder(HostLong);
end;

//function  GetSockName(s: TSocket; name: PSockAddr; var namelen: Integer): Integer;
function  GetSockName(s: TSocket; var name: TVarSin): Integer;
begin
  Result := 0;
  NullErr;
  try
    Name := s.localEndPoint as IPEndpoint;
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

//function  GetPeerName(s: TSocket; name: PSockAddr; var namelen: Integer): Integer;
function  GetPeerName(s: TSocket; var name: TVarSin): Integer;
begin
  Result := 0;
  NullErr;
  try
    Name := s.RemoteEndPoint as IPEndpoint;
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

//function  Connect(s: TSocket; name: PSockAddr; namelen: Integer): Integer;
function Connect(s: TSocket; const name: TVarSin): Integer;
begin
  Result := 0;
  NullErr;
  try
    s.Connect(name);
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

function CloseSocket(s: TSocket): Integer;
begin
  Result := 0;
  NullErr;
  try
    s.Close;
  except
    on e: System.Net.Sockets.SocketException do
    begin
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

//function  Bind(s: TSocket; addr: PSockAddr; namelen: Integer): Integer;
function Bind(s: TSocket; const addr: TVarSin): Integer;
begin
  Result := 0;
  NullErr;
  try
    s.Bind(addr);
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := integer(SOCKET_ERROR);
    end;
  end;
end;

//function  Accept(s: TSocket; addr: PSockAddr; var addrlen: Integer): TSocket;
function Accept(s: TSocket; var addr: TVarSin): TSocket;
begin
  NullErr;
  try
    result := s.Accept();
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := nil;
    end;
  end;
end;

function Socket(af, Struc, Protocol: Integer): TSocket;
begin
  NullErr;
  try
    result := TSocket.Create(AddressFamily(af), SocketType(Struc), ProtocolType(Protocol));
  except
    on e: System.Net.Sockets.SocketException do
    begin
      GetErrCode(e);
      Result := nil;
    end;
  end;
end;

{=============================================================================}
function GetPortService(value: string): integer;
var
  n: integer;
begin
  Result := 0;
  value := Lowercase(value);
  for n := 0 to High(Services) do
    if services[n, 0] = value then
    begin
      Result := strtointdef(services[n, 1], 0);
      break;
    end;
  if Result = 0 then
    Result := StrToIntDef(value, 0);
end;

{=============================================================================}
function IsNewApi(Family: TAddrFamily): Boolean;
begin
  Result := true;
end;

function SetVarSin(var Sin: TVarSin; IP, Port: string; Family: TAddrFamily; SockProtocol, SockType: integer; PreferIP4: Boolean): integer;
var
  IPs: array of IPAddress;
  n: integer;
  ip4, ip6: string;
  sip: string;
begin
  sip := '';
  ip4 := '';
  ip6 := '';
  IPs := Dns.Resolve(IP).AddressList;
  for n :=low(IPs) to high(IPs) do begin
    if (ip4 = '') and (IPs[n].AddressFamily = AF_INET) then
      ip4 := IPs[n].toString;
    if (ip6 = '') and (IPs[n].AddressFamily = AF_INET6) then
      ip6 := IPs[n].toString;
    if (ip4 <> '') and (ip6 <> '') then
      break;
  end;
  case Family of
    AF_UNSPEC:
      begin
        if (ip4 <> '') and (ip6 <> '') then
        begin
          if PreferIP4 then
            sip := ip4
          else
            Sip := ip6;
          end
        else
        begin
          sip := ip4;
          if (ip6 <> '') then
            sip := ip6;
        end;
      end;
    AF_INET:
      sip := ip4;
    AF_INET6:
      sip := ip6;
  end;
  sin := TVarSin.Create(IPAddress.Parse(sip), GetPortService(Port));
end;

function GetSinIP(Sin: TVarSin): string;
begin
  Result := Sin.Address.ToString;
end;

function GetSinPort(Sin: TVarSin): Integer;
begin
  Result := Sin.Port;
end;

procedure ResolveNameToIP(Name: string; Family: TAddrFamily; SockProtocol, SockType: integer; const IPList: TStrings);
var
  IPs :array of IPAddress;
  n: integer;
begin
  IPList.Clear;
  IPs := Dns.Resolve(Name).AddressList;
  for n := low(IPs) to high(IPs) do
  begin
    if not(((Family = AF_INET6) and (IPs[n].AddressFamily = AF_INET))
      or ((Family = AF_INET) and (IPs[n].AddressFamily = AF_INET6))) then
    begin
      IPList.Add(IPs[n].toString);
    end;
  end;
end;

function ResolvePort(Port: string; Family: TAddrFamily; SockProtocol, SockType: integer): Word;
var
  n: integer;
begin
  Result := StrToIntDef(port, 0);
  if Result = 0 then
  begin
    port := Lowercase(port);
    for n := 0 to High(Services) do
      if services[n, 0] = port then
      begin
        Result := strtointdef(services[n, 1], 0);
        break;
      end;
  end;
end;

function ResolveIPToName(IP: string; Family: TAddrFamily; SockProtocol, SockType: integer): string;
begin
  Result := Dns.GetHostByAddress(IP).HostName;
end;


{=============================================================================}
function InitSocketInterface(stack: string): Boolean;
begin
  Result := True;
end;

function DestroySocketInterface: Boolean;
begin
  NullErr;
  Result := True;
end;

initialization
begin
  SynSockCS := SyncObjs.TCriticalSection.Create;
//  SET_IN6_IF_ADDR_ANY (@in6addr_any);
//  SET_LOOPBACK_ADDR6  (@in6addr_loopback);
end;

finalization
begin
  NullErr;
  SynSockCS.Free;
end;

{$ENDIF}
