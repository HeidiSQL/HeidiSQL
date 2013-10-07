/// low level access to network Sockets for the Win32 platform
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.6
unit SynWinSock;

{
    This file is part of Synopse SQLite3 database framework.

    Synopse SQLite3 database framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse SQLite3 database framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****



     Low level access to network Sockets
    *************************************
    
   The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).
   Portions created by Lukas Gebauer are Copyright (c)2003.
   All Rights Reserved.

  Contributor(s):
   - Arnaud Bouchez, Jan 2009, for SynCrtSock: see http://synopse.info
     Delphi 2009/2010 compatibility (Jan 2010): the WinSock library
       expects Ansi encoded parameters
}

{.$DEFINE WINSOCK1}
{If you activate this compiler directive, then socket interface level 1.1 is
used instead default level 2.2. Level 2.2 is not available on old W95, however
you can install an update from microsoft}

{.$DEFINE FORCEOLDAPI}
{If you activate this compiler directive, then is allways used old socket API
for name resolution. If you leave this directive inactive, then the new API
is used, when running system allows it. For IPv6 support you must have the new API! }

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  Classes,
  Windows;

function InitSocketInterface(const stack: TFileName=''): Boolean;
function DestroySocketInterface: Boolean;

const
{$IFDEF WINSOCK1}
  WinsockLevel = $0101;
{$ELSE}
  WinsockLevel = $0202;
{$ENDIF}

type
  u_char = AnsiChar;
  u_short = Word;
  u_int = Integer;
  u_long = Longint;
  pu_long = ^u_long;
  pu_short = ^u_short;
  TSocket = u_int;
  TAddrFamily = integer;

  TMemory = pointer;

const
  {$IFDEF WINSOCK1}
    DLLStackName: PChar = 'wsock32.dll';
  {$ELSE}
    DLLStackName: PChar = 'ws2_32.dll';
  {$ENDIF}
  DLLwship6: PChar = 'wship6.dll';

  cLocalhost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  cBroadcast = '255.255.255.255';
  c6Localhost = '::1';
  c6AnyHost = '::0';
  c6Broadcast = 'ffff::1';
  cAnyPort = '0';
  

const
  FD_SETSIZE     =   64;
type
  PFDSet = ^TFDSet;
  TFDSet = packed record
    fd_count: u_int;
    fd_array: array[0..FD_SETSIZE-1] of TSocket;
  end;

const
  FIONREAD     = $4004667f;
  FIONBIO      = $8004667e;
  FIOASYNC     = $8004667d;

type
  PTimeVal = ^TTimeVal;
  TTimeVal = packed record
    tv_sec: Longint;
    tv_usec: Longint;
  end;

const
  IPPROTO_IP     =   0;		{ Dummy					}
  IPPROTO_ICMP   =   1;		{ Internet Control Message Protocol }
  IPPROTO_IGMP   =   2;		{ Internet Group Management Protocol}
  IPPROTO_TCP    =   6;		{ TCP           			}
  IPPROTO_UDP    =   17;	{ User Datagram Protocol		}
  IPPROTO_IPV6   =   41;
  IPPROTO_ICMPV6 =   58;

  IPPROTO_RAW    =   255;
  IPPROTO_MAX    =   256;

type

  PInAddr = ^TInAddr;
  TInAddr = packed record
    case integer of
      0: (S_bytes: packed array [0..3] of byte);
      1: (S_addr: u_long);
  end;

  PSockAddrIn = ^TSockAddrIn;
  TSockAddrIn = packed record
    case Integer of
      0: (sin_family: u_short;
          sin_port: u_short;
          sin_addr: TInAddr;
          sin_zero: array[0..7] of AnsiChar);
      1: (sa_family: u_short;
          sa_data: array[0..13] of AnsiChar)
  end;

  TIP_mreq =  record
    imr_multiaddr: TInAddr;     { IP multicast address of group }
    imr_interface: TInAddr;     { local IP address of interface }
  end;

  PInAddr6 = ^TInAddr6;
  TInAddr6 = packed record
    case integer of
      0: (S6_addr: packed array [0..15] of byte);
      1: (u6_addr8: packed array [0..15] of byte);
      2: (u6_addr16: packed array [0..7] of word);
      3: (u6_addr32: packed array [0..3] of integer);
  end;

  PSockAddrIn6 = ^TSockAddrIn6;
  TSockAddrIn6 = packed record
		sin6_family:   u_short;     // AF_INET6
		sin6_port:     u_short;     // Transport level port number
		sin6_flowinfo: u_long;	    // IPv6 flow information
		sin6_addr:     TInAddr6;    // IPv6 address
		sin6_scope_id: u_long;      // Scope Id: IF number for link-local
                                //           SITE id for site-local
  end;

  TIPv6_mreq = record
    ipv6mr_multiaddr: TInAddr6; // IPv6 multicast address.
    ipv6mr_interface: integer;   // Interface index.
    padding: integer;
  end;

  PHostEnt = ^THostEnt;
  THostEnt = packed record
    h_name: PAnsiChar;
    h_aliases: ^PAnsiChar;
    h_addrtype: Smallint;
    h_length: Smallint;
    case integer of
     0: (h_addr_list: ^PAnsiChar);
     1: (h_addr: ^PInAddr);
  end;

  PNetEnt = ^TNetEnt;
  TNetEnt = packed record
    n_name: PAnsiChar;
    n_aliases: ^PAnsiChar;
    n_addrtype: Smallint;
    n_net: u_long;
  end;

  PServEnt = ^TServEnt;
  TServEnt = packed record
    s_name: PAnsiChar;
    s_aliases: ^PAnsiChar;
    s_port: Smallint;
    s_proto: PAnsiChar;
  end;

  PProtoEnt = ^TProtoEnt;
  TProtoEnt = packed record
    p_name: PAnsiChar;
    p_aliases: ^PAnsiChar;
    p_proto: Smallint;
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
  {$IFDEF WINSOCK1}
    IP_OPTIONS          = 1;
    IP_MULTICAST_IF     = 2;           { set/get IP multicast interface   }
    IP_MULTICAST_TTL    = 3;           { set/get IP multicast timetolive  }
    IP_MULTICAST_LOOP   = 4;           { set/get IP multicast loopback    }
    IP_ADD_MEMBERSHIP   = 5;           { add  an IP group membership      }
    IP_DROP_MEMBERSHIP  = 6;           { drop an IP group membership      }
    IP_TTL              = 7;           { set/get IP Time To Live          }
    IP_TOS              = 8;           { set/get IP Type Of Service       }
    IP_DONTFRAGMENT     = 9;           { set/get IP Don't Fragment flag   }
  {$ELSE}
    IP_OPTIONS          = 1;
    IP_HDRINCL          = 2;
    IP_TOS              = 3;           { set/get IP Type Of Service       }
    IP_TTL              = 4;           { set/get IP Time To Live          }
    IP_MULTICAST_IF     = 9;           { set/get IP multicast interface   }
    IP_MULTICAST_TTL    = 10;           { set/get IP multicast timetolive  }
    IP_MULTICAST_LOOP   = 11;           { set/get IP multicast loopback    }
    IP_ADD_MEMBERSHIP   = 12;           { add  an IP group membership      }
    IP_DROP_MEMBERSHIP  = 13;           { drop an IP group membership      }
    IP_DONTFRAGMENT     = 14;           { set/get IP Don't Fragment flag   }
  {$ENDIF}

  IP_DEFAULT_MULTICAST_TTL   = 1;    { normally limit m'casts to 1 hop  }
  IP_DEFAULT_MULTICAST_LOOP  = 1;    { normally hear sends if a member  }
  IP_MAX_MEMBERSHIPS         = 20;   { per socket; must fit in one mbuf }

  SOL_SOCKET      = $ffff;          {options for socket level }
{ Option flags per-socket. }
  SO_DEBUG        = $0001;          { turn on debugging info recording }
  SO_ACCEPTCONN   = $0002;          { socket has had listen() }
  SO_REUSEADDR    = $0004;          { allow local address reuse }
  SO_KEEPALIVE    = $0008;          { keep connections alive }
  SO_DONTROUTE    = $0010;          { just use interface addresses }
  SO_BROADCAST    = $0020;          { permit sending of broadcast msgs }
  SO_USELOOPBACK  = $0040;          { bypass hardware when possible }
  SO_LINGER       = $0080;          { linger on close if data present }
  SO_OOBINLINE    = $0100;          { leave received OOB data in line }
  SO_DONTLINGER  =   $ff7f;
{ Additional options. }
  SO_SNDBUF       = $1001;          { send buffer size }
  SO_RCVBUF       = $1002;          { receive buffer size }
  SO_SNDLOWAT     = $1003;          { send low-water mark }
  SO_RCVLOWAT     = $1004;          { receive low-water mark }
  SO_SNDTIMEO     = $1005;          { send timeout }
  SO_RCVTIMEO     = $1006;          { receive timeout }
  SO_ERROR        = $1007;          { get error status and clear }
  SO_TYPE         = $1008;          { get socket type }
{ WinSock 2 extension -- new options }
  SO_GROUP_ID       = $2001; { ID of a socket group}
  SO_GROUP_PRIORITY = $2002; { the relative priority within a group}
  SO_MAX_MSG_SIZE   = $2003; { maximum message size }
  SO_PROTOCOL_INFOA = $2004; { WSAPROTOCOL_INFOA structure }
  SO_PROTOCOL_INFOW = $2005; { WSAPROTOCOL_INFOW structure }
  SO_PROTOCOL_INFO  = SO_PROTOCOL_INFOA;
  PVD_CONFIG        = $3001; {configuration info for service provider }
{ Option for opening sockets for synchronous access. }
  SO_OPENTYPE     = $7008;
  SO_SYNCHRONOUS_ALERT    = $10;
  SO_SYNCHRONOUS_NONALERT = $20;
{ Other NT-specific options. }
  SO_MAXDG        = $7009;
  SO_MAXPATHDG    = $700A;
  SO_UPDATE_ACCEPT_CONTEXT     = $700B;
  SO_CONNECT_TIME = $700C;

  SOMAXCONN       = $7fffffff;

  IPV6_UNICAST_HOPS      = 8;  // ???
  IPV6_MULTICAST_IF      = 9;  // set/get IP multicast i/f
  IPV6_MULTICAST_HOPS    = 10; // set/get IP multicast ttl
  IPV6_MULTICAST_LOOP    = 11; // set/get IP multicast loopback
  IPV6_JOIN_GROUP        = 12; // add an IP group membership
  IPV6_LEAVE_GROUP       = 13; // drop an IP group membership

  MSG_NOSIGNAL  = 0;

  // getnameinfo constants
  NI_MAXHOST	   = 1025;
  NI_MAXSERV	   = 32;
  NI_NOFQDN 	   = $1;
  NI_NUMERICHOST = $2;
  NI_NAMEREQD	   = $4;
  NI_NUMERICSERV = $8;
  NI_DGRAM       = $10;


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
  AF_INET6        = 23;              { Internetwork Version 6 }
  AF_MAX          = 24;

{ Protocol families, same as address families for now. }
  PF_UNSPEC       = AF_UNSPEC;
  PF_INET         = AF_INET;
  PF_INET6        = AF_INET6;
  PF_MAX          = AF_MAX;

type
  { Structure used by kernel to store most addresses. }
  PSockAddr = ^TSockAddr;
  TSockAddr = TSockAddrIn;

  { Structure used by kernel to pass protocol information in raw sockets. }
  PSockProto = ^TSockProto;
  TSockProto = packed record
    sp_family: u_short;
    sp_protocol: u_short;
  end;

type
  PAddrInfo = ^TAddrInfo;
  TAddrInfo = record
                ai_flags: integer;    // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST.
                ai_family: integer;   // PF_xxx.
                ai_socktype: integer; // SOCK_xxx.
                ai_protocol: integer; // 0 or IPPROTO_xxx for IPv4 and IPv6.
                ai_addrlen: u_int;    // Length of ai_addr.
                ai_canonname: PAnsiChar;  // Canonical name for nodename.
                ai_addr: PSockAddr;   // Binary address.
                ai_next: PAddrInfo;     // Next structure in linked list.
              end;

const
  // Flags used in "hints" argument to getaddrinfo().
  AI_PASSIVE     = $1;  // Socket address will be used in bind() call.
  AI_CANONNAME   = $2;  // Return canonical name in first ai_canonname.
  AI_NUMERICHOST = $4;  // Nodename must be a numeric address AnsiString.

type
{ Structure used for manipulating linger option. }
  PLinger = ^TLinger;
  TLinger = packed record
    l_onoff: u_short;
    l_linger: u_short;
  end;

const

  MSG_OOB       = $01;                  // Process out-of-band data.
  MSG_PEEK      = $02;                  // Peek at incoming messages.

const

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

  EAI_ADDRFAMILY  = 1;   // Address family for nodename not supported.
  EAI_AGAIN       = 2;   // Temporary failure in name resolution.
  EAI_BADFLAGS    = 3;   // Invalid value for ai_flags.
  EAI_FAIL        = 4;   // Non-recoverable failure in name resolution.
  EAI_FAMILY      = 5;   // Address family ai_family not supported.
  EAI_MEMORY      = 6;   // Memory allocation failure.
  EAI_NODATA      = 7;   // No address associated with nodename.
  EAI_NONAME      = 8;   // Nodename nor servname provided, or not known.
  EAI_SERVICE     = 9;   // Servname not supported for ai_socktype.
  EAI_SOCKTYPE    = 10;  // Socket type ai_socktype not supported.
  EAI_SYSTEM      = 11;  // System error returned in errno.

const
  WSADESCRIPTION_LEN     =   256;
  WSASYS_STATUS_LEN      =   128;
type
  PWSAData = ^TWSAData;
  TWSAData = packed record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..WSADESCRIPTION_LEN] of AnsiChar;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of AnsiChar;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PAnsiChar;
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


type
  TWSAStartup = function(wVersionRequired: Word; var WSData: TWSAData): Integer;
    stdcall;
  TWSACleanup = function: Integer;
    stdcall;
  TWSAGetLastError = function: Integer;
    stdcall;
  TGetServByName = function(name, proto: PAnsiChar): PServEnt;
    stdcall;
  TGetServByPort = function(port: Integer; proto: PAnsiChar): PServEnt;
    stdcall;
  TGetProtoByName = function(name: PAnsiChar): PProtoEnt;
    stdcall;
  TGetProtoByNumber = function(proto: Integer): PProtoEnt;
    stdcall;
  TGetHostByName = function(name: PAnsiChar): PHostEnt;
    stdcall;
  TGetHostByAddr = function(addr: Pointer; len, Struc: Integer): PHostEnt;
    stdcall;
  TGetHostName = function(name: PAnsiChar; len: Integer): Integer;
    stdcall;
  TShutdown = function(s: TSocket; how: Integer): Integer;
    stdcall;
  TSetSockOpt = function(s: TSocket; level, optname: Integer; optval: PAnsiChar;
    optlen: Integer): Integer;
    stdcall;
  TGetSockOpt = function(s: TSocket; level, optname: Integer; optval: PAnsiChar;
    var optlen: Integer): Integer;
    stdcall;
  TSendTo = function(s: TSocket; Buf: pointer; len, flags: Integer; addrto: PSockAddr;
    tolen: Integer): Integer;
    stdcall;
  TSend = function(s: TSocket; Buf: pointer; len, flags: Integer): Integer;
    stdcall;
  TRecv = function(s: TSocket; Buf: pointer; len, flags: Integer): Integer;
    stdcall;
  TRecvFrom = function(s: TSocket; Buf: pointer; len, flags: Integer; from: PSockAddr;
    fromlen: PInteger): Integer;
    stdcall;
  Tntohs = function(netshort: u_short): u_short;
    stdcall;
  Tntohl = function(netlong: u_long): u_long;
    stdcall;
  TListen = function(s: TSocket; backlog: Integer): Integer;
    stdcall;
  TIoctlSocket = function(s: TSocket; cmd: DWORD; var arg: Integer): Integer;
    stdcall;
  TInet_ntoa = function(inaddr: TInAddr): PAnsiChar;
    stdcall;
  TInet_addr = function(cp: PAnsiChar): u_long;
    stdcall;
  Thtons = function(hostshort: u_short): u_short;
    stdcall;
  Thtonl = function(hostlong: u_long): u_long;
    stdcall;
  TGetSockName = function(s: TSocket; name: PSockAddr; var namelen: Integer): Integer;
    stdcall;
  TGetPeerName = function(s: TSocket; name: PSockAddr; var namelen: Integer): Integer;
    stdcall;
  TConnect = function(s: TSocket; name: PSockAddr; namelen: Integer): Integer;
    stdcall;
  TCloseSocket = function(s: TSocket): Integer;
    stdcall;
  TBind = function(s: TSocket; addr: PSockAddr; namelen: Integer): Integer;
    stdcall;
  TAccept = function(s: TSocket; addr: PSockAddr; var addrlen: Integer): TSocket;
    stdcall;
  TTSocket = function(af, Struc, Protocol: Integer): TSocket;
    stdcall;
  TSelect = function(nfds: Integer; readfds, writefds, exceptfds: PFDSet;
    timeout: PTimeVal): Longint;
    stdcall;

  TGetAddrInfo = function(NodeName: PAnsiChar; ServName: PAnsiChar; Hints: PAddrInfo;
    var Addrinfo: PAddrInfo): integer;
    stdcall;
  TFreeAddrInfo = procedure(ai: PAddrInfo);
    stdcall;
  TGetNameInfo = function( addr: PSockAddr; namelen: Integer; host: PAnsiChar;
    hostlen: DWORD; serv: PAnsiChar; servlen: DWORD; flags: integer): integer;
    stdcall;

  T__WSAFDIsSet = function (s: TSocket; var FDSet: TFDSet): Bool;
    stdcall;

  TWSAIoctl = function (s: TSocket; dwIoControlCode: DWORD; lpvInBuffer: Pointer;
    cbInBuffer: DWORD; lpvOutBuffer: Pointer; cbOutBuffer: DWORD;
    lpcbBytesReturned: PDWORD; lpOverlapped: Pointer;
    lpCompletionRoutine: pointer): u_int;
    stdcall;

var
  WSAStartup: TWSAStartup = nil;
  WSACleanup: TWSACleanup = nil;
  WSAGetLastError: TWSAGetLastError = nil;
  GetServByName: TGetServByName = nil;
  GetServByPort: TGetServByPort = nil;
  GetProtoByName: TGetProtoByName = nil;
  GetProtoByNumber: TGetProtoByNumber = nil;
  GetHostByName: TGetHostByName = nil;
  GetHostByAddr: TGetHostByAddr = nil;
  ssGetHostName: TGetHostName = nil;
  Shutdown: TShutdown = nil;
  SetSockOpt: TSetSockOpt = nil;
  GetSockOpt: TGetSockOpt = nil;
  SendTo: TSendTo = nil;
  Send: TSend = nil;
  Recv: TRecv = nil;
  RecvFrom: TRecvFrom = nil;
  ntohs: Tntohs = nil;
  ntohl: Tntohl = nil;
  Listen: TListen = nil;
  IoctlSocket: TIoctlSocket = nil;
  Inet_ntoa: TInet_ntoa = nil;
  Inet_addr: TInet_addr = nil;
  htons: Thtons = nil;
  htonl: Thtonl = nil;
  ssGetSockName: TGetSockName = nil;
  ssGetPeerName: TGetPeerName = nil;
  ssConnect: TConnect = nil;
  CloseSocket: TCloseSocket = nil;
  ssBind: TBind = nil;
  ssAccept: TAccept = nil;
  Socket: TTSocket = nil;
  Select: TSelect = nil;

  GetAddrInfo: TGetAddrInfo = nil;
  FreeAddrInfo: TFreeAddrInfo = nil;
  GetNameInfo: TGetNameInfo = nil;

  __WSAFDIsSet: T__WSAFDIsSet = nil;

  WSAIoctl: TWSAIoctl = nil;

var
  SynSockCS: TRTLCriticalSection;
  SockEnhancedApi: Boolean;
  SockWship6Api: Boolean;

type
  PVarSin = ^TVarSin;
  TVarSin = packed record
    case integer of
      0: (AddressFamily: u_short);
      1: (
        case sin_family: u_short of
          AF_INET: (sin_port: u_short;
                    sin_addr: TInAddr;
                    sin_zero: array[0..7] of AnsiChar);
          AF_INET6: (sin6_port:     u_short;
                		sin6_flowinfo: u_long;
      	    	      sin6_addr:     TInAddr6;
      		          sin6_scope_id: u_long);
          );
  end;

function SizeOfVarSin(const sin: TVarSin): integer;

function GetSockName(s: TSocket; var name: TVarSin): Integer;
function GetPeerName(s: TSocket; var name: TVarSin): Integer;
function GetHostName: AnsiString;
function Bind(s: TSocket; const addr: TVarSin): Integer;
function Connect(s: TSocket; const name: TVarSin): Integer;
function Accept(s: TSocket; var addr: TVarSin): TSocket;

function IsNewApi(Family: integer): Boolean;
function SetVarSin(var Sin: TVarSin; const IP, Port: AnsiString; Family, SockProtocol, SockType: integer; PreferIP4: Boolean): integer;
function GetSinIP(const Sin: TVarSin): AnsiString;
function GetSinPort(const Sin: TVarSin): Integer;
procedure ResolveNameToIP(const Name: AnsiString; Family, SockProtocol, SockType: integer; IPList: TStrings);
function ResolveIPToName(const IP: AnsiString; Family, SockProtocol, SockType: integer): AnsiString;
function ResolvePort(const Port: AnsiString; Family, SockProtocol, SockType: integer): Word;


implementation

var
  SynSockCount: Integer = 0;
  LibHandle: THandle = 0;
  Libwship6Handle: THandle = 0;

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
  Result := (CompareMem(a, b, sizeof(TInAddr6)));
end;

procedure SET_IN6_IF_ADDR_ANY(const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
end;

procedure SET_LOOPBACK_ADDR6(const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
  a^.u6_addr8[15] := 1;
end;



procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
var I: Integer;
begin
  I := 0;
  while I < FDSet.fd_count do begin
    if FDSet.fd_array[I] = Socket then begin
      while I < FDSet.fd_count - 1 do begin
        FDSet.fd_array[I] := FDSet.fd_array[I + 1];
        Inc(I);
      end;
      Dec(FDSet.fd_count);
      Break;
    end;
    Inc(I);
  end;
end;

function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
begin
  Result := __WSAFDIsSet(Socket, FDSet);
end;

procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
begin
  if not FD_ISSET(Socket, FDSet) then
  if FDSet.fd_count < FD_SETSIZE then begin
    FDSet.fd_array[FDSet.fd_count] := Socket;
    Inc(FDSet.fd_count);
  end;
end;

procedure FD_ZERO(var FDSet: TFDSet);
begin
  FDSet.fd_count := 0;
end;

function SizeOfVarSin(const sin: TVarSin): integer;
begin
  case sin.sin_family of
    AF_INET:  Result := SizeOf(TSockAddrIn);
    AF_INET6: Result := SizeOf(TSockAddrIn6);
  else Result := 0;
  end;
end;

function GetSockName(s: TSocket; var name: TVarSin): Integer;
var len: integer;
begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  Result := ssGetSockName(s, @name, Len);
end;

function GetPeerName(s: TSocket; var name: TVarSin): Integer;
var len: integer;
begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  Result := ssGetPeerName(s, @name, Len);
end;

function GetHostName: AnsiString;
var s: array[0..255] of AnsiChar;
begin
  ssGetHostName(@s, 255);
  Result := s;
end;

function Accept(s: TSocket; var addr: TVarSin): TSocket;
var x: integer;
begin
  x := SizeOf(addr);
  Result := ssAccept(s, @addr, x);
end;

function Bind(s: TSocket; const addr: TVarSin): Integer;
begin
  Result := ssBind(s, @addr, SizeOfVarSin(addr));
end;

function Connect(s: TSocket; const name: TVarSin): Integer;
begin
  Result := ssConnect(s, @name, SizeOfVarSin(name));
end;


function IsNewApi(Family: integer): Boolean;
begin
  Result := SockEnhancedApi;
  if not Result then
    Result := (Family = AF_INET6) and SockWship6Api;
end;

function SetVarSin(var Sin: TVarSin; const IP, Port: AnsiString; Family, SockProtocol, SockType: integer;
   PreferIP4: Boolean): integer;
type
  pu_long = ^u_long;
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
  HostEnt: PHostEnt;
  r: integer;
  Hints1, Hints2: TAddrInfo;
  Sin1, Sin2: TVarSin;
  TwoPass: boolean;

  function GetAddr(const IP, port: AnsiString; var Hints: TAddrInfo; var Sin: TVarSin): integer;
  var Addr: PAddrInfo;
  begin
    Addr := nil;
    try
      FillChar(Sin, Sizeof(Sin), 0);
      if Hints.ai_socktype = SOCK_RAW then begin
        Hints.ai_socktype := 0;
        Hints.ai_protocol := 0;
        Result := GetAddrInfo(pointer(IP), nil, @Hints, Addr);
      end
      else begin
        if (IP = cAnyHost) or (IP = c6AnyHost) then begin
          Hints.ai_flags := AI_PASSIVE;
          Result := GetAddrInfo(nil, pointer(Port), @Hints, Addr);
        end
        else
          if (IP = cLocalhost) or (IP = c6Localhost) then
            Result := GetAddrInfo(nil, pointer(Port), @Hints, Addr) else
            Result := GetAddrInfo(pointer(IP), pointer(Port), @Hints, Addr);
      end;
      if Result = 0 then
        if (Addr <> nil) then
          Move(Addr^.ai_addr^, Sin, Addr^.ai_addrlen);
    finally
      if Assigned(Addr) then
        FreeAddrInfo(Addr);
    end;
  end;

begin
  Result := 0;
  FillChar(Sin, Sizeof(Sin), 0);
  if not IsNewApi(family) then
  begin
    EnterCriticalSection(SynSockCS);
    try
      Sin.sin_family := AF_INET;
      ProtoEnt := GetProtoByNumber(SockProtocol);
      ServEnt := nil;
      if ProtoEnt <> nil then
        ServEnt := GetServByName(pointer(Port), ProtoEnt^.p_name);
      if ServEnt = nil then
        Sin.sin_port := htons(StrToIntDef(string(Port), 0)) else
        Sin.sin_port := ServEnt^.s_port;
      if IP = cBroadcast then
        Sin.sin_addr.s_addr := u_long(INADDR_BROADCAST)
      else
      begin
        Sin.sin_addr.s_addr := inet_addr(pointer(IP));
        if Sin.sin_addr.s_addr = u_long(INADDR_NONE) then begin
          HostEnt := GetHostByName(pointer(IP));
          Result := WSAGetLastError;
          if HostEnt <> nil then
            Sin.sin_addr.S_addr := u_long(Pu_long(HostEnt^.h_addr_list^)^);
        end;
      end;
    finally
      LeaveCriticalSection(SynSockCS);
    end;
  end
  else begin
    FillChar(Hints1, Sizeof(Hints1), 0);
    FillChar(Hints2, Sizeof(Hints2), 0);
    TwoPass := False;
    if Family = AF_UNSPEC then begin
      if PreferIP4 then begin
        Hints1.ai_family := AF_INET;
        Hints2.ai_family := AF_INET6;
        TwoPass := True;
      end
      else begin
        Hints2.ai_family := AF_INET;
        Hints1.ai_family := AF_INET6;
        TwoPass := True;
      end;
    end
    else
      Hints1.ai_family := Family;
    Hints1.ai_socktype := SockType;
    Hints2.ai_socktype := Hints1.ai_socktype;
    Hints1.ai_protocol := SockProtocol;
    Hints2.ai_protocol := Hints1.ai_protocol;
    r := GetAddr(IP, Port, Hints1, Sin1);
    Result := r;
    sin := sin1;
    if r <> 0 then
      if TwoPass then begin
        r := GetAddr(IP, Port, Hints2, Sin2);
        Result := r;
        if r = 0 then
          sin := sin2;
      end;
  end;
end;

function GetSinIP(const Sin: TVarSin): AnsiString;
var p: PAnsiChar;
    host: array[0..NI_MAXHOST] of AnsiChar;
    serv: array[0..NI_MAXSERV] of AnsiChar;
    hostlen, servlen: integer;
    r: integer;
begin
  Result := '';
  if not IsNewApi(Sin.AddressFamily) then begin
    p := inet_ntoa(Sin.sin_addr);
    if p <> nil then
      Result := p;
  end
  else
  begin
    hostlen := NI_MAXHOST;
    servlen := NI_MAXSERV;
    r := getnameinfo(@sin, SizeOfVarSin(sin), host, hostlen,
      serv, servlen, NI_NUMERICHOST + NI_NUMERICSERV);
    if r = 0 then
      Result := host;
  end;
end;

function GetSinPort(const Sin: TVarSin): Integer;
begin
  if (Sin.sin_family = AF_INET6) then
    Result := ntohs(Sin.sin6_port) else
    Result := ntohs(Sin.sin_port);
end;

procedure ResolveNameToIP(const Name: AnsiString; Family, SockProtocol, SockType: integer;
  IPList: TStrings);
type
  TaPInAddr = array[0..250] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  Hints: TAddrInfo;
  Addr: PAddrInfo;
  AddrNext: PAddrInfo;
  r: integer;
  host: array[0..NI_MAXHOST] of AnsiChar;
  serv: array[0..NI_MAXSERV] of AnsiChar;
  hostlen, servlen: integer;
  RemoteHost: PHostEnt;
  IP: u_long;
  PAdrPtr: PaPInAddr;
  i: Integer;
  InAddr: TInAddr;
begin
  IPList.Clear;
  if not IsNewApi(Family) then begin
    IP := inet_addr(pointer(Name));
    if IP = u_long(INADDR_NONE) then begin
      EnterCriticalSection(SynSockCS);
      try
        RemoteHost := GetHostByName(pointer(Name));
        if RemoteHost <> nil then begin
          PAdrPtr := PAPInAddr(RemoteHost^.h_addr_list);
          i := 0;
          while PAdrPtr^[i] <> nil do begin
            InAddr := PAdrPtr^[i]^;
            IPList.Add(Format('%d.%d.%d.%d', [InAddr.S_bytes[0], InAddr.S_bytes[1],
              InAddr.S_bytes[2], InAddr.S_bytes[3]]));
            Inc(i);
          end;
        end;
      finally
        LeaveCriticalSection(SynSockCS);
      end;
    end
    else
      IPList.Add(string(Name));
  end
  else begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_socktype := SockType;
      Hints.ai_protocol := SockProtocol;
      r := GetAddrInfo(pointer(Name), nil, @Hints, Addr);
      if r = 0 then begin
        AddrNext := Addr;
        while not(AddrNext = nil) do begin
          if not(((Family = AF_INET6) and (AddrNext^.ai_family = AF_INET))
            or ((Family = AF_INET) and (AddrNext^.ai_family = AF_INET6))) then begin
            hostlen := NI_MAXHOST;
            servlen := NI_MAXSERV;
            r := getnameinfo(AddrNext^.ai_addr, AddrNext^.ai_addrlen,
              host, hostlen, serv, servlen,
              NI_NUMERICHOST + NI_NUMERICSERV);
            if r = 0 then
              IPList.Add(string(host));
          end;
          AddrNext := AddrNext^.ai_next;
        end;
      end;
    finally
      if Assigned(Addr) then
        FreeAddrInfo(Addr);
    end;
  end;
  if IPList.Count = 0 then
    IPList.Add(cAnyHost);
end;

function ResolvePort(const Port: AnsiString; Family, SockProtocol, SockType: integer): Word;
var ProtoEnt: PProtoEnt;
    ServEnt: PServEnt;
    Hints: TAddrInfo;
    Addr: PAddrInfo;
    r: integer;
begin
  Result := 0;
  if not IsNewApi(Family) then begin
    EnterCriticalSection(SynSockCS);
    try
      ProtoEnt := GetProtoByNumber(SockProtocol);
      ServEnt := nil;
      if ProtoEnt <> nil then
        ServEnt := GetServByName(pointer(Port), ProtoEnt^.p_name);
      if ServEnt = nil then
        Result := StrToIntDef(string(Port), 0) else
        Result := htons(ServEnt^.s_port);
    finally
      LeaveCriticalSection(SynSockCS);
    end;
  end
  else
  begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_socktype := SockType;
      Hints.ai_protocol := Sockprotocol;
      Hints.ai_flags := AI_PASSIVE;
      r := GetAddrInfo(nil, pointer(Port), @Hints, Addr);
      if (r = 0) and Assigned(Addr) then begin
        if Addr^.ai_family = AF_INET then
          Result := htons(Addr^.ai_addr^.sin_port);
        if Addr^.ai_family = AF_INET6 then
          Result := htons(PSockAddrIn6(Addr^.ai_addr)^.sin6_port);
      end;
    finally
      if Assigned(Addr) then
        FreeAddrInfo(Addr);
    end;
  end;
end;

function ResolveIPToName(const IP: AnsiString; Family, SockProtocol, SockType: integer): AnsiString;
var
  Hints: TAddrInfo;
  Addr: PAddrInfo;
  r: integer;
  host: array[0..NI_MAXHOST] of AnsiChar;
  serv: array[0..NI_MAXSERV] of AnsiChar;
  hostlen, servlen: integer;
  RemoteHost: PHostEnt;
  IPn: u_long;
begin
  Result := IP;
  if not IsNewApi(Family) then begin
    IPn := inet_addr(pointer(IP));
    if IPn <> u_long(INADDR_NONE) then begin
      EnterCriticalSection(SynSockCS);
      try
        RemoteHost := GetHostByAddr(@IPn, SizeOf(IPn), AF_INET);
        if RemoteHost <> nil then
          Result := RemoteHost^.h_name;
      finally
        LeaveCriticalSection(SynSockCS);
      end;
    end;
  end
  else begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_socktype := SockType;
      Hints.ai_protocol := SockProtocol;
      r := GetAddrInfo(pointer(IP), nil, @Hints, Addr);
      if (r = 0) and Assigned(Addr) then begin
        hostlen := NI_MAXHOST;
        servlen := NI_MAXSERV;
        r := getnameinfo(Addr^.ai_addr, Addr^.ai_addrlen,
          host, hostlen, serv, servlen, NI_NUMERICSERV);
        if r = 0 then
          Result := host;
      end;
    finally
      if Assigned(Addr) then
        FreeAddrInfo(Addr);
    end;
  end;
end;

function InitSocketInterface(const Stack: TFileName= ''): Boolean;
begin
  Result := False;
  SockEnhancedApi := False;
  EnterCriticalSection(SynSockCS);
  try
    if SynSockCount = 0 then begin
      SockEnhancedApi := False;
      SockWship6Api := False;
      if Stack = '' then
        LibHandle := LoadLibrary(DLLStackName) else
        LibHandle := LoadLibrary(pointer(Stack));
      if LibHandle <> 0 then begin
        WSAIoctl := GetProcAddress(LibHandle, 'WSAIoctl');
        __WSAFDIsSet := GetProcAddress(LibHandle, '__WSAFDIsSet');
        CloseSocket := GetProcAddress(LibHandle, 'closesocket');
        IoctlSocket := GetProcAddress(LibHandle, 'ioctlsocket');
        WSAGetLastError := GetProcAddress(LibHandle, 'WSAGetLastError');
        WSAStartup := GetProcAddress(LibHandle, 'WSAStartup');
        WSACleanup := GetProcAddress(LibHandle, 'WSACleanup');
        ssAccept := GetProcAddress(LibHandle, 'accept');
        ssBind := GetProcAddress(LibHandle, 'bind');
        ssConnect := GetProcAddress(LibHandle, 'connect');
        ssGetPeerName := GetProcAddress(LibHandle, 'getpeername');
        ssGetSockName := GetProcAddress(LibHandle, 'getsockname');
        GetSockOpt := GetProcAddress(LibHandle, 'getsockopt');
        Htonl := GetProcAddress(LibHandle, 'htonl');
        Htons := GetProcAddress(LibHandle, 'htons');
        Inet_Addr := GetProcAddress(LibHandle, 'inet_addr');
        Inet_Ntoa := GetProcAddress(LibHandle, 'inet_ntoa');
        Listen := GetProcAddress(LibHandle, 'listen');
        Ntohl := GetProcAddress(LibHandle, 'ntohl');
        Ntohs := GetProcAddress(LibHandle, 'ntohs');
        Recv := GetProcAddress(LibHandle, 'recv');
        RecvFrom := GetProcAddress(LibHandle, 'recvfrom');
        Select := GetProcAddress(LibHandle, 'select');
        Send := GetProcAddress(LibHandle, 'send');
        SendTo := GetProcAddress(LibHandle, 'sendto');
        SetSockOpt := GetProcAddress(LibHandle, 'setsockopt');
        ShutDown := GetProcAddress(LibHandle, 'shutdown');
        Socket := GetProcAddress(LibHandle, 'socket');
        GetHostByAddr := GetProcAddress(LibHandle, 'gethostbyaddr');
        GetHostByName := GetProcAddress(LibHandle, 'gethostbyname');
        GetProtoByName := GetProcAddress(LibHandle, 'getprotobyname');
        GetProtoByNumber := GetProcAddress(LibHandle, 'getprotobynumber');
        GetServByName := GetProcAddress(LibHandle, 'getservbyname');
        GetServByPort := GetProcAddress(LibHandle, 'getservbyport');
        ssGetHostName := GetProcAddress(LibHandle, 'gethostname');
{$IFNDEF FORCEOLDAPI}
        GetAddrInfo := GetProcAddress(LibHandle, 'getaddrinfo');
        FreeAddrInfo := GetProcAddress(LibHandle, 'freeaddrinfo');
        GetNameInfo := GetProcAddress(LibHandle, 'getnameinfo');
        SockEnhancedApi := Assigned(GetAddrInfo) and Assigned(FreeAddrInfo)
          and Assigned(GetNameInfo);
        if not SockEnhancedApi then begin
          LibWship6Handle := LoadLibrary(DLLWship6);
          if LibWship6Handle <> 0 then begin
            GetAddrInfo := GetProcAddress(LibWship6Handle, 'getaddrinfo');
            FreeAddrInfo := GetProcAddress(LibWship6Handle, 'freeaddrinfo');
            GetNameInfo := GetProcAddress(LibWship6Handle, 'getnameinfo');
            SockWship6Api := Assigned(GetAddrInfo) and Assigned(FreeAddrInfo)
              and Assigned(GetNameInfo);
          end;
        end;
{$ENDIF}Result := True;
      end;
    end
    else Result := True;
    if Result then
      Inc(SynSockCount);
  finally
    LeaveCriticalSection(SynSockCS);
  end;
end;

function DestroySocketInterface: Boolean;
begin
  EnterCriticalSection(SynSockCS);
  try
    Dec(SynSockCount);
    if SynSockCount < 0 then
      SynSockCount := 0;
    if SynSockCount = 0 then begin
      if LibHandle <> 0 then begin
        FreeLibrary(libHandle);
        LibHandle := 0;
      end;
      if LibWship6Handle <> 0 then begin
        FreeLibrary(LibWship6Handle);
        LibWship6Handle := 0;
      end;
    end;
  finally
    LeaveCriticalSection(SynSockCS);
  end;
  Result := True;
end;

initialization
  InitializeCriticalSection(SynSockCS);
  SET_IN6_IF_ADDR_ANY (@in6addr_any);
  SET_LOOPBACK_ADDR6  (@in6addr_loopback);

finalization
  SynSockCount := -254; // force release library
  DestroySocketInterface;
  DeleteCriticalSection(SynSockCS);
end.
