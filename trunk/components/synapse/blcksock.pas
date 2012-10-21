{==============================================================================|
| Project : Ararat Synapse                                       | 009.009.000 |
|==============================================================================|
| Content: Library base                                                        |
|==============================================================================|
| Copyright (c)1999-2012, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)1999-2012.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{
Special thanks to Gregor Ibic <gregor.ibic@intelicom.si>
 (Intelicom d.o.o., http://www.intelicom.si)
 for good inspiration about SSL programming.
}

{$DEFINE ONCEWINSOCK}
{Note about define ONCEWINSOCK:
If you remove this compiler directive, then socket interface is loaded and
initialized on constructor of TBlockSocket class for each socket separately.
Socket interface is used only if your need it.

If you leave this directive here, then socket interface is loaded and
initialized only once at start of your program! It boost performace on high
count of created and destroyed sockets. It eliminate possible small resource
leak on Windows systems too.
}

//{$DEFINE RAISEEXCEPT}
{When you enable this define, then is Raiseexcept property is on by default
}

{:@abstract(Synapse's library core)

Core with implementation basic socket classes.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE BCB}
{$ENDIF}
{$IFDEF BCB}
  {$ObjExportAll On}
{$ENDIF}
{$Q-}
{$H+}
{$M+}
{$TYPEDADDRESS OFF}


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

unit blcksock;

interface

uses
  SysUtils, Classes,
  synafpc,
  synsock, synautil, synacode, synaip
{$IFDEF CIL}
  ,System.Net
  ,System.Net.Sockets
  ,System.Text
{$ENDIF}
  ;

const

  SynapseRelease = '40';

  cLocalhost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  cBroadcast = '255.255.255.255';
  c6Localhost = '::1';
  c6AnyHost = '::0';
  c6Broadcast = 'ffff::1';
  cAnyPort = '0';
  CR = #$0d;
  LF = #$0a;
  CRLF = CR + LF;
  c64k = 65536;

type

  {:@abstract(Exception clas used by Synapse)
   When you enable generating of exceptions, this exception is raised by
   Synapse's units.}
  ESynapseError = class(Exception)
  private
    FErrorCode: Integer;
    FErrorMessage: string;
  published
    {:Code of error. Value depending on used operating system}
    property ErrorCode: Integer read FErrorCode Write FErrorCode;
    {:Human readable description of error.}
    property ErrorMessage: string read FErrorMessage Write FErrorMessage;
  end;

  {:Types of OnStatus events}
  THookSocketReason = (
    {:Resolving is begin. Resolved IP and port is in parameter in format like:
     'localhost.somewhere.com:25'.}
    HR_ResolvingBegin,
    {:Resolving is done. Resolved IP and port is in parameter in format like:
     'localhost.somewhere.com:25'. It is always same as in HR_ResolvingBegin!}
    HR_ResolvingEnd,
    {:Socket created by CreateSocket method. It reporting Family of created
     socket too!}
    HR_SocketCreate,
    {:Socket closed by CloseSocket method.}
    HR_SocketClose,
    {:Socket binded to IP and Port. Binded IP and Port is in parameter in format
     like: 'localhost.somewhere.com:25'.}
    HR_Bind,
    {:Socket connected to IP and Port. Connected IP and Port is in parameter in
     format like: 'localhost.somewhere.com:25'.}
    HR_Connect,
    {:Called when CanRead method is used with @True result.}
    HR_CanRead,
    {:Called when CanWrite method is used with @True result.}
    HR_CanWrite,
    {:Socket is swithed to Listen mode. (TCP socket only)}
    HR_Listen,
    {:Socket Accepting client connection. (TCP socket only)}
    HR_Accept,
    {:report count of bytes readed from socket. Number is in parameter string.
     If you need is in integer, you must use StrToInt function!}
    HR_ReadCount,
    {:report count of bytes writed to socket. Number is in parameter string. If
     you need is in integer, you must use StrToInt function!}
    HR_WriteCount,
    {:If is limiting of bandwidth on, then this reason is called when sending or
     receiving is stopped for satisfy bandwidth limit. Parameter is count of
     waiting milliseconds.}
    HR_Wait,
    {:report situation where communication error occured. When raiseexcept is
     @true, then exception is called after this Hook reason.}
    HR_Error
    );

  {:Procedural type for OnStatus event. Sender is calling TBlockSocket object,
   Reason is one of set Status events and value is optional data.}
  THookSocketStatus = procedure(Sender: TObject; Reason: THookSocketReason;
    const Value: String) of object;

  {:This procedural type is used for DataFilter hooks.}
  THookDataFilter = procedure(Sender: TObject; var Value: AnsiString) of object;

  {:This procedural type is used for hook OnCreateSocket. By this hook you can
   insert your code after initialisation of socket. (you can set special socket
   options, etc.)}
  THookCreateSocket = procedure(Sender: TObject) of object;

  {:This procedural type is used for monitoring of communication.}
  THookMonitor = procedure(Sender: TObject; Writing: Boolean;
    const Buffer: TMemory; Len: Integer) of object;

  {:This procedural type is used for hook OnAfterConnect. By this hook you can
   insert your code after TCP socket has been sucessfully connected.}
  THookAfterConnect = procedure(Sender: TObject) of object;

  {:This procedural type is used for hook OnVerifyCert. By this hook you can
   insert your additional certificate verification code. Usefull to verify server
   CN against URL. }

  THookVerifyCert = function(Sender: TObject):boolean of object;

 {:This procedural type is used for hook OnHeartbeat. By this hook you can
   call your code repeately during long socket operations.
   You must enable heartbeats by @Link(HeartbeatRate) property!}
  THookHeartbeat = procedure(Sender: TObject) of object;

  {:Specify family of socket.}
  TSocketFamily = (
    {:Default mode. Socket family is defined by target address for connection.
     It allows instant access to IPv4 and IPv6 nodes. When you need IPv6 address
     as destination, then is used IPv6 mode. othervise is used IPv4 mode.
     However this mode not working properly with preliminary IPv6 supports!}
    SF_Any,
    {:Turn this class to pure IPv4 mode. This mode is totally compatible with
     previous Synapse releases.}
    SF_IP4,
    {:Turn to only IPv6 mode.}
    SF_IP6
    );

  {:specify possible values of SOCKS modes.}
  TSocksType = (
    ST_Socks5,
    ST_Socks4
    );

  {:Specify requested SSL/TLS version for secure connection.}
  TSSLType = (
    LT_all,
    LT_SSLv2,
    LT_SSLv3,
    LT_TLSv1,
    LT_TLSv1_1,
    LT_SSHv2
    );

  {:Specify type of socket delayed option.}
  TSynaOptionType = (
    SOT_Linger,
    SOT_RecvBuff,
    SOT_SendBuff,
    SOT_NonBlock,
    SOT_RecvTimeout,
    SOT_SendTimeout,
    SOT_Reuse,
    SOT_TTL,
    SOT_Broadcast,
    SOT_MulticastTTL,
    SOT_MulticastLoop
    );

  {:@abstract(this object is used for remember delayed socket option set.)}
  TSynaOption = class(TObject)
  public
    Option: TSynaOptionType;
    Enabled: Boolean;
    Value: Integer;
  end;

  TCustomSSL = class;
  TSSLClass = class of TCustomSSL;

  {:@abstract(Basic IP object.)
   This is parent class for other class with protocol implementations. Do not
   use this class directly! Use @link(TICMPBlockSocket), @link(TRAWBlockSocket),
   @link(TTCPBlockSocket) or @link(TUDPBlockSocket) instead.}
  TBlockSocket = class(TObject)
  private
    FOnStatus: THookSocketStatus;
    FOnReadFilter: THookDataFilter;
    FOnCreateSocket: THookCreateSocket;
    FOnMonitor: THookMonitor;
    FOnHeartbeat: THookHeartbeat;
    FLocalSin: TVarSin;
    FRemoteSin: TVarSin;
    FTag: integer;
    FBuffer: AnsiString;
    FRaiseExcept: Boolean;
    FNonBlockMode: Boolean;
    FMaxLineLength: Integer;
    FMaxSendBandwidth: Integer;
    FNextSend: LongWord;
    FMaxRecvBandwidth: Integer;
    FNextRecv: LongWord;
    FConvertLineEnd: Boolean;
    FLastCR: Boolean;
    FLastLF: Boolean;
    FBinded: Boolean;
    FFamily: TSocketFamily;
    FFamilySave: TSocketFamily;
    FIP6used: Boolean;
    FPreferIP4: Boolean;
    FDelayedOptions: TList;
    FInterPacketTimeout: Boolean;
    {$IFNDEF CIL}
    FFDSet: TFDSet;
    {$ENDIF}
    FRecvCounter: Integer;
    FSendCounter: Integer;
    FSendMaxChunk: Integer;
    FStopFlag: Boolean;
    FNonblockSendTimeout: Integer;
    FHeartbeatRate: integer;
    FConnectionTimeout: integer;
    {$IFNDEF ONCEWINSOCK}
    FWsaDataOnce: TWSADATA;
    {$ENDIF}
    function GetSizeRecvBuffer: Integer;
    procedure SetSizeRecvBuffer(Size: Integer);
    function GetSizeSendBuffer: Integer;
    procedure SetSizeSendBuffer(Size: Integer);
    procedure SetNonBlockMode(Value: Boolean);
    procedure SetTTL(TTL: integer);
    function GetTTL:integer;
    procedure SetFamily(Value: TSocketFamily); virtual;
    procedure SetSocket(Value: TSocket); virtual;
    function GetWsaData: TWSAData;
    function FamilyToAF(f: TSocketFamily): TAddrFamily;
  protected
    FSocket: TSocket;
    FLastError: Integer;
    FLastErrorDesc: string;
    FOwner: TObject;
    procedure SetDelayedOption(const Value: TSynaOption);
    procedure DelayedOption(const Value: TSynaOption);
    procedure ProcessDelayedOptions;
    procedure InternalCreateSocket(Sin: TVarSin);
    procedure SetSin(var Sin: TVarSin; IP, Port: string);
    function GetSinIP(Sin: TVarSin): string;
    function GetSinPort(Sin: TVarSin): Integer;
    procedure DoStatus(Reason: THookSocketReason; const Value: string);
    procedure DoReadFilter(Buffer: TMemory; var Len: Integer);
    procedure DoMonitor(Writing: Boolean; const Buffer: TMemory; Len: Integer);
    procedure DoCreateSocket;
    procedure DoHeartbeat;
    procedure LimitBandwidth(Length: Integer; MaxB: integer; var Next: LongWord);
    procedure SetBandwidth(Value: Integer);
    function TestStopFlag: Boolean;
    procedure InternalSendStream(const Stream: TStream; WithSize, Indy: boolean); virtual;
    function InternalCanRead(Timeout: Integer): Boolean; virtual;
  public
    constructor Create;

    {:Create object and load all necessary socket library. What library is
     loaded is described by STUB parameter. If STUB is empty string, then is
     loaded default libraries.}
    constructor CreateAlternate(Stub: string);
    destructor Destroy; override;

    {:If @link(family) is not SF_Any, then create socket with type defined in
     @link(Family) property. If family is SF_Any, then do nothing! (socket is
     created automaticly when you know what type of socket you need to create.
     (i.e. inside @link(Connect) or @link(Bind) call.) When socket is created,
     then is aplyed all stored delayed socket options.}
    procedure CreateSocket;

    {:It create socket. Address resolving of Value tells what type of socket is
     created. If Value is resolved as IPv4 IP, then is created IPv4 socket. If
     value is resolved as IPv6 address, then is created IPv6 socket.}
    procedure CreateSocketByName(const Value: String);

    {:Destroy socket in use. This method is also automatically called from
     object destructor.}
    procedure CloseSocket; virtual;

    {:Abort any work on Socket and destroy them.}
    procedure AbortSocket; virtual;

    {:Connects socket to local IP address and PORT. IP address may be numeric or
     symbolic ('192.168.74.50', 'cosi.nekde.cz', 'ff08::1'). The same for PORT
     - it may be number or mnemonic port ('23', 'telnet').

     If port value is '0', system chooses itself and conects unused port in the
     range 1024 to 4096 (this depending by operating system!). Structure
     LocalSin is filled after calling this method.

     Note: If you call this on non-created socket, then socket is created
     automaticly.

     Warning: when you call : Bind('0.0.0.0','0'); then is nothing done! In this
     case is used implicit system bind instead.}
    procedure Bind(IP, Port: string);

    {:Connects socket to remote IP address and PORT. The same rules as with
     @link(BIND) method are valid. The only exception is that PORT with 0 value
     will not be connected!

     Structures LocalSin and RemoteSin will be filled with valid values.

     When you call this on non-created socket, then socket is created
     automaticly. Type of created socket is by @link(Family) property. If is
     used SF_IP4, then is created socket for IPv4. If is used SF_IP6, then is
     created socket for IPv6. When you have family on SF_Any (default!), then
     type of created socket is determined by address resolving of destination
     address. (Not work properly on prilimitary winsock IPv6 support!)}
    procedure Connect(IP, Port: string); virtual;

    {:Sets socket to receive mode for new incoming connections. It is necessary
     to use @link(TBlockSocket.BIND) function call before this method to select
     receiving port!}
    procedure Listen; virtual;

    {:Waits until new incoming connection comes. After it comes a new socket is
     automatically created (socket handler is returned by this function as
     result).}
    function Accept: TSocket; virtual;

    {:Sends data of LENGTH from BUFFER address via connected socket. System
     automatically splits data to packets.}
    function SendBuffer(Buffer: Tmemory; Length: Integer): Integer; virtual;

    {:One data BYTE is sent via connected socket.}
    procedure SendByte(Data: Byte); virtual;

    {:Send data string via connected socket. Any terminator is not added! If you
     need send true string with CR-LF termination, you must add CR-LF characters
     to sended string! Because any termination is not added automaticly, you can
     use this function for sending any binary data in binary string.}
    procedure SendString(Data: AnsiString); virtual;

    {:Send integer as four bytes to socket.}
    procedure SendInteger(Data: integer); virtual;

    {:Send data as one block to socket. Each block begin with 4 bytes with
     length of data in block. This 4 bytes is added automaticly by this
     function.}
    procedure SendBlock(const Data: AnsiString); virtual;

    {:Send data from stream to socket.}
    procedure SendStreamRaw(const Stream: TStream); virtual;

    {:Send content of stream to socket. It using @link(SendBlock) method}
    procedure SendStream(const Stream: TStream); virtual;

    {:Send content of stream to socket. It using @link(SendBlock) method and
    this is compatible with streams in Indy library.}
    procedure SendStreamIndy(const Stream: TStream); virtual;

    {:Note: This is low-level receive function. You must be sure if data is
     waiting for read before call this function for avoid deadlock!

     Waits until allocated buffer is filled by received data. Returns number of
     data received, which equals to LENGTH value under normal operation. If it
     is not equal the communication channel is possibly broken.

     On stream oriented sockets if is received 0 bytes, it mean 'socket is
     closed!"

     On datagram socket is readed first waiting datagram.}
    function RecvBuffer(Buffer: TMemory; Length: Integer): Integer; virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions!

     Method waits until data is received. If no data is received within TIMEOUT
     (in milliseconds) period, @link(LastError) is set to WSAETIMEDOUT. Methods
     serves for reading any size of data (i.e. one megabyte...). This method is
     preffered for reading from stream sockets (like TCP).}
    function RecvBufferEx(Buffer: Tmemory; Len: Integer;
      Timeout: Integer): Integer; virtual;

    {:Similar to @link(RecvBufferEx), but readed data is stored in binary
     string, not in memory buffer.}
    function RecvBufferStr(Len: Integer; Timeout: Integer): AnsiString; virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Waits until one data byte is received which is also returned as function
     result. If no data is received within TIMEOUT (in milliseconds)period,
     @link(LastError) is set to WSAETIMEDOUT and result have value 0.}
    function RecvByte(Timeout: Integer): Byte; virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Waits until one four bytes are received and return it as one Ineger Value.
     If no data is received within TIMEOUT (in milliseconds)period,
     @link(LastError) is set to WSAETIMEDOUT and result have value 0.}
    function RecvInteger(Timeout: Integer): Integer; virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Method waits until data string is received. This string is terminated by
     CR-LF characters. The resulting string is returned without this termination
     (CR-LF)! If @link(ConvertLineEnd) is used, then CR-LF sequence may not be
     exactly CR-LF. See @link(ConvertLineEnd) description. If no data is
     received within TIMEOUT (in milliseconds) period, @link(LastError) is set
     to WSAETIMEDOUT. You may also specify maximum length of reading data by
     @link(MaxLineLength) property.}
    function RecvString(Timeout: Integer): AnsiString; virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Method waits until data string is received. This string is terminated by
     Terminator string. The resulting string is returned without this
     termination. If no data is received within TIMEOUT (in milliseconds)
     period, @link(LastError) is set to WSAETIMEDOUT. You may also specify
     maximum length of reading data by @link(MaxLineLength) property.}
    function RecvTerminated(Timeout: Integer; const Terminator: AnsiString): AnsiString; virtual;

    {:Note: This is high-level receive function. It using internal
     @link(LineBuffer) and you can combine this function freely with other
     high-level functions.

     Method reads all data waiting for read. If no data is received within
     TIMEOUT (in milliseconds) period, @link(LastError) is set to WSAETIMEDOUT.
     Methods serves for reading unknown size of data. Because before call this
     function you don't know size of received data, returned data is stored in
     dynamic size binary string. This method is preffered for reading from
     stream sockets (like TCP). It is very goot for receiving datagrams too!
     (UDP protocol)}
    function RecvPacket(Timeout: Integer): AnsiString; virtual;

    {:Read one block of data from socket. Each block begin with 4 bytes with
     length of data in block. This function read first 4 bytes for get lenght,
     then it wait for reported count of bytes.}
    function RecvBlock(Timeout: Integer): AnsiString; virtual;

    {:Read all data from socket to stream until socket is closed (or any error
     occured.)}
    procedure RecvStreamRaw(const Stream: TStream; Timeout: Integer); virtual;
    {:Read requested count of bytes from socket to stream.}
    procedure RecvStreamSize(const Stream: TStream; Timeout: Integer; Size: Integer);

    {:Receive data to stream. It using @link(RecvBlock) method.}
    procedure RecvStream(const Stream: TStream; Timeout: Integer); virtual;

    {:Receive data to stream. This function is compatible with similar function
    in Indy library. It using @link(RecvBlock) method.}
    procedure RecvStreamIndy(const Stream: TStream; Timeout: Integer); virtual;

    {:Same as @link(RecvBuffer), but readed data stays in system input buffer.
    Warning: this function not respect data in @link(LineBuffer)! Is not
    recommended to use this function!}
    function PeekBuffer(Buffer: TMemory; Length: Integer): Integer; virtual;

    {:Same as @link(RecvByte), but readed data stays in input system buffer.
     Warning: this function not respect data in @link(LineBuffer)! Is not
    recommended to use this function!}
    function PeekByte(Timeout: Integer): Byte; virtual;

    {:On stream sockets it returns number of received bytes waiting for picking.
     0 is returned when there is no such data. On datagram socket it returns
     length of the first waiting datagram. Returns 0 if no datagram is waiting.}
    function WaitingData: Integer; virtual;

    {:Same as @link(WaitingData), but if exists some of data in @link(Linebuffer),
     return their length instead.}
    function WaitingDataEx: Integer;

    {:Clear all waiting data for read from buffers.}
    procedure Purge;

    {:Sets linger. Enabled linger means that the system waits another LINGER
     (in milliseconds) time for delivery of sent data. This function is only for
     stream type of socket! (TCP)}
    procedure SetLinger(Enable: Boolean; Linger: Integer);

    {:Actualize values in @link(LocalSin).}
    procedure GetSinLocal;

    {:Actualize values in @link(RemoteSin).}
    procedure GetSinRemote;

    {:Actualize values in @link(LocalSin) and @link(RemoteSin).}
    procedure GetSins;

    {:Reset @link(LastError) and @link(LastErrorDesc) to non-error state.}
    procedure ResetLastError;

    {:If you "manually" call Socket API functions, forward their return code as
     parameter to this function, which evaluates it, eventually calls
     GetLastError and found error code returns and stores to @link(LastError).}
    function SockCheck(SockResult: Integer): Integer; virtual;

    {:If @link(LastError) contains some error code and @link(RaiseExcept)
     property is @true, raise adequate exception.}
    procedure ExceptCheck;

    {:Returns local computer name as numerical or symbolic value. It try get
     fully qualified domain name. Name is returned in the format acceptable by
     functions demanding IP as input parameter.}
    function LocalName: string;

    {:Try resolve name to all possible IP address. i.e. If you pass as name
     result of @link(LocalName) method, you get all IP addresses used by local
     system.}
    procedure ResolveNameToIP(Name: string; const IPList: TStrings);

    {:Try resolve name to primary IP address. i.e. If you pass as name result of
     @link(LocalName) method, you get primary IP addresses used by local system.}
    function ResolveName(Name: string): string;

    {:Try resolve IP to their primary domain name. If IP not have domain name,
     then is returned original IP.}
    function ResolveIPToName(IP: string): string;

    {:Try resolve symbolic port name to port number. (i.e. 'Echo' to 8)}
    function ResolvePort(Port: string): Word;

    {:Set information about remote side socket. It is good for seting remote
     side for sending UDP packet, etc.}
    procedure SetRemoteSin(IP, Port: string);

    {:Picks IP socket address from @link(LocalSin).}
    function GetLocalSinIP: string; virtual;

    {:Picks IP socket address from @link(RemoteSin).}
    function GetRemoteSinIP: string; virtual;

    {:Picks socket PORT number from @link(LocalSin).}
    function GetLocalSinPort: Integer; virtual;

    {:Picks socket PORT number from @link(RemoteSin).}
    function GetRemoteSinPort: Integer; virtual;

    {:Return @TRUE, if you can read any data from socket or is incoming
     connection on TCP based socket. Status is tested for time Timeout (in
     milliseconds). If value in Timeout is 0, status is only tested and
     continue. If value in Timeout is -1, run is breaked and waiting for read
     data maybe forever.

     This function is need only on special cases, when you need use
     @link(RecvBuffer) function directly! read functioms what have timeout as
     calling parameter, calling this function internally.}
    function CanRead(Timeout: Integer): Boolean; virtual;

    {:Same as @link(CanRead), but additionally return @TRUE if is some data in
     @link(LineBuffer).}
    function CanReadEx(Timeout: Integer): Boolean; virtual;

    {:Return @TRUE, if you can to socket write any data (not full sending
     buffer). Status is tested for time Timeout (in milliseconds). If value in
     Timeout is 0, status is only tested and continue. If value in Timeout is
     -1, run is breaked and waiting for write data maybe forever.

     This function is need only on special cases!}
    function CanWrite(Timeout: Integer): Boolean; virtual;

    {:Same as @link(SendBuffer), but send datagram to address from
     @link(RemoteSin). Usefull for sending reply to datagram received by
     function @link(RecvBufferFrom).}
    function SendBufferTo(Buffer: TMemory; Length: Integer): Integer; virtual;

    {:Note: This is low-lever receive function. You must be sure if data is
     waiting for read before call this function for avoid deadlock!

     Receives first waiting datagram to allocated buffer. If there is no waiting
     one, then waits until one comes. Returns length of datagram stored in
     BUFFER. If length exceeds buffer datagram is truncated. After this
     @link(RemoteSin) structure contains information about sender of UDP packet.}
    function RecvBufferFrom(Buffer: TMemory; Length: Integer): Integer; virtual;
{$IFNDEF CIL}
    {:This function is for check for incoming data on set of sockets. Whitch
    sockets is checked is decribed by SocketList Tlist with TBlockSocket
    objects. TList may have maximal number of objects defined by FD_SETSIZE
    constant. Return @TRUE, if you can from some socket read any data or is
    incoming connection on TCP based socket. Status is tested for time Timeout
    (in milliseconds). If value in Timeout is 0, status is only tested and
    continue. If value in Timeout is -1, run is breaked and waiting for read
    data maybe forever. If is returned @TRUE, CanReadList TList is filled by all
    TBlockSocket objects what waiting for read.}
    function GroupCanRead(const SocketList: TList; Timeout: Integer;
      const CanReadList: TList): Boolean;
{$ENDIF}
    {:By this method you may turn address reuse mode for local @link(bind). It
     is good specially for UDP protocol. Using this with TCP protocol is
     hazardous!}
    procedure EnableReuse(Value: Boolean);

    {:Try set timeout for all sending and receiving operations, if socket
     provider can do it. (It not supported by all socket providers!)}
    procedure SetTimeout(Timeout: Integer);

    {:Try set timeout for all sending operations, if socket provider can do it.
     (It not supported by all socket providers!)}
    procedure SetSendTimeout(Timeout: Integer);

    {:Try set timeout for all receiving operations, if socket provider can do
     it. (It not supported by all socket providers!)}
    procedure SetRecvTimeout(Timeout: Integer);

    {:Return value of socket type.}
    function GetSocketType: integer; Virtual;

    {:Return value of protocol type for socket creation.}
    function GetSocketProtocol: integer; Virtual;

    {:WSA structure with information about socket provider. On non-windows 
     platforms this structure is simulated!}
    property WSAData: TWSADATA read GetWsaData;

    {:FDset structure prepared for usage with this socket.}
    property FDset: TFDSet read FFDset;

    {:Structure describing local socket side.}
    property LocalSin: TVarSin read FLocalSin write FLocalSin;

    {:Structure describing remote socket side.}
    property RemoteSin: TVarSin read FRemoteSin write FRemoteSin;

    {:Socket handler. Suitable for "manual" calls to socket API or manual
     connection of socket to a previously created socket (i.e by Accept method
     on TCP socket)}
    property Socket: TSocket read FSocket write SetSocket;

    {:Last socket operation error code. Error codes are described in socket
     documentation. Human readable error description is stored in
     @link(LastErrorDesc) property.}
    property LastError: Integer read FLastError;

    {:Human readable error description of @link(LastError) code.}
    property LastErrorDesc: string read FLastErrorDesc;

    {:Buffer used by all high-level receiving functions. This buffer is used for
     optimized reading of data from socket. In normal cases you not need access
     to this buffer directly!}
    property LineBuffer: AnsiString read FBuffer write FBuffer;

    {:Size of Winsock receive buffer. If it is not supported by socket provider,
     it return as size one kilobyte.}
    property SizeRecvBuffer: Integer read GetSizeRecvBuffer write SetSizeRecvBuffer;

    {:Size of Winsock send buffer. If it is not supported by socket provider, it
     return as size one kilobyte.}
    property SizeSendBuffer: Integer read GetSizeSendBuffer write SetSizeSendBuffer;

    {:If @True, turn class to non-blocking mode. Not all functions are working
     properly in this mode, you must know exactly what you are doing! However
     when you have big experience with non-blocking programming, then you can
     optimise your program by non-block mode!}
    property NonBlockMode: Boolean read FNonBlockMode Write SetNonBlockMode;

    {:Set Time-to-live value. (if system supporting it!)}
    property TTL: Integer read GetTTL Write SetTTL;

    {:If is @true, then class in in IPv6 mode.}
    property IP6used: Boolean read FIP6used;

    {:Return count of received bytes on this socket from begin of current
     connection.}
    property RecvCounter: Integer read FRecvCounter;

    {:Return count of sended bytes on this socket from begin of current
     connection.}
    property SendCounter: Integer read FSendCounter;
  published
    {:Return descriptive string for given error code. This is class function.
     You may call it without created object!}
    class function GetErrorDesc(ErrorCode: Integer): string;

    {:Return descriptive string for @link(LastError).}
    function GetErrorDescEx: string; virtual;

    {:this value is for free use.}
    property Tag: Integer read FTag write FTag;

    {:If @true, winsock errors raises exception. Otherwise is setted
    @link(LastError) value only and you must check it from your program! Default
    value is @false.}
    property RaiseExcept: Boolean read FRaiseExcept write FRaiseExcept;

    {:Define maximum length in bytes of @link(LineBuffer) for high-level
     receiving functions. If this functions try to read more data then this
     limit, error is returned! If value is 0 (default), no limitation is used.
     This is very good protection for stupid attacks to your server by sending
     lot of data without proper terminator... until all your memory is allocated
     by LineBuffer!

     Note: This maximum length is checked only in functions, what read unknown
     number of bytes! (like @link(RecvString) or @link(RecvTerminated))}
    property MaxLineLength: Integer read FMaxLineLength Write FMaxLineLength;

    {:Define maximal bandwidth for all sending operations in bytes per second.
     If value is 0 (default), bandwidth limitation is not used.}
    property MaxSendBandwidth: Integer read FMaxSendBandwidth Write FMaxSendBandwidth;

    {:Define maximal bandwidth for all receiving operations in bytes per second.
     If value is 0 (default), bandwidth limitation is not used.}
    property MaxRecvBandwidth: Integer read FMaxRecvBandwidth Write FMaxRecvBandwidth;

    {:Define maximal bandwidth for all sending and receiving operations in bytes
     per second. If value is 0 (default), bandwidth limitation is not used.}
    property MaxBandwidth: Integer Write SetBandwidth;

    {:Do a conversion of non-standard line terminators to CRLF. (Off by default)
     If @True, then terminators like sigle CR, single LF or LFCR are converted
     to CRLF internally. This have effect only in @link(RecvString) method!}
    property ConvertLineEnd: Boolean read FConvertLineEnd Write FConvertLineEnd;

    {:Specified Family of this socket. When you are using Windows preliminary
     support for IPv6, then I recommend to set this property!}
    property Family: TSocketFamily read FFamily Write SetFamily;

    {:When resolving of domain name return both IPv4 and IPv6 addresses, then
     specify if is used IPv4 (dafault - @true) or IPv6.}
    property PreferIP4: Boolean read FPreferIP4 Write FPreferIP4;

    {:By default (@true) is all timeouts used as timeout between two packets in
     reading operations. If you set this to @false, then Timeouts is for overall
     reading operation!}
    property InterPacketTimeout: Boolean read FInterPacketTimeout Write FInterPacketTimeout;

    {:All sended datas was splitted by this value.}
    property SendMaxChunk: Integer read FSendMaxChunk Write FSendMaxChunk;

    {:By setting this property to @true you can stop any communication. You can
     use this property for soft abort of communication.}
    property StopFlag: Boolean read FStopFlag Write FStopFlag;

    {:Timeout for data sending by non-blocking socket mode.}
    property NonblockSendTimeout: Integer read FNonblockSendTimeout Write FNonblockSendTimeout;

    {:Timeout for @link(Connect) call. Default value 0 means default system timeout.
     Non-zero value means timeout in millisecond.}
    property ConnectionTimeout: Integer read FConnectionTimeout write FConnectionTimeout;

    {:This event is called by various reasons. It is good for monitoring socket,
     create gauges for data transfers, etc.}
    property OnStatus: THookSocketStatus read FOnStatus write FOnStatus;

    {:this event is good for some internal thinks about filtering readed datas.
     It is used by telnet client by example.}
    property OnReadFilter: THookDataFilter read FOnReadFilter write FOnReadFilter;

    {:This event is called after real socket creation for setting special socket
     options, because you not know when socket is created. (it is depended on
     Ipv4, IPv6 or automatic mode)}
    property OnCreateSocket: THookCreateSocket read FOnCreateSocket write FOnCreateSocket;

    {:This event is good for monitoring content of readed or writed datas.}
    property OnMonitor: THookMonitor read FOnMonitor write FOnMonitor;

    {:This event is good for calling your code during long socket operations.
      (Example, for refresing UI if class in not called within the thread.)
      Rate of heartbeats can be modified by @link(HeartbeatRate) property.}
    property OnHeartbeat: THookHeartbeat read FOnHeartbeat write FOnHeartbeat;

    {:Specify typical rate of @link(OnHeartbeat) event and @link(StopFlag) testing.
      Default value 0 disabling heartbeats! Value is in milliseconds.
      Real rate can be higher or smaller then this value, because it depending
      on real socket operations too!
      Note: Each heartbeat slowing socket processing.}
    property HeartbeatRate: integer read FHeartbeatRate Write FHeartbeatRate;
    {:What class own this socket? Used by protocol implementation classes.}
    property Owner: TObject read FOwner Write FOwner;
  end;

  {:@abstract(Support for SOCKS4 and SOCKS5 proxy)
   Layer with definition all necessary properties and functions for
   implementation SOCKS proxy client. Do not use this class directly.}
  TSocksBlockSocket = class(TBlockSocket)
  protected
    FSocksIP: string;
    FSocksPort: string;
    FSocksTimeout: integer;
    FSocksUsername: string;
    FSocksPassword: string;
    FUsingSocks: Boolean;
    FSocksResolver: Boolean;
    FSocksLastError: integer;
    FSocksResponseIP: string;
    FSocksResponsePort: string;
    FSocksLocalIP: string;
    FSocksLocalPort: string;
    FSocksRemoteIP: string;
    FSocksRemotePort: string;
    FBypassFlag: Boolean;
    FSocksType: TSocksType;
    function SocksCode(IP, Port: string): Ansistring;
    function SocksDecode(Value: Ansistring): integer;
  public
    constructor Create;

    {:Open connection to SOCKS proxy and if @link(SocksUsername) is set, do
     authorisation to proxy. This is needed only in special cases! (it is called
     internally!)}
    function SocksOpen: Boolean;

    {:Send specified request to SOCKS proxy. This is needed only in special
     cases! (it is called internally!)}
    function SocksRequest(Cmd: Byte; const IP, Port: string): Boolean;

    {:Receive response to previosly sended request. This is needed only in
     special cases! (it is called internally!)}
    function SocksResponse: Boolean;

    {:Is @True when class is using SOCKS proxy.}
    property UsingSocks: Boolean read FUsingSocks;

    {:If SOCKS proxy failed, here is error code returned from SOCKS proxy.}
    property SocksLastError: integer read FSocksLastError;
  published
    {:Address of SOCKS server. If value is empty string, SOCKS support is
     disabled. Assingning any value to this property enable SOCKS mode.
     Warning: You cannot combine this mode with HTTP-tunneling mode!}
    property SocksIP: string read FSocksIP write FSocksIP;

    {:Port of SOCKS server. Default value is '1080'.}
    property SocksPort: string read FSocksPort write FSocksPort;

    {:If you need authorisation on SOCKS server, set username here.}
    property SocksUsername: string read FSocksUsername write FSocksUsername;

    {:If you need authorisation on SOCKS server, set password here.}
    property SocksPassword: string read FSocksPassword write FSocksPassword;

    {:Specify timeout for communicatin with SOCKS server. Default is one minute.}
    property SocksTimeout: integer read FSocksTimeout write FSocksTimeout;

    {:If @True, all symbolic names of target hosts is not translated to IP's
     locally, but resolving is by SOCKS proxy. Default is @True.}
    property SocksResolver: Boolean read FSocksResolver write FSocksResolver;

    {:Specify SOCKS type. By default is used SOCKS5, but you can use SOCKS4 too.
     When you select SOCKS4, then if @link(SOCKSResolver) is enabled, then is
     used SOCKS4a. Othervise is used pure SOCKS4.}
    property SocksType: TSocksType read FSocksType write FSocksType;
  end;

  {:@abstract(Implementation of TCP socket.)
   Supported features: IPv4, IPv6, SSL/TLS or SSH (depending on used plugin),
   SOCKS5 proxy (outgoing connections and limited incomming), SOCKS4/4a proxy
   (outgoing connections and limited incomming), TCP through HTTP proxy tunnel.}
  TTCPBlockSocket = class(TSocksBlockSocket)
  protected
    FOnAfterConnect: THookAfterConnect;
    FSSL: TCustomSSL;
    FHTTPTunnelIP: string;
    FHTTPTunnelPort: string;
    FHTTPTunnel: Boolean;
    FHTTPTunnelRemoteIP: string;
    FHTTPTunnelRemotePort: string;
    FHTTPTunnelUser: string;
    FHTTPTunnelPass: string;
    FHTTPTunnelTimeout: integer;
    procedure SocksDoConnect(IP, Port: string);
    procedure HTTPTunnelDoConnect(IP, Port: string);
    procedure DoAfterConnect;
  public
    {:Create TCP socket class with default plugin for SSL/TSL/SSH implementation
    (see @link(SSLImplementation))}
    constructor Create;

    {:Create TCP socket class with desired plugin for SSL/TSL/SSH implementation}
    constructor CreateWithSSL(SSLPlugin: TSSLClass);
    destructor Destroy; override;

    {:See @link(TBlockSocket.CloseSocket)}
    procedure CloseSocket; override;

    {:See @link(TBlockSocket.WaitingData)}
    function WaitingData: Integer; override;

    {:Sets socket to receive mode for new incoming connections. It is necessary
     to use @link(TBlockSocket.BIND) function call before this method to select
     receiving port!

     If you use SOCKS, activate incoming TCP connection by this proxy. (By BIND
     method of SOCKS.)}
    procedure Listen; override;

    {:Waits until new incoming connection comes. After it comes a new socket is
     automatically created (socket handler is returned by this function as
     result).

     If you use SOCKS, new socket is not created! In this case is used same
     socket as socket for listening! So, you can accept only one connection in
     SOCKS mode.}
    function Accept: TSocket; override;

    {:Connects socket to remote IP address and PORT. The same rules as with
     @link(TBlockSocket.BIND) method are valid. The only exception is that PORT
     with 0 value will not be connected. After call to this method
     a communication channel between local and remote socket is created. Local
     socket is assigned automatically if not controlled by previous call to
     @link(TBlockSocket.BIND) method. Structures @link(TBlockSocket.LocalSin)
     and @link(TBlockSocket.RemoteSin) will be filled with valid values.

     If you use SOCKS, activate outgoing TCP connection by SOCKS proxy specified
     in @link(TSocksBlockSocket.SocksIP). (By CONNECT method of SOCKS.)

     If you use HTTP-tunnel mode, activate outgoing TCP connection by HTTP
     tunnel specified in @link(HTTPTunnelIP). (By CONNECT method of HTTP
     protocol.)

     Note: If you call this on non-created socket, then socket is created
     automaticly.}
    procedure Connect(IP, Port: string); override;

    {:If you need upgrade existing TCP connection to SSL/TLS (or SSH2, if plugin
     allows it) mode, then call this method. This method switch this class to
     SSL mode and do SSL/TSL handshake.}
    procedure SSLDoConnect;

    {:By this method you can downgrade existing SSL/TLS connection to normal TCP
     connection.}
    procedure SSLDoShutdown;

    {:If you need use this component as SSL/TLS TCP server, then after accepting
     of inbound connection you need start SSL/TLS session by this method. Before
     call this function, you must have assigned all neeeded certificates and
     keys!}
    function SSLAcceptConnection: Boolean;

    {:See @link(TBlockSocket.GetLocalSinIP)}
    function GetLocalSinIP: string; override;

    {:See @link(TBlockSocket.GetRemoteSinIP)}
    function GetRemoteSinIP: string; override;

    {:See @link(TBlockSocket.GetLocalSinPort)}
    function GetLocalSinPort: Integer; override;

    {:See @link(TBlockSocket.GetRemoteSinPort)}
    function GetRemoteSinPort: Integer; override;

    {:See @link(TBlockSocket.SendBuffer)}
    function SendBuffer(Buffer: TMemory; Length: Integer): Integer; override;

    {:See @link(TBlockSocket.RecvBuffer)}
    function RecvBuffer(Buffer: TMemory; Len: Integer): Integer; override;

    {:Return value of socket type. For TCP return SOCK_STREAM.}
    function GetSocketType: integer; override;

    {:Return value of protocol type for socket creation. For TCP return
     IPPROTO_TCP.}
    function GetSocketProtocol: integer; override;

    {:Class implementing SSL/TLS support. It is allways some descendant
     of @link(TCustomSSL) class. When programmer not select some SSL plugin
     class, then is used @link(TSSLNone)}
    property SSL: TCustomSSL read FSSL;

    {:@True if is used HTTP tunnel mode.}
    property HTTPTunnel: Boolean read FHTTPTunnel;
  published
    {:Return descriptive string for @link(LastError). On case of error
     in SSL/TLS subsystem, it returns right error description.}
    function GetErrorDescEx: string; override;

    {:Specify IP address of HTTP proxy. Assingning non-empty value to this
     property enable HTTP-tunnel mode. This mode is for tunnelling any outgoing
     TCP connection through HTTP proxy server. (If policy on HTTP proxy server
     allow this!) Warning: You cannot combine this mode with SOCK5 mode!}
    property HTTPTunnelIP: string read FHTTPTunnelIP Write FHTTPTunnelIP;

    {:Specify port of HTTP proxy for HTTP-tunneling.}
    property HTTPTunnelPort: string read FHTTPTunnelPort Write FHTTPTunnelPort;

    {:Specify authorisation username for access to HTTP proxy in HTTP-tunnel
     mode. If you not need authorisation, then let this property empty.}
    property HTTPTunnelUser: string read FHTTPTunnelUser Write FHTTPTunnelUser;

    {:Specify authorisation password for access to HTTP proxy in HTTP-tunnel
     mode.}
    property HTTPTunnelPass: string read FHTTPTunnelPass Write FHTTPTunnelPass;

    {:Specify timeout for communication with HTTP proxy in HTTPtunnel mode.}
    property HTTPTunnelTimeout: integer read FHTTPTunnelTimeout Write FHTTPTunnelTimeout;

    {:This event is called after sucessful TCP socket connection.}
    property OnAfterConnect: THookAfterConnect read FOnAfterConnect write FOnAfterConnect;
  end;

  {:@abstract(Datagram based communication)
   This class implementing datagram based communication instead default stream
   based communication style.}
  TDgramBlockSocket = class(TSocksBlockSocket)
  public
    {:Fill @link(TBlockSocket.RemoteSin) structure. This address is used for
     sending data.}
    procedure Connect(IP, Port: string); override;

    {:Silently redirected to @link(TBlockSocket.SendBufferTo).}
    function SendBuffer(Buffer: TMemory; Length: Integer): Integer; override;

    {:Silently redirected to @link(TBlockSocket.RecvBufferFrom).}
    function RecvBuffer(Buffer: TMemory; Length: Integer): Integer; override;
  end;

  {:@abstract(Implementation of UDP socket.)
   NOTE: in this class is all receiving redirected to RecvBufferFrom. You can
   use for reading any receive function. Preffered is RecvPacket! Similary all
   sending is redirected to SendbufferTo. You can use for sending UDP packet any
   sending function, like SendString.

   Supported features: IPv4, IPv6, unicasts, broadcasts, multicasts, SOCKS5
   proxy (only unicasts! Outgoing and incomming.)}
  TUDPBlockSocket = class(TDgramBlockSocket)
  protected
    FSocksControlSock: TTCPBlockSocket;
    function UdpAssociation: Boolean;
    procedure SetMulticastTTL(TTL: integer);
    function GetMulticastTTL:integer;
  public
    destructor Destroy; override;

    {:Enable or disable sending of broadcasts. If seting OK, result is @true.
     This method is not supported in SOCKS5 mode! IPv6 does not support
     broadcasts! In this case you must use Multicasts instead.}
    procedure EnableBroadcast(Value: Boolean);

    {:See @link(TBlockSocket.SendBufferTo)}
    function SendBufferTo(Buffer: TMemory; Length: Integer): Integer; override;

    {:See @link(TBlockSocket.RecvBufferFrom)}
    function RecvBufferFrom(Buffer: TMemory; Length: Integer): Integer; override;
{$IFNDEF CIL}
    {:Add this socket to given multicast group. You cannot use Multicasts in
     SOCKS mode!}
    procedure AddMulticast(MCastIP:string);

    {:Remove this socket from given multicast group.}
    procedure DropMulticast(MCastIP:string);
{$ENDIF}
    {:All sended multicast datagrams is loopbacked to your interface too. (you
     can read your sended datas.) You can disable this feature by this function.
     This function not working on some Windows systems!}
    procedure EnableMulticastLoop(Value: Boolean);

    {:Return value of socket type. For UDP return SOCK_DGRAM.}
    function GetSocketType: integer; override;

    {:Return value of protocol type for socket creation. For UDP return
     IPPROTO_UDP.}
    function GetSocketProtocol: integer; override;

    {:Set Time-to-live value for multicasts packets. It define number of routers
     for transfer of datas. If you set this to 1 (dafault system value), then
     multicasts packet goes only to you local network. If you need transport
     multicast packet to worldwide, then increase this value, but be carefull,
     lot of routers on internet does not transport multicasts packets!}
    property MulticastTTL: Integer read GetMulticastTTL Write SetMulticastTTL;
  end;

  {:@abstract(Implementation of RAW ICMP socket.)
   For this object you must have rights for creating RAW sockets!}
  TICMPBlockSocket = class(TDgramBlockSocket)
  public
    {:Return value of socket type. For RAW and ICMP return SOCK_RAW.}
    function GetSocketType: integer; override;

    {:Return value of protocol type for socket creation. For ICMP returns
     IPPROTO_ICMP or IPPROTO_ICMPV6}
    function GetSocketProtocol: integer; override;
  end;

  {:@abstract(Implementation of RAW socket.)
   For this object you must have rights for creating RAW sockets!}
  TRAWBlockSocket = class(TBlockSocket)
  public
    {:Return value of socket type. For RAW and ICMP return SOCK_RAW.}
    function GetSocketType: integer; override;

    {:Return value of protocol type for socket creation. For RAW returns
     IPPROTO_RAW.}
    function GetSocketProtocol: integer; override;
  end;

  {:@abstract(Implementation of PGM-message socket.)
   Not all systems supports this protocol!}
  TPGMMessageBlockSocket = class(TBlockSocket)
  public
    {:Return value of socket type. For PGM-message return SOCK_RDM.}
    function GetSocketType: integer; override;

    {:Return value of protocol type for socket creation. For PGM-message returns
     IPPROTO_RM.}
    function GetSocketProtocol: integer; override;
  end;

  {:@abstract(Implementation of PGM-stream socket.)
   Not all systems supports this protocol!}
  TPGMStreamBlockSocket = class(TBlockSocket)
  public
    {:Return value of socket type. For PGM-stream return SOCK_STREAM.}
    function GetSocketType: integer; override;

    {:Return value of protocol type for socket creation. For PGM-stream returns
     IPPROTO_RM.}
    function GetSocketProtocol: integer; override;
  end;

  {:@abstract(Parent class for all SSL plugins.)
   This is abstract class defining interface for other SSL plugins.

   Instance of this class will be created for each @link(TTCPBlockSocket).

   Warning: not all methods and propertis can work in all existing SSL plugins!
   Please, read documentation of used SSL plugin.}
  TCustomSSL = class(TObject)
  private
  protected
    FOnVerifyCert: THookVerifyCert;
    FSocket: TTCPBlockSocket;
    FSSLEnabled: Boolean;
    FLastError: integer;
    FLastErrorDesc: string;
    FSSLType: TSSLType;
    FKeyPassword: string;
    FCiphers: string;
    FCertificateFile: string;
    FPrivateKeyFile: string;
    FCertificate: Ansistring;
    FPrivateKey: Ansistring;
    FPFX: Ansistring;
    FPFXfile: string;
    FCertCA: Ansistring;
    FCertCAFile: string;
    FTrustCertificate: Ansistring;
    FTrustCertificateFile: string;
    FVerifyCert: Boolean;
    FUsername: string;
    FPassword: string;
    FSSHChannelType: string;
    FSSHChannelArg1: string;
    FSSHChannelArg2: string;
    FCertComplianceLevel: integer;
    FSNIHost: string;
    procedure ReturnError;
    procedure SetCertCAFile(const Value: string); virtual;
    function DoVerifyCert:boolean;
    function CreateSelfSignedCert(Host: string): Boolean; virtual;
  public
    {: Create plugin class. it is called internally from @link(TTCPBlockSocket)}
    constructor Create(const Value: TTCPBlockSocket); virtual;

    {: Assign settings (certificates and configuration) from another SSL plugin
     class.}
    procedure Assign(const Value: TCustomSSL); virtual;

    {: return description of used plugin. It usually return name and version
     of used SSL library.}
    function LibVersion: String; virtual;

    {: return name of used plugin.}
    function LibName: String; virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for start SSL connection.}
    function Connect: boolean; virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for acept new SSL connection.}
    function Accept: boolean; virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for hard shutdown of SSL connection. (for example,
     before socket is closed)}
    function Shutdown: boolean; virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for soft shutdown of SSL connection. (for example,
     when you need to continue with unprotected connection.)}
    function BiShutdown: boolean; virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for sending some datas by SSL connection.}
    function SendBuffer(Buffer: TMemory; Len: Integer): Integer; virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for receiving some datas by SSL connection.}
    function RecvBuffer(Buffer: TMemory; Len: Integer): Integer; virtual;

    {: Do not call this directly. It is used internally by @link(TTCPBlockSocket)!

     Here is needed code for getting count of datas what waiting for read.
     If SSL plugin not allows this, then it should return 0.}
    function WaitingData: Integer; virtual;

    {:Return string with identificator of SSL/TLS version of existing
     connection.}
    function GetSSLVersion: string; virtual;

    {:Return subject of remote SSL peer.}
    function GetPeerSubject: string; virtual;

    {:Return Serial number if remote X509 certificate.}
    function GetPeerSerialNo: integer; virtual;

    {:Return issuer certificate of remote SSL peer.}
    function GetPeerIssuer: string; virtual;

    {:Return peer name from remote side certificate. This is good for verify,
     if certificate is generated for remote side IP name.}
    function GetPeerName: string; virtual;

    {:Returns has of peer name from remote side certificate. This is good
     for fast remote side authentication.}
    function GetPeerNameHash: cardinal; virtual;

    {:Return fingerprint of remote SSL peer.}
    function GetPeerFingerprint: string; virtual;

    {:Return all detailed information about certificate from remote side of
     SSL/TLS connection. Result string can be multilined! Each plugin can return
     this informations in different format!}
    function GetCertInfo: string; virtual;

    {:Return currently used Cipher.}
    function GetCipherName: string; virtual;

    {:Return currently used number of bits in current Cipher algorythm.}
    function GetCipherBits: integer; virtual;

    {:Return number of bits in current Cipher algorythm.}
    function GetCipherAlgBits: integer; virtual;

    {:Return result value of verify remote side certificate. Look to OpenSSL
     documentation for possible values. For example 0 is successfuly verified
     certificate, or 18 is self-signed certificate.}
    function GetVerifyCert: integer; virtual;

    {: Resurn @true if SSL mode is enabled on existing cvonnection.}
    property SSLEnabled: Boolean read FSSLEnabled;

    {:Return error code of last SSL operation. 0 is OK.}
    property LastError: integer read FLastError;

    {:Return error description of last SSL operation.}
    property LastErrorDesc: string read FLastErrorDesc;
  published
    {:Here you can specify requested SSL/TLS mode. Default is autodetection, but
     on some servers autodetection not working properly. In this case you must
     specify requested SSL/TLS mode by your hand!}
    property SSLType: TSSLType read FSSLType write FSSLType;

    {:Password for decrypting of encoded certificate or key.}
    property KeyPassword: string read FKeyPassword write FKeyPassword;

    {:Username for possible credentials.}
    property Username: string read FUsername write FUsername;

    {:password for possible credentials.}
    property Password: string read FPassword write FPassword;

    {:By this property you can modify default set of SSL/TLS ciphers.}
    property Ciphers: string read FCiphers write FCiphers;

    {:Used for loading certificate from disk file. See to plugin documentation
     if this method is supported and how!}
    property CertificateFile: string read FCertificateFile write FCertificateFile;

    {:Used for loading private key from disk file. See to plugin documentation
     if this method is supported and how!}
    property PrivateKeyFile: string read FPrivateKeyFile write FPrivateKeyFile;

    {:Used for loading certificate from binary string. See to plugin documentation
     if this method is supported and how!}
    property Certificate: Ansistring read FCertificate write FCertificate;

    {:Used for loading private key from binary string. See to plugin documentation
     if this method is supported and how!}
    property PrivateKey: Ansistring read FPrivateKey write FPrivateKey;

    {:Used for loading PFX from binary string. See to plugin documentation
     if this method is supported and how!}
    property PFX: Ansistring read FPFX write FPFX;

    {:Used for loading PFX from disk file. See to plugin documentation
     if this method is supported and how!}
    property PFXfile: string read FPFXfile write FPFXfile;

    {:Used for loading trusted certificates from disk file. See to plugin documentation
     if this method is supported and how!}
    property TrustCertificateFile: string read FTrustCertificateFile write FTrustCertificateFile;

    {:Used for loading trusted certificates from binary string. See to plugin documentation
     if this method is supported and how!}
    property TrustCertificate: Ansistring read FTrustCertificate write FTrustCertificate;

    {:Used for loading CA certificates from binary string. See to plugin documentation
     if this method is supported and how!}
    property CertCA: Ansistring read FCertCA write FCertCA;

    {:Used for loading CA certificates from disk file. See to plugin documentation
     if this method is supported and how!}
    property CertCAFile: string read FCertCAFile write SetCertCAFile;

    {:If @true, then is verified client certificate. (it is good for writing
     SSL/TLS servers.) When you are not server, but you are client, then if this
     property is @true, verify servers certificate.}
    property VerifyCert: Boolean read FVerifyCert write FVerifyCert;

    {:channel type for possible SSH connections}
    property SSHChannelType: string read FSSHChannelType write FSSHChannelType;

    {:First argument of channel type for possible SSH connections}
    property SSHChannelArg1: string read FSSHChannelArg1 write FSSHChannelArg1;

    {:Second argument of channel type for possible SSH connections}
    property SSHChannelArg2: string read FSSHChannelArg2 write FSSHChannelArg2;

    {: Level of standards compliance level
      (CryptLib: values in cryptlib.pas, -1: use default value )  }
    property CertComplianceLevel:integer read FCertComplianceLevel write FCertComplianceLevel;

    {:This event is called when verifying the server certificate immediatally after
     a successfull verification in the ssl library.}
    property OnVerifyCert: THookVerifyCert read FOnVerifyCert write FOnVerifyCert;

    {: Server Name Identification. Host name to send to server. If empty the host name
       found in URL will be used, which should be the normal use (http Header Host = SNI Host).
       The value is cleared after the connection is established.
      (SNI support requires OpenSSL 0.9.8k or later. Cryptlib not supported, yet )  }
    property SNIHost:string read FSNIHost write FSNIHost;
  end;

  {:@abstract(Default SSL plugin with no SSL support.)
   Dummy SSL plugin implementation for applications without SSL/TLS support.}
  TSSLNone = class (TCustomSSL)
  public
    {:See @inherited}
    function LibVersion: String; override;
    {:See @inherited}
    function LibName: String; override;
  end;

  {:@abstract(Record with definition of IP packet header.)
   For reading data from ICMP or RAW sockets.}
  TIPHeader = record
    VerLen: Byte;
    TOS: Byte;
    TotalLen: Word;
    Identifer: Word;
    FragOffsets: Word;
    TTL: Byte;
    Protocol: Byte;
    CheckSum: Word;
    SourceIp: LongWord;
    DestIp: LongWord;
    Options: LongWord;
  end;

  {:@abstract(Parent class of application protocol implementations.)
   By this class is defined common properties.}
  TSynaClient = Class(TObject)
  protected
    FTargetHost: string;
    FTargetPort: string;
    FIPInterface: string;
    FTimeout: integer;
    FUserName: string;
    FPassword: string;
  public
    constructor Create;
  published
    {:Specify terget server IP (or symbolic name). Default is 'localhost'.}
    property TargetHost: string read FTargetHost Write FTargetHost;

    {:Specify terget server port (or symbolic name).}
    property TargetPort: string read FTargetPort Write FTargetPort;

    {:Defined local socket address. (outgoing IP address). By default is used
     '0.0.0.0' as wildcard for default IP.}
    property IPInterface: string read FIPInterface Write FIPInterface;

    {:Specify default timeout for socket operations.}
    property Timeout: integer read FTimeout Write FTimeout;

    {:If protocol need user authorization, then fill here username.}
    property UserName: string read FUserName Write FUserName;

    {:If protocol need user authorization, then fill here password.}
    property Password: string read FPassword Write FPassword;
  end;

var
  {:Selected SSL plugin. Default is @link(TSSLNone).

   Do not change this value directly!!!

   Just add your plugin unit to your project uses instead. Each plugin unit have
   initialization code what modify this variable.}
  SSLImplementation: TSSLClass = TSSLNone;

implementation

{$IFDEF ONCEWINSOCK}
var
  WsaDataOnce: TWSADATA;
  e: ESynapseError;
{$ENDIF}


constructor TBlockSocket.Create;
begin
  CreateAlternate('');
end;

constructor TBlockSocket.CreateAlternate(Stub: string);
{$IFNDEF ONCEWINSOCK}
var
  e: ESynapseError;
{$ENDIF}
begin
  inherited Create;
  FDelayedOptions := TList.Create;
  FRaiseExcept := False;
{$IFDEF RAISEEXCEPT}
  FRaiseExcept := True;
{$ENDIF}
  FSocket := INVALID_SOCKET;
  FBuffer := '';
  FLastCR := False;
  FLastLF := False;
  FBinded := False;
  FNonBlockMode := False;
  FMaxLineLength := 0;
  FMaxSendBandwidth := 0;
  FNextSend := 0;
  FMaxRecvBandwidth := 0;
  FNextRecv := 0;
  FConvertLineEnd := False;
  FFamily := SF_Any;
  FFamilySave := SF_Any;
  FIP6used := False;
  FPreferIP4 := True;
  FInterPacketTimeout := True;
  FRecvCounter := 0;
  FSendCounter := 0;
  FSendMaxChunk := c64k;
  FStopFlag := False;
  FNonblockSendTimeout := 15000;
  FHeartbeatRate := 0;
  FConnectionTimeout := 0;
  FOwner := nil;
{$IFNDEF ONCEWINSOCK}
  if Stub = '' then
    Stub := DLLStackName;
  if not InitSocketInterface(Stub) then
  begin
    e := ESynapseError.Create('Error loading Socket interface (' + Stub + ')!');
    e.ErrorCode := 0;
    e.ErrorMessage := 'Error loading Socket interface (' + Stub + ')!';
    raise e;
  end;
  SockCheck(synsock.WSAStartup(WinsockLevel, FWsaDataOnce));
  ExceptCheck;
{$ENDIF}
end;

destructor TBlockSocket.Destroy;
var
  n: integer;
  p: TSynaOption;
begin
  CloseSocket;
{$IFNDEF ONCEWINSOCK}
  synsock.WSACleanup;
  DestroySocketInterface;
{$ENDIF}
  for n := FDelayedOptions.Count - 1 downto 0 do
    begin
      p := TSynaOption(FDelayedOptions[n]);
      p.Free;
    end;
  FDelayedOptions.Free;
  inherited Destroy;
end;

function TBlockSocket.FamilyToAF(f: TSocketFamily): TAddrFamily;
begin
  case f of
    SF_ip4:
      Result := AF_INET;
    SF_ip6:
      Result := AF_INET6;
  else
    Result := AF_UNSPEC;
  end;
end;

procedure TBlockSocket.SetDelayedOption(const Value: TSynaOption);
var
  li: TLinger;
  x: integer;
  buf: TMemory;
{$IFNDEF MSWINDOWS}
  timeval: TTimeval;
{$ENDIF}
begin
  case value.Option of
    SOT_Linger:
      begin
        {$IFDEF CIL}
        li := TLinger.Create(Value.Enabled, Value.Value div 1000);
        synsock.SetSockOptObj(FSocket, integer(SOL_SOCKET), integer(SO_LINGER), li);
        {$ELSE}
        li.l_onoff := Ord(Value.Enabled);
        li.l_linger := Value.Value div 1000;
        buf := @li;
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_LINGER), buf, SizeOf(li));
        {$ENDIF}
      end;
    SOT_RecvBuff:
      begin
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        {$ELSE}
        buf := @Value.Value;
        {$ENDIF}
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_RCVBUF),
          buf, SizeOf(Value.Value));
      end;
    SOT_SendBuff:
      begin
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        {$ELSE}
        buf := @Value.Value;
        {$ENDIF}
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_SNDBUF),
          buf, SizeOf(Value.Value));
      end;
    SOT_NonBlock:
      begin
        FNonBlockMode := Value.Enabled;
        x := Ord(FNonBlockMode);
        synsock.IoctlSocket(FSocket, FIONBIO, x);
      end;
    SOT_RecvTimeout:
      begin
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_RCVTIMEO),
          buf, SizeOf(Value.Value));
        {$ELSE}
          {$IFDEF MSWINDOWS}
        buf := @Value.Value;
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_RCVTIMEO),
          buf, SizeOf(Value.Value));
          {$ELSE}
        timeval.tv_sec:=Value.Value div 1000;
        timeval.tv_usec:=(Value.Value mod 1000) * 1000;
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_RCVTIMEO),
          @timeval, SizeOf(timeval));
          {$ENDIF}
        {$ENDIF}
      end;
    SOT_SendTimeout:
      begin
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        {$ELSE}
          {$IFDEF MSWINDOWS}
        buf := @Value.Value;
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_SNDTIMEO),
          buf, SizeOf(Value.Value));
          {$ELSE}
        timeval.tv_sec:=Value.Value div 1000;
        timeval.tv_usec:=(Value.Value mod 1000) * 1000;
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_SNDTIMEO),
          @timeval, SizeOf(timeval));
          {$ENDIF}
        {$ENDIF}
      end;
    SOT_Reuse:
      begin
        x := Ord(Value.Enabled);
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(x);
        {$ELSE}
        buf := @x;
        {$ENDIF}
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_REUSEADDR), buf, SizeOf(x));
      end;
    SOT_TTL:
      begin
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        {$ELSE}
        buf := @Value.Value;
        {$ENDIF}
        if FIP6Used then
          synsock.SetSockOpt(FSocket, integer(IPPROTO_IPV6), integer(IPV6_UNICAST_HOPS),
            buf, SizeOf(Value.Value))
        else
          synsock.SetSockOpt(FSocket, integer(IPPROTO_IP), integer(IP_TTL),
            buf, SizeOf(Value.Value));
      end;
    SOT_Broadcast:
      begin
//#todo1 broadcasty na IP6
        x := Ord(Value.Enabled);
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(x);
        {$ELSE}
        buf := @x;
        {$ENDIF}
        synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_BROADCAST), buf, SizeOf(x));
      end;
    SOT_MulticastTTL:
      begin
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(value.Value);
        {$ELSE}
        buf := @Value.Value;
        {$ENDIF}
        if FIP6Used then
          synsock.SetSockOpt(FSocket, integer(IPPROTO_IPV6), integer(IPV6_MULTICAST_HOPS),
            buf, SizeOf(Value.Value))
        else
          synsock.SetSockOpt(FSocket, integer(IPPROTO_IP), integer(IP_MULTICAST_TTL),
            buf, SizeOf(Value.Value));
      end;
   SOT_MulticastLoop:
      begin
        x := Ord(Value.Enabled);
        {$IFDEF CIL}
        buf := System.BitConverter.GetBytes(x);
        {$ELSE}
        buf := @x;
        {$ENDIF}
        if FIP6Used then
          synsock.SetSockOpt(FSocket, integer(IPPROTO_IPV6), integer(IPV6_MULTICAST_LOOP), buf, SizeOf(x))
        else
          synsock.SetSockOpt(FSocket, integer(IPPROTO_IP), integer(IP_MULTICAST_LOOP), buf, SizeOf(x));
      end;
  end;
  Value.free;
end;

procedure TBlockSocket.DelayedOption(const Value: TSynaOption);
begin
  if FSocket = INVALID_SOCKET then
  begin
    FDelayedOptions.Insert(0, Value);
  end
  else
    SetDelayedOption(Value);
end;

procedure TBlockSocket.ProcessDelayedOptions;
var
  n: integer;
  d: TSynaOption;
begin
  for n := FDelayedOptions.Count - 1 downto 0 do
  begin
    d := TSynaOption(FDelayedOptions[n]);
    SetDelayedOption(d);
  end;
  FDelayedOptions.Clear;
end;

procedure TBlockSocket.SetSin(var Sin: TVarSin; IP, Port: string);
var
  f: TSocketFamily;
begin
  DoStatus(HR_ResolvingBegin, IP + ':' + Port);
  ResetLastError;
  //if socket exists, then use their type, else use users selection
  f := SF_Any;
  if (FSocket = INVALID_SOCKET) and (FFamily = SF_any) then
  begin
    if IsIP(IP) then
      f := SF_IP4
    else
      if IsIP6(IP) then
        f := SF_IP6;
  end
  else
    f := FFamily;
  FLastError := synsock.SetVarSin(sin, ip, port, FamilyToAF(f),
    GetSocketprotocol, GetSocketType, FPreferIP4);
  DoStatus(HR_ResolvingEnd, GetSinIP(sin) + ':' + IntTostr(GetSinPort(sin)));
end;

function TBlockSocket.GetSinIP(Sin: TVarSin): string;
begin
  Result := synsock.GetSinIP(sin);
end;

function TBlockSocket.GetSinPort(Sin: TVarSin): Integer;
begin
  Result := synsock.GetSinPort(sin);
end;

procedure TBlockSocket.CreateSocket;
var
  sin: TVarSin;
begin
  //dummy for SF_Any Family mode
  ResetLastError;
  if (FFamily <> SF_Any) and (FSocket = INVALID_SOCKET) then
  begin
    {$IFDEF CIL}
    if FFamily = SF_IP6 then
      sin := TVarSin.Create(IPAddress.Parse('::0'), 0)
    else
      sin := TVarSin.Create(IPAddress.Parse('0.0.0.0'), 0);
    {$ELSE}
    FillChar(Sin, Sizeof(Sin), 0);
    if FFamily = SF_IP6 then
      sin.sin_family := AF_INET6
    else
      sin.sin_family := AF_INET;
    {$ENDIF}
    InternalCreateSocket(Sin);
  end;
end;

procedure TBlockSocket.CreateSocketByName(const Value: String);
var
  sin: TVarSin;
begin
  ResetLastError;
  if FSocket = INVALID_SOCKET then
  begin
    SetSin(sin, value, '0');
    if FLastError = 0 then
      InternalCreateSocket(Sin);
  end;
end;

procedure TBlockSocket.InternalCreateSocket(Sin: TVarSin);
begin
  FStopFlag := False;
  FRecvCounter := 0;
  FSendCounter := 0;
  ResetLastError;
  if FSocket = INVALID_SOCKET then
  begin
    FBuffer := '';
    FBinded := False;
    FIP6Used := Sin.AddressFamily = AF_INET6;
    FSocket := synsock.Socket(integer(Sin.AddressFamily), GetSocketType, GetSocketProtocol);
    if FSocket = INVALID_SOCKET then
      FLastError := synsock.WSAGetLastError;
    {$IFNDEF CIL}
    FD_ZERO(FFDSet);
    FD_SET(FSocket, FFDSet);
    {$ENDIF}
    ExceptCheck;
    if FIP6used then
      DoStatus(HR_SocketCreate, 'IPv6')
    else
      DoStatus(HR_SocketCreate, 'IPv4');
    ProcessDelayedOptions;
    DoCreateSocket;
  end;
end;

procedure TBlockSocket.CloseSocket;
begin
  AbortSocket;
end;

procedure TBlockSocket.AbortSocket;
var
  n: integer;
  p: TSynaOption;
begin
  if FSocket <> INVALID_SOCKET then
    synsock.CloseSocket(FSocket);
  FSocket := INVALID_SOCKET;
  for n := FDelayedOptions.Count - 1 downto 0 do
    begin
      p := TSynaOption(FDelayedOptions[n]);
      p.Free;
    end;
  FDelayedOptions.Clear;
  FFamily := FFamilySave;
  DoStatus(HR_SocketClose, '');
end;

procedure TBlockSocket.Bind(IP, Port: string);
var
  Sin: TVarSin;
begin
  ResetLastError;
  if (FSocket <> INVALID_SOCKET)
    or not((FFamily = SF_ANY) and (IP = cAnyHost) and (Port = cAnyPort)) then
  begin
    SetSin(Sin, IP, Port);
    if FLastError = 0 then
    begin
      if FSocket = INVALID_SOCKET then
        InternalCreateSocket(Sin);
      SockCheck(synsock.Bind(FSocket, Sin));
      GetSinLocal;
      FBuffer := '';
      FBinded := True;
    end;
    ExceptCheck;
    DoStatus(HR_Bind, IP + ':' + Port);
  end;
end;

procedure TBlockSocket.Connect(IP, Port: string);
var
  Sin: TVarSin;
  b: boolean;
begin
  SetSin(Sin, IP, Port);
  if FLastError = 0 then
  begin
    if FSocket = INVALID_SOCKET then
      InternalCreateSocket(Sin);
    if FConnectionTimeout > 0 then
    begin
      // connect in non-blocking mode
      b := NonBlockMode;
      NonBlockMode := true;
      SockCheck(synsock.Connect(FSocket, Sin));
      if (FLastError = WSAEINPROGRESS) OR (FLastError = WSAEWOULDBLOCK) then
        if not CanWrite(FConnectionTimeout) then
          FLastError := WSAETIMEDOUT;
      NonBlockMode := b;
    end
    else
      SockCheck(synsock.Connect(FSocket, Sin));
    if FLastError = 0 then
      GetSins;
    FBuffer := '';
    FLastCR := False;
    FLastLF := False;
  end;
  ExceptCheck;
  DoStatus(HR_Connect, IP + ':' + Port);
end;

procedure TBlockSocket.Listen;
begin
  SockCheck(synsock.Listen(FSocket, SOMAXCONN));
  GetSins;
  ExceptCheck;
  DoStatus(HR_Listen, '');
end;

function TBlockSocket.Accept: TSocket;
begin
  Result := synsock.Accept(FSocket, FRemoteSin);
///    SockCheck(Result);
  ExceptCheck;
  DoStatus(HR_Accept, '');
end;

procedure TBlockSocket.GetSinLocal;
begin
  synsock.GetSockName(FSocket, FLocalSin);
end;

procedure TBlockSocket.GetSinRemote;
begin
  synsock.GetPeerName(FSocket, FRemoteSin);
end;

procedure TBlockSocket.GetSins;
begin
  GetSinLocal;
  GetSinRemote;
end;

procedure TBlockSocket.SetBandwidth(Value: Integer);
begin
  MaxSendBandwidth := Value;
  MaxRecvBandwidth := Value;
end;

procedure TBlockSocket.LimitBandwidth(Length: Integer; MaxB: integer; var Next: LongWord);
var
  x: LongWord;
  y: LongWord;
  n: integer;
begin
  if FStopFlag then
    exit;
  if MaxB > 0 then
  begin
    y := GetTick;
    if Next > y then
    begin
      x := Next - y;
      if x > 0 then
      begin
        DoStatus(HR_Wait, IntToStr(x));
        sleep(x mod 250);
        for n := 1 to x div 250 do
          if FStopFlag then
            Break
          else
            sleep(250);
      end;
    end;
    Next := GetTick + Trunc((Length / MaxB) * 1000);
  end;
end;

function TBlockSocket.TestStopFlag: Boolean;
begin
  DoHeartbeat;
  Result := FStopFlag;
  if Result then
  begin
    FStopFlag := False;
    FLastError := WSAECONNABORTED;
    ExceptCheck;
  end;
end;


function TBlockSocket.SendBuffer(Buffer: TMemory; Length: Integer): Integer;
{$IFNDEF CIL}
var
  x, y: integer;
  l, r: integer;
  p: Pointer;
{$ENDIF}
begin
  Result := 0;
  if TestStopFlag then
    Exit;
  DoMonitor(True, Buffer, Length);
{$IFDEF CIL}
  Result := synsock.Send(FSocket, Buffer, Length, 0);
{$ELSE}
  l := Length;
  x := 0;
  while x < l do
  begin
    y := l - x;
    if y > FSendMaxChunk then
      y := FSendMaxChunk;
    if y > 0 then
    begin
      LimitBandwidth(y, FMaxSendBandwidth, FNextsend);
      p := IncPoint(Buffer, x);
      r := synsock.Send(FSocket, p, y, MSG_NOSIGNAL);
      SockCheck(r);
      if FLastError = WSAEWOULDBLOCK then
      begin
        if CanWrite(FNonblockSendTimeout) then
        begin
          r := synsock.Send(FSocket, p, y, MSG_NOSIGNAL);
          SockCheck(r);
        end
        else
          FLastError := WSAETIMEDOUT;
      end;
      if FLastError <> 0 then
        Break;
      Inc(x, r);
      Inc(Result, r);
      Inc(FSendCounter, r);
      DoStatus(HR_WriteCount, IntToStr(r));
    end
    else
      break;
  end;
{$ENDIF}
  ExceptCheck;
end;

procedure TBlockSocket.SendByte(Data: Byte);
{$IFDEF CIL}
var
  buf: TMemory;
{$ENDIF}
begin
{$IFDEF CIL}
  setlength(buf, 1);
  buf[0] := Data;
  SendBuffer(buf, 1);
{$ELSE}
  SendBuffer(@Data, 1);
{$ENDIF}
end;

procedure TBlockSocket.SendString(Data: AnsiString);
var
  buf: TMemory;
begin
  {$IFDEF CIL}
  buf := BytesOf(Data);
  {$ELSE}
  buf := Pointer(data);
  {$ENDIF}
  SendBuffer(buf, Length(Data));
end;

procedure TBlockSocket.SendInteger(Data: integer);
var
  buf: TMemory;
begin
  {$IFDEF CIL}
  buf := System.BitConverter.GetBytes(Data);
  {$ELSE}
  buf := @Data;
  {$ENDIF}
  SendBuffer(buf, SizeOf(Data));
end;

procedure TBlockSocket.SendBlock(const Data: AnsiString);
var
  i: integer;
begin
  i := SwapBytes(Length(data));
  SendString(Codelongint(i) + Data);
end;

procedure TBlockSocket.InternalSendStream(const Stream: TStream; WithSize, Indy: boolean);
var
  l: integer;
  yr: integer;
  s: AnsiString;
  b: boolean;
{$IFDEF CIL}
  buf: TMemory;
{$ENDIF}
begin
  b := true;
  l := 0;
  if WithSize then
  begin
    l := Stream.Size - Stream.Position;;
    if not Indy then
      l := synsock.HToNL(l);
  end;
  repeat
    {$IFDEF CIL}
    Setlength(buf, FSendMaxChunk);
    yr := Stream.read(buf, FSendMaxChunk);
    if yr > 0 then
    begin
      if WithSize and b then
      begin
        b := false;
        SendString(CodeLongInt(l));
      end;
      SendBuffer(buf, yr);
      if FLastError <> 0 then
        break;
    end
    {$ELSE}
    Setlength(s, FSendMaxChunk);
    yr := Stream.read(Pointer(s)^, FSendMaxChunk);
    if yr > 0 then
    begin
      SetLength(s, yr);
      if WithSize and b then
      begin
        b := false;
        SendString(CodeLongInt(l) + s);
      end
      else
        SendString(s);
      if FLastError <> 0 then
        break;
    end
    {$ENDIF}
  until yr <= 0;
end;

procedure TBlockSocket.SendStreamRaw(const Stream: TStream);
begin
  InternalSendStream(Stream, false, false);
end;

procedure TBlockSocket.SendStreamIndy(const Stream: TStream);
begin
  InternalSendStream(Stream, true, true);
end;

procedure TBlockSocket.SendStream(const Stream: TStream);
begin
  InternalSendStream(Stream, true, false);
end;

function TBlockSocket.RecvBuffer(Buffer: TMemory; Length: Integer): Integer;
begin
  Result := 0;
  if TestStopFlag then
    Exit;
  LimitBandwidth(Length, FMaxRecvBandwidth, FNextRecv);
//  Result := synsock.Recv(FSocket, Buffer^, Length, MSG_NOSIGNAL);
  Result := synsock.Recv(FSocket, Buffer, Length, MSG_NOSIGNAL);
  if Result = 0 then
    FLastError := WSAECONNRESET
  else
    SockCheck(Result);
  ExceptCheck;
  if Result > 0 then
  begin
    Inc(FRecvCounter, Result);
    DoStatus(HR_ReadCount, IntToStr(Result));
    DoMonitor(False, Buffer, Result);
    DoReadFilter(Buffer, Result);
  end;
end;

function TBlockSocket.RecvBufferEx(Buffer: TMemory; Len: Integer;
  Timeout: Integer): Integer;
var
  s: AnsiString;
  rl, l: integer;
  ti: LongWord;
{$IFDEF CIL}
  n: integer;
  b: TMemory;
{$ENDIF}
begin
  ResetLastError;
  Result := 0;
  if Len > 0 then
  begin
    rl := 0;
    repeat
      ti := GetTick;
      s := RecvPacket(Timeout);
      l := Length(s);
      if (rl + l) > Len then
        l := Len - rl;
      {$IFDEF CIL}
      b := BytesOf(s);
      for n := 0 to l do
        Buffer[rl + n] := b[n];
      {$ELSE}
      Move(Pointer(s)^, IncPoint(Buffer, rl)^, l);
      {$ENDIF}
      rl := rl + l;
      if FLastError <> 0 then
        Break;
      if rl >= Len then
        Break;
      if not FInterPacketTimeout then
      begin
        Timeout := Timeout - integer(TickDelta(ti, GetTick));
        if Timeout <= 0 then
        begin
          FLastError := WSAETIMEDOUT;
          Break;
        end;
      end;
    until False;
    delete(s, 1, l);
    FBuffer := s;
    Result := rl;
  end;
end;

function TBlockSocket.RecvBufferStr(Len: Integer; Timeout: Integer): AnsiString;
var
  x: integer;
{$IFDEF CIL}
  buf: Tmemory;
{$ENDIF}
begin
  Result := '';
  if Len > 0 then
  begin
    {$IFDEF CIL}
    Setlength(Buf, Len);
    x := RecvBufferEx(buf, Len , Timeout);
    if FLastError = 0 then
    begin
      SetLength(Buf, x);
      Result := StringOf(buf);
    end
    else
      Result := '';
    {$ELSE}
    Setlength(Result, Len);
    x := RecvBufferEx(Pointer(Result), Len , Timeout);
    if FLastError = 0 then
      SetLength(Result, x)
    else
      Result := '';
    {$ENDIF}
  end;
end;

function TBlockSocket.RecvPacket(Timeout: Integer): AnsiString;
var
  x: integer;
{$IFDEF CIL}
  buf: TMemory;
{$ENDIF}
begin
  Result := '';
  ResetLastError;
  if FBuffer <> '' then
  begin
    Result := FBuffer;
    FBuffer := '';
  end
  else
  begin
    {$IFDEF MSWINDOWS}
    //not drain CPU on large downloads...
    Sleep(0);
    {$ENDIF}
    x := WaitingData;
    if x > 0 then
    begin
      {$IFDEF CIL}
      SetLength(Buf, x);
      x := RecvBuffer(Buf, x);
      if x >= 0 then
      begin
        SetLength(Buf, x);
        Result := StringOf(Buf);
      end;
      {$ELSE}
      SetLength(Result, x);
      x := RecvBuffer(Pointer(Result), x);
      if x >= 0 then
        SetLength(Result, x);
      {$ENDIF}
    end
    else
    begin
      if CanRead(Timeout) then
      begin
        x := WaitingData;
        if x = 0 then
          FLastError := WSAECONNRESET;
        if x > 0 then
        begin
          {$IFDEF CIL}
          SetLength(Buf, x);
          x := RecvBuffer(Buf, x);
          if x >= 0 then
          begin
            SetLength(Buf, x);
            result := StringOf(Buf);
          end;
          {$ELSE}
          SetLength(Result, x);
          x := RecvBuffer(Pointer(Result), x);
          if x >= 0 then
            SetLength(Result, x);
          {$ENDIF}
        end;
      end
      else
        FLastError := WSAETIMEDOUT;
    end;
  end;
  if FConvertLineEnd and (Result <> '') then
  begin
    if FLastCR and (Result[1] = LF) then
      Delete(Result, 1, 1);
    if FLastLF and (Result[1] = CR) then
      Delete(Result, 1, 1);
    FLastCR := False;
    FLastLF := False;
  end;
  ExceptCheck;
end;


function TBlockSocket.RecvByte(Timeout: Integer): Byte;
begin
  Result := 0;
  ResetLastError;
  if FBuffer = '' then
    FBuffer := RecvPacket(Timeout);
  if (FLastError = 0) and (FBuffer <> '') then
  begin
    Result := Ord(FBuffer[1]);
    Delete(FBuffer, 1, 1);
  end;
  ExceptCheck;
end;

function TBlockSocket.RecvInteger(Timeout: Integer): Integer;
var
  s: AnsiString;
begin
  Result := 0;
  s := RecvBufferStr(4, Timeout);
  if FLastError = 0 then
    Result := (ord(s[1]) + ord(s[2]) * 256) + (ord(s[3]) + ord(s[4]) * 256) * 65536;
end;

function TBlockSocket.RecvTerminated(Timeout: Integer; const Terminator: AnsiString): AnsiString;
var
  x: Integer;
  s: AnsiString;
  l: Integer;
  CorCRLF: Boolean;
  t: AnsiString;
  tl: integer;
  ti: LongWord;
begin
  ResetLastError;
  Result := '';
  l := Length(Terminator);
  if l = 0 then
    Exit;
  tl := l;
  CorCRLF := FConvertLineEnd and (Terminator = CRLF);
  s := '';
  x := 0;
  repeat
    //get rest of FBuffer or incomming new data...
    ti := GetTick;
    s := s + RecvPacket(Timeout);
    if FLastError <> 0 then
      Break;
    x := 0;
    if Length(s) > 0 then
      if CorCRLF then
      begin
        t := '';
        x := PosCRLF(s, t);
        tl := Length(t);
        if t = CR then
          FLastCR := True;
        if t = LF then
          FLastLF := True;
      end
      else
      begin
        x := pos(Terminator, s);
        tl := l;
      end;
    if (FMaxLineLength <> 0) and (Length(s) > FMaxLineLength) then
    begin
      FLastError := WSAENOBUFS;
      Break;
    end;
    if x > 0 then
      Break;
    if not FInterPacketTimeout then
    begin
      Timeout := Timeout - integer(TickDelta(ti, GetTick));
      if Timeout <= 0 then
      begin
        FLastError := WSAETIMEDOUT;
        Break;
      end;
    end;
  until False;
  if x > 0 then
  begin
    Result := Copy(s, 1, x - 1);
    Delete(s, 1, x + tl - 1);
  end;
  FBuffer := s;
  ExceptCheck;
end;

function TBlockSocket.RecvString(Timeout: Integer): AnsiString;
var
  s: AnsiString;
begin
  Result := '';
  s := RecvTerminated(Timeout, CRLF);
  if FLastError = 0 then
    Result := s;
end;

function TBlockSocket.RecvBlock(Timeout: Integer): AnsiString;
var
  x: integer;
begin
  Result := '';
  x := RecvInteger(Timeout);
  if FLastError = 0 then
    Result := RecvBufferStr(x, Timeout);
end;

procedure TBlockSocket.RecvStreamRaw(const Stream: TStream; Timeout: Integer);
var
  s: AnsiString;
begin
  repeat
    s := RecvPacket(Timeout);
    if FLastError = 0 then
      WriteStrToStream(Stream, s);
  until FLastError <> 0;
end;

procedure TBlockSocket.RecvStreamSize(const Stream: TStream; Timeout: Integer; Size: Integer);
var
  s: AnsiString;
  n: integer;
{$IFDEF CIL}
  buf: TMemory;
{$ENDIF}
begin
  for n := 1 to (Size div FSendMaxChunk) do
  begin
    {$IFDEF CIL}
    SetLength(buf, FSendMaxChunk);
    RecvBufferEx(buf, FSendMaxChunk, Timeout);
    if FLastError <> 0 then
      Exit;
    Stream.Write(buf, FSendMaxChunk);
    {$ELSE}
    s := RecvBufferStr(FSendMaxChunk, Timeout);
    if FLastError <> 0 then
      Exit;
    WriteStrToStream(Stream, s);
    {$ENDIF}
  end;
  n := Size mod FSendMaxChunk;
  if n > 0 then
  begin
    {$IFDEF CIL}
    SetLength(buf, n);
    RecvBufferEx(buf, n, Timeout);
    if FLastError <> 0 then
      Exit;
    Stream.Write(buf, n);
    {$ELSE}
    s := RecvBufferStr(n, Timeout);
    if FLastError <> 0 then
      Exit;
    WriteStrToStream(Stream, s);
    {$ENDIF}
  end;
end;

procedure TBlockSocket.RecvStreamIndy(const Stream: TStream; Timeout: Integer);
var
  x: integer;
begin
  x := RecvInteger(Timeout);
  x := synsock.NToHL(x);
  if FLastError = 0 then
    RecvStreamSize(Stream, Timeout, x);
end;

procedure TBlockSocket.RecvStream(const Stream: TStream; Timeout: Integer);
var
  x: integer;
begin
  x := RecvInteger(Timeout);
  if FLastError = 0 then
    RecvStreamSize(Stream, Timeout, x);
end;

function TBlockSocket.PeekBuffer(Buffer: TMemory; Length: Integer): Integer;
begin
 {$IFNDEF CIL}
//  Result := synsock.Recv(FSocket, Buffer^, Length, MSG_PEEK + MSG_NOSIGNAL);
  Result := synsock.Recv(FSocket, Buffer, Length, MSG_PEEK + MSG_NOSIGNAL);
  SockCheck(Result);
  ExceptCheck;
  {$ENDIF}
end;

function TBlockSocket.PeekByte(Timeout: Integer): Byte;
var
  s: string;
begin
 {$IFNDEF CIL}
  Result := 0;
  if CanRead(Timeout) then
  begin
    SetLength(s, 1);
    PeekBuffer(Pointer(s), 1);
    if s <> '' then
      Result := Ord(s[1]);
  end
  else
    FLastError := WSAETIMEDOUT;
  ExceptCheck;
  {$ENDIF}
end;

procedure TBlockSocket.ResetLastError;
begin
  FLastError := 0;
  FLastErrorDesc := '';
end;

function TBlockSocket.SockCheck(SockResult: Integer): Integer;
begin
  ResetLastError;
  if SockResult = integer(SOCKET_ERROR) then
  begin
    FLastError := synsock.WSAGetLastError;
    FLastErrorDesc := GetErrorDescEx;
  end;
  Result := FLastError;
end;

procedure TBlockSocket.ExceptCheck;
var
  e: ESynapseError;
begin
  FLastErrorDesc := GetErrorDescEx;
  if (LastError <> 0) and (LastError <> WSAEINPROGRESS)
    and (LastError <> WSAEWOULDBLOCK) then
  begin
    DoStatus(HR_Error, IntToStr(FLastError) + ',' + FLastErrorDesc);
    if FRaiseExcept then
    begin
      e := ESynapseError.Create(Format('Synapse TCP/IP Socket error %d: %s',
        [FLastError, FLastErrorDesc]));
      e.ErrorCode := FLastError;
      e.ErrorMessage := FLastErrorDesc;
      raise e;
    end;
  end;
end;

function TBlockSocket.WaitingData: Integer;
var
  x: Integer;
begin
  Result := 0;
  if synsock.IoctlSocket(FSocket, FIONREAD, x) = 0 then
    Result := x;
  if Result > c64k then
    Result := c64k;
end;

function TBlockSocket.WaitingDataEx: Integer;
begin
  if FBuffer <> '' then
    Result := Length(FBuffer)
  else
    Result := WaitingData;
end;

procedure TBlockSocket.Purge;
begin
  Sleep(1);
  try
    while (Length(FBuffer) > 0) or (WaitingData > 0) do
    begin
      RecvPacket(0);
      if FLastError <> 0 then
        break;
    end;
  except
    on exception do;
  end;
  ResetLastError;
end;

procedure TBlockSocket.SetLinger(Enable: Boolean; Linger: Integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_Linger;
  d.Enabled := Enable;
  d.Value := Linger;
  DelayedOption(d);
end;

function TBlockSocket.LocalName: string;
begin
  Result := synsock.GetHostName;
  if Result = '' then
    Result := '127.0.0.1';
end;

procedure TBlockSocket.ResolveNameToIP(Name: string; const IPList: TStrings);
begin
  IPList.Clear;
  synsock.ResolveNameToIP(Name, FamilyToAF(FFamily), GetSocketprotocol, GetSocketType, IPList);
  if IPList.Count = 0 then
    IPList.Add(cAnyHost);
end;

function TBlockSocket.ResolveName(Name: string): string;
var
  l: TStringList;
begin
  l := TStringList.Create;
  try
    ResolveNameToIP(Name, l);
    Result := l[0];
  finally
    l.Free;
  end;
end;

function TBlockSocket.ResolvePort(Port: string): Word;
begin
  Result := synsock.ResolvePort(Port, FamilyToAF(FFamily), GetSocketProtocol, GetSocketType);
end;

function TBlockSocket.ResolveIPToName(IP: string): string;
begin
  if not IsIP(IP) and not IsIp6(IP) then
    IP := ResolveName(IP);
  Result := synsock.ResolveIPToName(IP, FamilyToAF(FFamily), GetSocketProtocol, GetSocketType);
end;

procedure TBlockSocket.SetRemoteSin(IP, Port: string);
begin
  SetSin(FRemoteSin, IP, Port);
end;

function TBlockSocket.GetLocalSinIP: string;
begin
  Result := GetSinIP(FLocalSin);
end;

function TBlockSocket.GetRemoteSinIP: string;
begin
  Result := GetSinIP(FRemoteSin);
end;

function TBlockSocket.GetLocalSinPort: Integer;
begin
  Result := GetSinPort(FLocalSin);
end;

function TBlockSocket.GetRemoteSinPort: Integer;
begin
  Result := GetSinPort(FRemoteSin);
end;

function TBlockSocket.InternalCanRead(Timeout: Integer): Boolean;
{$IFDEF CIL}
begin
  Result := FSocket.Poll(Timeout * 1000, SelectMode.SelectRead);
{$ELSE}
var
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x: Integer;
  FDSet: TFDSet;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  FDSet := FFdSet;
  x := synsock.Select(FSocket + 1, @FDSet, nil, nil, TimeVal);
  SockCheck(x);
  if FLastError <> 0 then
    x := 0;
  Result := x > 0;
{$ENDIF}
end;

function TBlockSocket.CanRead(Timeout: Integer): Boolean;
var
  ti, tr: Integer;
  n: integer;
begin
  if (FHeartbeatRate <> 0) and (Timeout <> -1) then
  begin
    ti := Timeout div FHeartbeatRate;
    tr := Timeout mod FHeartbeatRate;
  end
  else
  begin
    ti := 0;
    tr := Timeout;
  end;
  Result := InternalCanRead(tr);
  if not Result then
    for n := 0 to ti do
    begin
      DoHeartbeat;
      if FStopFlag then
      begin
        Result := False;
        FStopFlag := False;
        Break;
      end;
      Result := InternalCanRead(FHeartbeatRate);
      if Result then
        break;
    end;
  ExceptCheck;
  if Result then
    DoStatus(HR_CanRead, '');
end;

function TBlockSocket.CanWrite(Timeout: Integer): Boolean;
{$IFDEF CIL}
begin
  Result := FSocket.Poll(Timeout * 1000, SelectMode.SelectWrite);
{$ELSE}
var
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x: Integer;
  FDSet: TFDSet;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  FDSet := FFdSet;
  x := synsock.Select(FSocket + 1, nil, @FDSet, nil, TimeVal);
  SockCheck(x);
  if FLastError <> 0 then
    x := 0;
  Result := x > 0;
{$ENDIF}
  ExceptCheck;
  if Result then
    DoStatus(HR_CanWrite, '');
end;

function TBlockSocket.CanReadEx(Timeout: Integer): Boolean;
begin
  if FBuffer <> '' then
    Result := True
  else
    Result := CanRead(Timeout);
end;

function TBlockSocket.SendBufferTo(Buffer: TMemory; Length: Integer): Integer;
begin
  Result := 0;
  if TestStopFlag then
    Exit;
  DoMonitor(True, Buffer, Length);
  LimitBandwidth(Length, FMaxSendBandwidth, FNextsend);
  Result := synsock.SendTo(FSocket, Buffer, Length, MSG_NOSIGNAL, FRemoteSin);
  SockCheck(Result);
  ExceptCheck;
  Inc(FSendCounter, Result);
  DoStatus(HR_WriteCount, IntToStr(Result));
end;

function TBlockSocket.RecvBufferFrom(Buffer: TMemory; Length: Integer): Integer;
begin
  Result := 0;
  if TestStopFlag then
    Exit;
  LimitBandwidth(Length, FMaxRecvBandwidth, FNextRecv);
  Result := synsock.RecvFrom(FSocket, Buffer, Length, MSG_NOSIGNAL, FRemoteSin);
  SockCheck(Result);
  ExceptCheck;
  Inc(FRecvCounter, Result);
  DoStatus(HR_ReadCount, IntToStr(Result));
  DoMonitor(False, Buffer, Result);
end;

function TBlockSocket.GetSizeRecvBuffer: Integer;
var
  l: Integer;
{$IFDEF CIL}
  buf: TMemory;
{$ENDIF}
begin
{$IFDEF CIL}
  setlength(buf, 4);
  SockCheck(synsock.GetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_RCVBUF), buf, l));
  Result := System.BitConverter.ToInt32(buf,0);
{$ELSE}
  l := SizeOf(Result);
  SockCheck(synsock.GetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, @Result, l));
  if FLastError <> 0 then
    Result := 1024;
  ExceptCheck;
{$ENDIF}
end;

procedure TBlockSocket.SetSizeRecvBuffer(Size: Integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_RecvBuff;
  d.Value := Size;
  DelayedOption(d);
end;

function TBlockSocket.GetSizeSendBuffer: Integer;
var
  l: Integer;
{$IFDEF CIL}
  buf: TMemory;
{$ENDIF}
begin
{$IFDEF CIL}
  setlength(buf, 4);
  SockCheck(synsock.GetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_SNDBUF), buf, l));
  Result := System.BitConverter.ToInt32(buf,0);
{$ELSE}
  l := SizeOf(Result);
  SockCheck(synsock.GetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, @Result, l));
  if FLastError <> 0 then
    Result := 1024;
  ExceptCheck;
{$ENDIF}
end;

procedure TBlockSocket.SetSizeSendBuffer(Size: Integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_SendBuff;
  d.Value := Size;
  DelayedOption(d);
end;

procedure TBlockSocket.SetNonBlockMode(Value: Boolean);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_nonblock;
  d.Enabled := Value;
  DelayedOption(d);
end;

procedure TBlockSocket.SetTimeout(Timeout: Integer);
begin
  SetSendTimeout(Timeout);
  SetRecvTimeout(Timeout);
end;

procedure TBlockSocket.SetSendTimeout(Timeout: Integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_sendtimeout;
  d.Value := Timeout;
  DelayedOption(d);
end;

procedure TBlockSocket.SetRecvTimeout(Timeout: Integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_recvtimeout;
  d.Value := Timeout;
  DelayedOption(d);
end;

{$IFNDEF CIL}
function TBlockSocket.GroupCanRead(const SocketList: TList; Timeout: Integer;
  const CanReadList: TList): boolean;
var
  FDSet: TFDSet;
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x, n: Integer;
  Max: Integer;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  FD_ZERO(FDSet);
  Max := 0;
  for n := 0 to SocketList.Count - 1 do
    if TObject(SocketList.Items[n]) is TBlockSocket then
    begin
      if TBlockSocket(SocketList.Items[n]).Socket > Max then
        Max := TBlockSocket(SocketList.Items[n]).Socket;
      FD_SET(TBlockSocket(SocketList.Items[n]).Socket, FDSet);
    end;
  x := synsock.Select(Max + 1, @FDSet, nil, nil, TimeVal);
  SockCheck(x);
  ExceptCheck;
  if FLastError <> 0 then
    x := 0;
  Result := x > 0;
  CanReadList.Clear;
  if Result then
    for n := 0 to SocketList.Count - 1 do
      if TObject(SocketList.Items[n]) is TBlockSocket then
        if FD_ISSET(TBlockSocket(SocketList.Items[n]).Socket, FDSet) then
          CanReadList.Add(TBlockSocket(SocketList.Items[n]));
end;
{$ENDIF}

procedure TBlockSocket.EnableReuse(Value: Boolean);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_reuse;
  d.Enabled := Value;
  DelayedOption(d);
end;

procedure TBlockSocket.SetTTL(TTL: integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_TTL;
  d.Value := TTL;
  DelayedOption(d);
end;

function TBlockSocket.GetTTL:integer;
var
  l: Integer;
begin
{$IFNDEF CIL}
  l := SizeOf(Result);
  if FIP6Used then
    synsock.GetSockOpt(FSocket, IPPROTO_IPV6, IPV6_UNICAST_HOPS, @Result, l)
  else
    synsock.GetSockOpt(FSocket, IPPROTO_IP, IP_TTL, @Result, l);
{$ENDIF}
end;

procedure TBlockSocket.SetFamily(Value: TSocketFamily);
begin
  FFamily := Value;
  FFamilySave := Value;
end;

procedure TBlockSocket.SetSocket(Value: TSocket);
begin
  FRecvCounter := 0;
  FSendCounter := 0;
  FSocket := Value;
{$IFNDEF CIL}
  FD_ZERO(FFDSet);
  FD_SET(FSocket, FFDSet);
{$ENDIF}
  GetSins;
  FIP6Used := FRemoteSin.AddressFamily = AF_INET6;
end;

function TBlockSocket.GetWsaData: TWSAData;
begin
  {$IFDEF ONCEWINSOCK}
  Result := WsaDataOnce;
  {$ELSE}
  Result := FWsaDataOnce;
  {$ENDIF}
end;

function TBlockSocket.GetSocketType: integer;
begin
  Result := 0;
end;

function TBlockSocket.GetSocketProtocol: integer;
begin
  Result := integer(IPPROTO_IP);
end;

procedure TBlockSocket.DoStatus(Reason: THookSocketReason; const Value: string);
begin
  if assigned(OnStatus) then
    OnStatus(Self, Reason, Value);
end;

procedure TBlockSocket.DoReadFilter(Buffer: TMemory; var Len: Integer);
var
  s: AnsiString;
begin
  if assigned(OnReadFilter) then
    if Len > 0 then
      begin
        {$IFDEF CIL}
        s := StringOf(Buffer);
        {$ELSE}
        SetLength(s, Len);
        Move(Buffer^, Pointer(s)^, Len);
        {$ENDIF}
        OnReadFilter(Self, s);
        if Length(s) > Len then
          SetLength(s, Len);
        Len := Length(s);
        {$IFDEF CIL}
        Buffer := BytesOf(s);
        {$ELSE}
        Move(Pointer(s)^, Buffer^, Len);
        {$ENDIF}
      end;
end;

procedure TBlockSocket.DoCreateSocket;
begin
  if assigned(OnCreateSocket) then
    OnCreateSocket(Self);
end;

procedure TBlockSocket.DoMonitor(Writing: Boolean; const Buffer: TMemory; Len: Integer);
begin
  if assigned(OnMonitor) then
  begin
    OnMonitor(Self, Writing, Buffer, Len);
  end;
end;

procedure TBlockSocket.DoHeartbeat;
begin
  if assigned(OnHeartbeat) and (FHeartbeatRate <> 0) then
  begin
    OnHeartbeat(Self);
  end;
end;

function TBlockSocket.GetErrorDescEx: string;
begin
  Result := GetErrorDesc(FLastError);
end;

class function TBlockSocket.GetErrorDesc(ErrorCode: Integer): string;
begin
{$IFDEF CIL}
  if ErrorCode = 0 then
    Result := ''
  else
  begin
    Result := WSAGetLastErrorDesc;
    if Result = '' then
      Result := 'Other Winsock error (' + IntToStr(ErrorCode) + ')';
  end;
{$ELSE}
  case ErrorCode of
    0:
      Result := '';
    WSAEINTR: {10004}
      Result := 'Interrupted system call';
    WSAEBADF: {10009}
      Result := 'Bad file number';
    WSAEACCES: {10013}
      Result := 'Permission denied';
    WSAEFAULT: {10014}
      Result := 'Bad address';
    WSAEINVAL: {10022}
      Result := 'Invalid argument';
    WSAEMFILE: {10024}
      Result := 'Too many open files';
    WSAEWOULDBLOCK: {10035}
      Result := 'Operation would block';
    WSAEINPROGRESS: {10036}
      Result := 'Operation now in progress';
    WSAEALREADY: {10037}
      Result := 'Operation already in progress';
    WSAENOTSOCK: {10038}
      Result := 'Socket operation on nonsocket';
    WSAEDESTADDRREQ: {10039}
      Result := 'Destination address required';
    WSAEMSGSIZE: {10040}
      Result := 'Message too long';
    WSAEPROTOTYPE: {10041}
      Result := 'Protocol wrong type for Socket';
    WSAENOPROTOOPT: {10042}
      Result := 'Protocol not available';
    WSAEPROTONOSUPPORT: {10043}
      Result := 'Protocol not supported';
    WSAESOCKTNOSUPPORT: {10044}
      Result := 'Socket not supported';
    WSAEOPNOTSUPP: {10045}
      Result := 'Operation not supported on Socket';
    WSAEPFNOSUPPORT: {10046}
      Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT: {10047}
      Result := 'Address family not supported';
    WSAEADDRINUSE: {10048}
      Result := 'Address already in use';
    WSAEADDRNOTAVAIL: {10049}
      Result := 'Can''t assign requested address';
    WSAENETDOWN: {10050}
      Result := 'Network is down';
    WSAENETUNREACH: {10051}
      Result := 'Network is unreachable';
    WSAENETRESET: {10052}
      Result := 'Network dropped connection on reset';
    WSAECONNABORTED: {10053}
      Result := 'Software caused connection abort';
    WSAECONNRESET: {10054}
      Result := 'Connection reset by peer';
    WSAENOBUFS: {10055}
      Result := 'No Buffer space available';
    WSAEISCONN: {10056}
      Result := 'Socket is already connected';
    WSAENOTCONN: {10057}
      Result := 'Socket is not connected';
    WSAESHUTDOWN: {10058}
      Result := 'Can''t send after Socket shutdown';
    WSAETOOMANYREFS: {10059}
      Result := 'Too many references:can''t splice';
    WSAETIMEDOUT: {10060}
      Result := 'Connection timed out';
    WSAECONNREFUSED: {10061}
      Result := 'Connection refused';
    WSAELOOP: {10062}
      Result := 'Too many levels of symbolic links';
    WSAENAMETOOLONG: {10063}
      Result := 'File name is too long';
    WSAEHOSTDOWN: {10064}
      Result := 'Host is down';
    WSAEHOSTUNREACH: {10065}
      Result := 'No route to host';
    WSAENOTEMPTY: {10066}
      Result := 'Directory is not empty';
    WSAEPROCLIM: {10067}
      Result := 'Too many processes';
    WSAEUSERS: {10068}
      Result := 'Too many users';
    WSAEDQUOT: {10069}
      Result := 'Disk quota exceeded';
    WSAESTALE: {10070}
      Result := 'Stale NFS file handle';
    WSAEREMOTE: {10071}
      Result := 'Too many levels of remote in path';
    WSASYSNOTREADY: {10091}
      Result := 'Network subsystem is unusable';
    WSAVERNOTSUPPORTED: {10092}
      Result := 'Winsock DLL cannot support this application';
    WSANOTINITIALISED: {10093}
      Result := 'Winsock not initialized';
    WSAEDISCON: {10101}
      Result := 'Disconnect';
    WSAHOST_NOT_FOUND: {11001}
      Result := 'Host not found';
    WSATRY_AGAIN: {11002}
      Result := 'Non authoritative - host not found';
    WSANO_RECOVERY: {11003}
      Result := 'Non recoverable error';
    WSANO_DATA: {11004}
      Result := 'Valid name, no data record of requested type'
  else
    Result := 'Other Winsock error (' + IntToStr(ErrorCode) + ')';
  end;
{$ENDIF}
end;

{======================================================================}

constructor TSocksBlockSocket.Create;
begin
  inherited Create;
  FSocksIP:= '';
  FSocksPort:= '1080';
  FSocksTimeout:= 60000;
  FSocksUsername:= '';
  FSocksPassword:= '';
  FUsingSocks := False;
  FSocksResolver := True;
  FSocksLastError := 0;
  FSocksResponseIP := '';
  FSocksResponsePort := '';
  FSocksLocalIP := '';
  FSocksLocalPort := '';
  FSocksRemoteIP := '';
  FSocksRemotePort := '';
  FBypassFlag := False;
  FSocksType := ST_Socks5;
end;

function TSocksBlockSocket.SocksOpen: boolean;
var
  Buf: AnsiString;
  n: integer;
begin
  Result := False;
  FUsingSocks := False;
  if FSocksType <> ST_Socks5 then
  begin
    FUsingSocks := True;
    Result := True;
  end
  else
  begin
    FBypassFlag := True;
    try
      if FSocksUsername = '' then
        Buf := #5 + #1 + #0
      else
        Buf := #5 + #2 + #2 +#0;
      SendString(Buf);
      Buf := RecvBufferStr(2, FSocksTimeout);
      if Length(Buf) < 2 then
        Exit;
      if Buf[1] <> #5 then
        Exit;
      n := Ord(Buf[2]);
      case n of
        0: //not need authorisation
          ;
        2:
          begin
            Buf := #1 + AnsiChar(Length(FSocksUsername)) + FSocksUsername
              + AnsiChar(Length(FSocksPassword)) + FSocksPassword;
            SendString(Buf);
            Buf := RecvBufferStr(2, FSocksTimeout);
            if Length(Buf) < 2 then
              Exit;
            if Buf[2] <> #0 then
              Exit;
          end;
      else
        //other authorisation is not supported!
        Exit;
      end;
      FUsingSocks := True;
      Result := True;
    finally
      FBypassFlag := False;
    end;
  end;
end;

function TSocksBlockSocket.SocksRequest(Cmd: Byte;
  const IP, Port: string): Boolean;
var
  Buf: AnsiString;
begin
  FBypassFlag := True;
  try
    if FSocksType <> ST_Socks5 then
      Buf := #4 + AnsiChar(Cmd) + SocksCode(IP, Port)
    else
      Buf := #5 + AnsiChar(Cmd) + #0 + SocksCode(IP, Port);
    SendString(Buf);
    Result := FLastError = 0;
  finally
    FBypassFlag := False;
  end;
end;

function TSocksBlockSocket.SocksResponse: Boolean;
var
  Buf, s: AnsiString;
  x: integer;
begin
  Result := False;
  FBypassFlag := True;
  try
    FSocksResponseIP := '';
    FSocksResponsePort := '';
    FSocksLastError := -1;
    if FSocksType <> ST_Socks5 then
    begin
      Buf := RecvBufferStr(8, FSocksTimeout);
      if FLastError <> 0 then
        Exit;
      if Buf[1] <> #0 then
        Exit;
      FSocksLastError := Ord(Buf[2]);
    end
    else
    begin
      Buf := RecvBufferStr(4, FSocksTimeout);
      if FLastError <> 0 then
        Exit;
      if Buf[1] <> #5 then
        Exit;
      case Ord(Buf[4]) of
        1:
          s := RecvBufferStr(4, FSocksTimeout);
        3:
          begin
            x := RecvByte(FSocksTimeout);
            if FLastError <> 0 then
              Exit;
            s := AnsiChar(x) + RecvBufferStr(x, FSocksTimeout);
          end;
        4:
          s := RecvBufferStr(16, FSocksTimeout);
      else
        Exit;
      end;
      Buf := Buf + s + RecvBufferStr(2, FSocksTimeout);
      if FLastError <> 0 then
        Exit;
      FSocksLastError := Ord(Buf[2]);
    end;
    if ((FSocksLastError <> 0) and (FSocksLastError <> 90)) then
      Exit;
    SocksDecode(Buf);
    Result := True;
  finally
    FBypassFlag := False;
  end;
end;

function TSocksBlockSocket.SocksCode(IP, Port: string): Ansistring;
var
  ip6: TIp6Bytes;
  n: integer;
begin
  if FSocksType <> ST_Socks5 then
  begin
    Result := CodeInt(ResolvePort(Port));
    if not FSocksResolver then
      IP := ResolveName(IP);
    if IsIP(IP) then
    begin
      Result := Result + IPToID(IP);
      Result := Result + FSocksUsername + #0;
    end
    else
    begin
      Result := Result + IPToID('0.0.0.1');
      Result := Result + FSocksUsername + #0;
      Result := Result + IP + #0;
    end;
  end
  else
  begin
    if not FSocksResolver then
      IP := ResolveName(IP);
    if IsIP(IP) then
      Result := #1 + IPToID(IP)
    else
      if IsIP6(IP) then
      begin
        ip6 := StrToIP6(IP);
        Result := #4;
        for n := 0 to 15 do
          Result := Result + AnsiChar(ip6[n]);
      end
      else
        Result := #3 + AnsiChar(Length(IP)) + IP;
    Result := Result + CodeInt(ResolvePort(Port));
  end;
end;

function TSocksBlockSocket.SocksDecode(Value: Ansistring): integer;
var
  Atyp: Byte;
  y, n: integer;
  w: Word;
  ip6: TIp6Bytes;
begin
  FSocksResponsePort := '0';
  Result := 0;
  if FSocksType <> ST_Socks5 then
  begin
    if Length(Value) < 8 then
      Exit;
    Result := 3;
    w := DecodeInt(Value, Result);
    FSocksResponsePort := IntToStr(w);
    FSocksResponseIP := Format('%d.%d.%d.%d',
      [Ord(Value[5]), Ord(Value[6]), Ord(Value[7]), Ord(Value[8])]);
    Result := 9;
  end
  else
  begin
    if Length(Value) < 4 then
      Exit;
    Atyp := Ord(Value[4]);
    Result := 5;
    case Atyp of
      1:
        begin
          if Length(Value) < 10 then
            Exit;
          FSocksResponseIP := Format('%d.%d.%d.%d',
              [Ord(Value[5]), Ord(Value[6]), Ord(Value[7]), Ord(Value[8])]);
          Result := 9;
        end;
      3:
        begin
          y := Ord(Value[5]);
          if Length(Value) < (5 + y + 2) then
            Exit;
          for n := 6 to 6 + y - 1 do
            FSocksResponseIP := FSocksResponseIP + Value[n];
          Result := 5 + y + 1;
        end;
      4:
        begin
          if Length(Value) < 22 then
            Exit;
          for n := 0 to 15 do
            ip6[n] := ord(Value[n + 5]);
          FSocksResponseIP := IP6ToStr(ip6);
          Result := 21;
        end;
    else
      Exit;
    end;
    w := DecodeInt(Value, Result);
    FSocksResponsePort := IntToStr(w);
    Result := Result + 2;
  end;
end;

{======================================================================}

procedure TDgramBlockSocket.Connect(IP, Port: string);
begin
  SetRemoteSin(IP, Port);
  InternalCreateSocket(FRemoteSin);
  FBuffer := '';
  DoStatus(HR_Connect, IP + ':' + Port);
end;

function TDgramBlockSocket.RecvBuffer(Buffer: TMemory; Length: Integer): Integer;
begin
  Result := RecvBufferFrom(Buffer, Length);
end;

function TDgramBlockSocket.SendBuffer(Buffer: TMemory; Length: Integer): Integer;
begin
  Result := SendBufferTo(Buffer, Length);
end;

{======================================================================}

destructor TUDPBlockSocket.Destroy;
begin
  if Assigned(FSocksControlSock) then
    FSocksControlSock.Free;
  inherited;
end;

procedure TUDPBlockSocket.EnableBroadcast(Value: Boolean);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_Broadcast;
  d.Enabled := Value;
  DelayedOption(d);
end;

function TUDPBlockSocket.UdpAssociation: Boolean;
var
  b: Boolean;
begin
  Result := True;
  FUsingSocks := False;
  if FSocksIP <> '' then
  begin
    Result := False;
    if not Assigned(FSocksControlSock) then
      FSocksControlSock := TTCPBlockSocket.Create;
    FSocksControlSock.CloseSocket;
    FSocksControlSock.CreateSocketByName(FSocksIP);
    FSocksControlSock.Connect(FSocksIP, FSocksPort);
    if FSocksControlSock.LastError <> 0 then
      Exit;
    // if not assigned local port, assign it!
    if not FBinded then
      Bind(cAnyHost, cAnyPort);
    //open control TCP connection to SOCKS
    FSocksControlSock.FSocksUsername := FSocksUsername;
    FSocksControlSock.FSocksPassword := FSocksPassword;
    b := FSocksControlSock.SocksOpen;
    if b then
      b := FSocksControlSock.SocksRequest(3, GetLocalSinIP, IntToStr(GetLocalSinPort));
    if b then
      b := FSocksControlSock.SocksResponse;
    if not b and (FLastError = 0) then
      FLastError := WSANO_RECOVERY;
    FUsingSocks :=FSocksControlSock.UsingSocks;
    FSocksRemoteIP := FSocksControlSock.FSocksResponseIP;
    FSocksRemotePort := FSocksControlSock.FSocksResponsePort;
    Result := b and (FLastError = 0);
  end;
end;

function TUDPBlockSocket.SendBufferTo(Buffer: TMemory; Length: Integer): Integer;
var
  SIp: string;
  SPort: integer;
  Buf: Ansistring;
begin
  Result := 0;
  FUsingSocks := False;
  if (FSocksIP <> '') and (not UdpAssociation) then
    FLastError := WSANO_RECOVERY
  else
  begin
    if FUsingSocks then
    begin
{$IFNDEF CIL}
      Sip := GetRemoteSinIp;
      SPort := GetRemoteSinPort;
      SetRemoteSin(FSocksRemoteIP, FSocksRemotePort);
      SetLength(Buf,Length);
      Move(Buffer^, Pointer(Buf)^, Length);
      Buf := #0 + #0 + #0 + SocksCode(Sip, IntToStr(SPort)) + Buf;
      Result := inherited SendBufferTo(Pointer(Buf), System.Length(buf));
      SetRemoteSin(Sip, IntToStr(SPort));
{$ENDIF}
    end
    else
      Result := inherited SendBufferTo(Buffer, Length);
  end;
end;

function TUDPBlockSocket.RecvBufferFrom(Buffer: TMemory; Length: Integer): Integer;
var
  Buf: Ansistring;
  x: integer;
begin
  Result := inherited RecvBufferFrom(Buffer, Length);
  if FUsingSocks then
  begin
{$IFNDEF CIL}
    SetLength(Buf, Result);
    Move(Buffer^, Pointer(Buf)^, Result);
    x := SocksDecode(Buf);
    Result := Result - x + 1;
    Buf := Copy(Buf, x, Result);
    Move(Pointer(Buf)^, Buffer^, Result);
    SetRemoteSin(FSocksResponseIP, FSocksResponsePort);
{$ENDIF}
  end;
end;

{$IFNDEF CIL}
procedure TUDPBlockSocket.AddMulticast(MCastIP: string);
var
  Multicast: TIP_mreq;
  Multicast6: TIPv6_mreq;
  n: integer;
  ip6: Tip6bytes;
begin
  if FIP6Used then
  begin
    ip6 := StrToIp6(MCastIP);
    for n := 0 to 15 do
      Multicast6.ipv6mr_multiaddr.u6_addr8[n] := Ip6[n];
    Multicast6.ipv6mr_interface := 0;
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IPV6, IPV6_JOIN_GROUP,
      PAnsiChar(@Multicast6), SizeOf(Multicast6)));
  end
  else
  begin
    Multicast.imr_multiaddr.S_addr := swapbytes(strtoip(MCastIP));
    Multicast.imr_interface.S_addr := INADDR_ANY;
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_ADD_MEMBERSHIP,
      PAnsiChar(@Multicast), SizeOf(Multicast)));
  end;
  ExceptCheck;
end;

procedure TUDPBlockSocket.DropMulticast(MCastIP: string);
var
  Multicast: TIP_mreq;
  Multicast6: TIPv6_mreq;
  n: integer;
  ip6: Tip6bytes;
begin
  if FIP6Used then
  begin
    ip6 := StrToIp6(MCastIP);
    for n := 0 to 15 do
      Multicast6.ipv6mr_multiaddr.u6_addr8[n] := Ip6[n];
    Multicast6.ipv6mr_interface := 0;
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IPV6, IPV6_LEAVE_GROUP,
      PAnsiChar(@Multicast6), SizeOf(Multicast6)));
  end
  else
  begin
    Multicast.imr_multiaddr.S_addr := swapbytes(strtoip(MCastIP));
    Multicast.imr_interface.S_addr := INADDR_ANY;
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_DROP_MEMBERSHIP,
      PAnsiChar(@Multicast), SizeOf(Multicast)));
  end;
  ExceptCheck;
end;
{$ENDIF}

procedure TUDPBlockSocket.SetMulticastTTL(TTL: integer);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_MulticastTTL;
  d.Value := TTL;
  DelayedOption(d);
end;

function TUDPBlockSocket.GetMulticastTTL:integer;
var
  l: Integer;
begin
{$IFNDEF CIL}
  l := SizeOf(Result);
  if FIP6Used then
    synsock.GetSockOpt(FSocket, IPPROTO_IPV6, IPV6_MULTICAST_HOPS, @Result, l)
  else
    synsock.GetSockOpt(FSocket, IPPROTO_IP, IP_MULTICAST_TTL, @Result, l);
{$ENDIF}
end;

procedure TUDPBlockSocket.EnableMulticastLoop(Value: Boolean);
var
  d: TSynaOption;
begin
  d := TSynaOption.Create;
  d.Option := SOT_MulticastLoop;
  d.Enabled := Value;
  DelayedOption(d);
end;

function TUDPBlockSocket.GetSocketType: integer;
begin
  Result := integer(SOCK_DGRAM);
end;

function TUDPBlockSocket.GetSocketProtocol: integer;
begin
 Result := integer(IPPROTO_UDP);
end;

{======================================================================}
constructor TTCPBlockSocket.CreateWithSSL(SSLPlugin: TSSLClass);
begin
  inherited Create;
  FSSL := SSLPlugin.Create(self);
  FHTTPTunnelIP := '';
  FHTTPTunnelPort := '';
  FHTTPTunnel := False;
  FHTTPTunnelRemoteIP := '';
  FHTTPTunnelRemotePort := '';
  FHTTPTunnelUser := '';
  FHTTPTunnelPass := '';
  FHTTPTunnelTimeout := 30000;
end;

constructor TTCPBlockSocket.Create;
begin
  CreateWithSSL(SSLImplementation);
end;

destructor TTCPBlockSocket.Destroy;
begin
  inherited Destroy;
  FSSL.Free;
end;

function TTCPBlockSocket.GetErrorDescEx: string;
begin
  Result := inherited GetErrorDescEx;
  if (FLastError = WSASYSNOTREADY) and (self.SSL.LastError <> 0) then
  begin
    Result := self.SSL.LastErrorDesc;
  end;
end;

procedure TTCPBlockSocket.CloseSocket;
begin
  if FSSL.SSLEnabled then
    FSSL.Shutdown;
  if (FSocket <> INVALID_SOCKET) and (FLastError = 0) then
  begin
    Synsock.Shutdown(FSocket, 1);
    Purge;
  end;
  inherited CloseSocket;
end;

procedure TTCPBlockSocket.DoAfterConnect;
begin
  if assigned(OnAfterConnect) then
  begin
    OnAfterConnect(Self);
  end;
end;

function TTCPBlockSocket.WaitingData: Integer;
begin
  Result := 0;
  if FSSL.SSLEnabled and (FSocket <> INVALID_SOCKET) then
    Result := FSSL.WaitingData;
  if Result = 0 then
    Result := inherited WaitingData;
end;

procedure TTCPBlockSocket.Listen;
var
  b: Boolean;
  Sip,SPort: string;
begin
  if FSocksIP = '' then
  begin
    inherited Listen;
  end
  else
  begin
    Sip := GetLocalSinIP;
    if Sip = cAnyHost then
      Sip := LocalName;
    SPort := IntToStr(GetLocalSinPort);
    inherited Connect(FSocksIP, FSocksPort);
    b := SocksOpen;
    if b then
      b := SocksRequest(2, Sip, SPort);
    if b then
      b := SocksResponse;
    if not b and (FLastError = 0) then
      FLastError := WSANO_RECOVERY;
    FSocksLocalIP := FSocksResponseIP;
    if FSocksLocalIP = cAnyHost then
      FSocksLocalIP := FSocksIP;
    FSocksLocalPort := FSocksResponsePort;
    FSocksRemoteIP := '';
    FSocksRemotePort := '';
    ExceptCheck;
    DoStatus(HR_Listen, '');
  end;
end;

function TTCPBlockSocket.Accept: TSocket;
begin
  if FUsingSocks then
  begin
    if not SocksResponse and (FLastError = 0) then
      FLastError := WSANO_RECOVERY;
    FSocksRemoteIP := FSocksResponseIP;
    FSocksRemotePort := FSocksResponsePort;
    Result := FSocket;
    ExceptCheck;
    DoStatus(HR_Accept, '');
  end
  else
  begin
    result := inherited Accept;
  end;
end;

procedure TTCPBlockSocket.Connect(IP, Port: string);
begin
  if FSocksIP <> '' then
    SocksDoConnect(IP, Port)
  else
    if FHTTPTunnelIP <> '' then
      HTTPTunnelDoConnect(IP, Port)
    else
      inherited Connect(IP, Port);
  if FLasterror = 0 then
    DoAfterConnect;
end;

procedure TTCPBlockSocket.SocksDoConnect(IP, Port: string);
var
  b: Boolean;
begin
  inherited Connect(FSocksIP, FSocksPort);
  if FLastError = 0 then
  begin
    b := SocksOpen;
    if b then
      b := SocksRequest(1, IP, Port);
    if b then
      b := SocksResponse;
    if not b and (FLastError = 0) then
      FLastError := WSASYSNOTREADY;
    FSocksLocalIP := FSocksResponseIP;
    FSocksLocalPort := FSocksResponsePort;
    FSocksRemoteIP := IP;
    FSocksRemotePort := Port;
  end;
  ExceptCheck;
  DoStatus(HR_Connect, IP + ':' + Port);
end;

procedure TTCPBlockSocket.HTTPTunnelDoConnect(IP, Port: string);
//bugfixed by Mike Green (mgreen@emixode.com)
var
  s: string;
begin
  Port := IntToStr(ResolvePort(Port));
  inherited Connect(FHTTPTunnelIP, FHTTPTunnelPort);
  if FLastError <> 0 then
    Exit;
  FHTTPTunnel := False;
  if IsIP6(IP) then
    IP := '[' + IP + ']';
  SendString('CONNECT ' + IP + ':' + Port + ' HTTP/1.0' + CRLF);
  if FHTTPTunnelUser <> '' then
  Sendstring('Proxy-Authorization: Basic ' +
    EncodeBase64(FHTTPTunnelUser + ':' + FHTTPTunnelPass) + CRLF);
  SendString(CRLF);
  repeat
    s := RecvTerminated(FHTTPTunnelTimeout, #$0a);
    if FLastError <> 0 then
      Break;
    if (Pos('HTTP/', s) = 1) and (Length(s) > 11) then
      FHTTPTunnel := s[10] = '2';
  until (s = '') or (s = #$0d);
  if (FLasterror = 0) and not FHTTPTunnel then
    FLastError := WSAECONNREFUSED;
  FHTTPTunnelRemoteIP := IP;
  FHTTPTunnelRemotePort := Port;
  ExceptCheck;
end;

procedure TTCPBlockSocket.SSLDoConnect;
begin
  ResetLastError;
  if not FSSL.Connect then
    FLastError := WSASYSNOTREADY;
  ExceptCheck;
end;

procedure TTCPBlockSocket.SSLDoShutdown;
begin
  ResetLastError;
  FSSL.BiShutdown;
end;

function TTCPBlockSocket.GetLocalSinIP: string;
begin
  if FUsingSocks then
    Result := FSocksLocalIP
  else
    Result := inherited GetLocalSinIP;
end;

function TTCPBlockSocket.GetRemoteSinIP: string;
begin
  if FUsingSocks then
    Result := FSocksRemoteIP
  else
    if FHTTPTunnel then
      Result := FHTTPTunnelRemoteIP
    else
      Result := inherited GetRemoteSinIP;
end;

function TTCPBlockSocket.GetLocalSinPort: Integer;
begin
  if FUsingSocks then
    Result := StrToIntDef(FSocksLocalPort, 0)
  else
    Result := inherited GetLocalSinPort;
end;

function TTCPBlockSocket.GetRemoteSinPort: Integer;
begin
  if FUsingSocks then
    Result := ResolvePort(FSocksRemotePort)
  else
    if FHTTPTunnel then
      Result := StrToIntDef(FHTTPTunnelRemotePort, 0)
    else
      Result := inherited GetRemoteSinPort;
end;

function TTCPBlockSocket.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
begin
  if FSSL.SSLEnabled then
  begin
    Result := 0;
    if TestStopFlag then
      Exit;
    ResetLastError;
    LimitBandwidth(Len, FMaxRecvBandwidth, FNextRecv);
    Result := FSSL.RecvBuffer(Buffer, Len);
    if FSSL.LastError <> 0 then
      FLastError := WSASYSNOTREADY;
    ExceptCheck;
    Inc(FRecvCounter, Result);
    DoStatus(HR_ReadCount, IntToStr(Result));
    DoMonitor(False, Buffer, Result);
    DoReadFilter(Buffer, Result);
  end
  else
    Result := inherited RecvBuffer(Buffer, Len);
end;

function TTCPBlockSocket.SendBuffer(Buffer: TMemory; Length: Integer): Integer;
var
  x, y: integer;
  l, r: integer;
{$IFNDEF CIL}
  p: Pointer;
{$ENDIF}
begin
  if FSSL.SSLEnabled then
  begin
    Result := 0;
    if TestStopFlag then
      Exit;
    ResetLastError;
    DoMonitor(True, Buffer, Length);
{$IFDEF CIL}
    Result := FSSL.SendBuffer(Buffer, Length);
    if FSSL.LastError <> 0 then
      FLastError := WSASYSNOTREADY;
    Inc(FSendCounter, Result);
    DoStatus(HR_WriteCount, IntToStr(Result));
{$ELSE}
    l := Length;
    x := 0;
    while x < l do
    begin
      y := l - x;
      if y > FSendMaxChunk then
        y := FSendMaxChunk;
      if y > 0 then
      begin
        LimitBandwidth(y, FMaxSendBandwidth, FNextsend);
        p := IncPoint(Buffer, x);
        r := FSSL.SendBuffer(p, y);
        if FSSL.LastError <> 0 then
          FLastError := WSASYSNOTREADY;
        if Flasterror <> 0 then
          Break;
        Inc(x, r);
        Inc(Result, r);
        Inc(FSendCounter, r);
        DoStatus(HR_WriteCount, IntToStr(r));
      end
      else
        break;
    end;
{$ENDIF}
    ExceptCheck;
  end
  else
    Result := inherited SendBuffer(Buffer, Length);
end;

function TTCPBlockSocket.SSLAcceptConnection: Boolean;
begin
  ResetLastError;
  if not FSSL.Accept then
    FLastError := WSASYSNOTREADY;
  ExceptCheck;
  Result := FLastError = 0;
end;

function TTCPBlockSocket.GetSocketType: integer;
begin
  Result := integer(SOCK_STREAM);
end;

function TTCPBlockSocket.GetSocketProtocol: integer;
begin
  Result := integer(IPPROTO_TCP);
end;

{======================================================================}

function TICMPBlockSocket.GetSocketType: integer;
begin
  Result := integer(SOCK_RAW);
end;

function TICMPBlockSocket.GetSocketProtocol: integer;
begin
  if FIP6Used then
    Result := integer(IPPROTO_ICMPV6)
  else
    Result := integer(IPPROTO_ICMP);
end;

{======================================================================}

function TRAWBlockSocket.GetSocketType: integer;
begin
  Result := integer(SOCK_RAW);
end;

function TRAWBlockSocket.GetSocketProtocol: integer;
begin
  Result := integer(IPPROTO_RAW);
end;

{======================================================================}

function TPGMmessageBlockSocket.GetSocketType: integer;
begin
  Result := integer(SOCK_RDM);
end;

function TPGMmessageBlockSocket.GetSocketProtocol: integer;
begin
  Result := integer(IPPROTO_RM);
end;

{======================================================================}

function TPGMstreamBlockSocket.GetSocketType: integer;
begin
  Result := integer(SOCK_STREAM);
end;

function TPGMstreamBlockSocket.GetSocketProtocol: integer;
begin
  Result := integer(IPPROTO_RM);
end;

{======================================================================}

constructor TSynaClient.Create;
begin
  inherited Create;
  FIPInterface := cAnyHost;
  FTargetHost := cLocalhost;
  FTargetPort := cAnyPort;
  FTimeout := 5000;
  FUsername := '';
  FPassword := '';
end;

{======================================================================}

constructor TCustomSSL.Create(const Value: TTCPBlockSocket);
begin
  inherited Create;
  FSocket := Value;
  FSSLEnabled := False;
  FUsername := '';
  FPassword := '';
  FLastError := 0;
  FLastErrorDesc := '';
  FVerifyCert := False;
  FSSLType := LT_all;
  FKeyPassword := '';
  FCiphers := '';
  FCertificateFile := '';
  FPrivateKeyFile := '';
  FCertCAFile := '';
  FCertCA := '';
  FTrustCertificate := '';
  FTrustCertificateFile := '';
  FCertificate := '';
  FPrivateKey := '';
  FPFX := '';
  FPFXfile := '';
  FSSHChannelType := '';
  FSSHChannelArg1 := '';
  FSSHChannelArg2 := '';
  FCertComplianceLevel := -1; //default
  FSNIHost := '';
end;

procedure TCustomSSL.Assign(const Value: TCustomSSL);
begin
  FUsername := Value.Username;
  FPassword := Value.Password;
  FVerifyCert := Value.VerifyCert;
  FSSLType := Value.SSLType;
  FKeyPassword := Value.KeyPassword;
  FCiphers := Value.Ciphers;
  FCertificateFile := Value.CertificateFile;
  FPrivateKeyFile := Value.PrivateKeyFile;
  FCertCAFile := Value.CertCAFile;
  FCertCA := Value.CertCA;
  FTrustCertificate := Value.TrustCertificate;
  FTrustCertificateFile := Value.TrustCertificateFile;
  FCertificate := Value.Certificate;
  FPrivateKey := Value.PrivateKey;
  FPFX := Value.PFX;
  FPFXfile := Value.PFXfile;
  FCertComplianceLevel := Value.CertComplianceLevel;
  FSNIHost := Value.FSNIHost;
end;

procedure TCustomSSL.ReturnError;
begin
  FLastError := -1;
  FLastErrorDesc := 'SSL/TLS support is not compiled!';
end;

function TCustomSSL.LibVersion: String;
begin
  Result := '';
end;

function TCustomSSL.LibName: String;
begin
  Result := '';
end;

function TCustomSSL.CreateSelfSignedCert(Host: string): Boolean;
begin
  Result := False;
end;

function TCustomSSL.Connect: boolean;
begin
  ReturnError;
  Result := False;
end;

function TCustomSSL.Accept: boolean;
begin
  ReturnError;
  Result := False;
end;

function TCustomSSL.Shutdown: boolean;
begin
  ReturnError;
  Result := False;
end;

function TCustomSSL.BiShutdown: boolean;
begin
  ReturnError;
  Result := False;
end;

function TCustomSSL.SendBuffer(Buffer: TMemory; Len: Integer): Integer;
begin
  ReturnError;
  Result := integer(SOCKET_ERROR);
end;

procedure TCustomSSL.SetCertCAFile(const Value: string);
begin
  FCertCAFile := Value;
end;

function TCustomSSL.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
begin
  ReturnError;
  Result := integer(SOCKET_ERROR);
end;

function TCustomSSL.WaitingData: Integer;
begin
  ReturnError;
  Result := 0;
end;

function TCustomSSL.GetSSLVersion: string;
begin
  Result := '';
end;

function TCustomSSL.GetPeerSubject: string;
begin
  Result := '';
end;

function TCustomSSL.GetPeerSerialNo: integer;
begin
  Result := -1;
end;

function TCustomSSL.GetPeerName: string;
begin
  Result := '';
end;

function TCustomSSL.GetPeerNameHash: cardinal;
begin
  Result := 0;
end;

function TCustomSSL.GetPeerIssuer: string;
begin
  Result := '';
end;

function TCustomSSL.GetPeerFingerprint: string;
begin
  Result := '';
end;

function TCustomSSL.GetCertInfo: string;
begin
  Result := '';
end;

function TCustomSSL.GetCipherName: string;
begin
  Result := '';
end;

function TCustomSSL.GetCipherBits: integer;
begin
  Result := 0;
end;

function TCustomSSL.GetCipherAlgBits: integer;
begin
  Result := 0;
end;

function TCustomSSL.GetVerifyCert: integer;
begin
  Result := 1;
end;

function TCustomSSL.DoVerifyCert:boolean;
begin
  if assigned(OnVerifyCert) then
  begin
    result:=OnVerifyCert(Self);
  end
  else
    result:=true;
end;


{======================================================================}

function TSSLNone.LibVersion: String;
begin
  Result := 'Without SSL support';
end;

function TSSLNone.LibName: String;
begin
  Result := 'ssl_none';
end;

{======================================================================}

initialization
begin
{$IFDEF ONCEWINSOCK}
  if not InitSocketInterface(DLLStackName) then
  begin
    e := ESynapseError.Create('Error loading Socket interface (' + DLLStackName + ')!');
    e.ErrorCode := 0;
    e.ErrorMessage := 'Error loading Socket interface (' + DLLStackName + ')!';
    raise e;
  end;
  synsock.WSAStartup(WinsockLevel, WsaDataOnce);
{$ENDIF}
end;

finalization
begin
{$IFDEF ONCEWINSOCK}
  synsock.WSACleanup;
  DestroySocketInterface;
{$ENDIF}
end;

end.
