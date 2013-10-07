/// classes implementing HTTP/1.1 client and server protocol
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SynCrtSock;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2012 Arnaud Bouchez
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

  The Original Code is Synopse SQLite3/mORMot database framework.

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


   TCP/IP and HTTP/1.1 Client and Server
  ***************************************

  Initial version: 2009 May, by Arnaud Bouchez

  Version 1.4 - February 8, 2010
  - whole Synopse SQLite3 database framework released under the GNU Lesser
    General Public License version 3, instead of generic "Public Domain"
  - fix a bug happening when multiple HTTP connections were opened and
    closed in the same program

  Version 1.5 - March 1, 2010
  - new generic unix implementation, using libc sockets, in SynLibcSock.pas

  Version 1.9
  - avoid some GPF during client deconnection when the server shut down
  - rewrite HTTP Server handle request loop keep alive timing
  - HTTP Server now use a Thread Pool to speed up multi-connections: this
    speed up a lot HTTP/1.0 requests, by creating a Thread only if
    necessary

  Version 1.9.2
  - deleted deprecated DOS related code (formerly used with DWPL Dos Extender)
  - a dedicated thread is now used if the incoming HTTP request has
    POSTed a body content of more than 16 KB (to avoid Deny Of Service, and
    preserve the Thread Pool to only real small processes)
  - new CROnly parameter for TCrtSocket.SockRecvLn, to handle #13 as
    line delimiter: by default, #10 or #13#10 are line delimiters
    (as for normal Window/Linux text files)

  Version 1.12
  - added connection check and exception handling in
    THttpServerSocket.GetRequest, which now is a function returning a boolean
  - added DOS / TCP SYN Flood detection if THttpServerSocket.GetRequest
    spent more than 2 seconds to get header from HTTP Client

  Version 1.13
  - code modifications to compile with Delphi 5 compiler
  - new THttpApiServer class, using fast http.sys kernel-mode server
    for better performance and less resource usage
  - DOS / TCP SYN Flood detection time enhanced to 5 seconds
  - fixed HTTP client stream layout (to be more RFC compliant)
  - new generic compression handling mechanism: can handle gzip, deflate
    or custom synlz / synlzo algorithms via THttpSocketCompress functions
  - new THttpServerGeneric.Request virtual abstract method prototype
  - new TWinINet class, using WinINet API (very slow, do not use)
  - new TWinHTTP class, using WinHTTP API (faster than THttpClientSocket):
    this is the class to be used

  Version 1.15
  - unit now tested with Delphi XE2 (32 Bit)
  - fixed issue in HTTP_RESPONSE.SetHeaders

  Version 1.16
  - fixed issue in case of wrong void parameter e.g. in THttpApiServer.AddUrl
  - circumvent some bugs of Delphi XE2 background compiler (main compiler is OK)
  - added 'RemoteIP: 127.0.0.1' to the retrieved HTTP headers
  - major speed up of THttpApiServer for Windows Vista and up, by processing
    huge content in chunks: upload of 100Mb file take 25 sec before and 6 sec
    after changes, according to feedback by MPV - ticket 711247b998
  - new THttpServerGeneric.OnHttpThreadTerminate event, available to clean-up
    any process in the thread context, when it is terminated (to call e.g.
    TSQLDBConnectionPropertiesThreadSafe.EndCurrentThread in order to call
    CoUnInitialize from thread in which CoInitialize was initialy made) - see
    http://synopse.info/fossil/tktview?name=213544b2f5

  Version 1.17
  - replaced TSockData string type to the generic RawByteString type (and
    the default AnsiString for non-Unicode version of Delphi)
  - added optional aProxyName, aProxyByPass parameters to TWinHttpAPI /
    TWinInet and TWinHTTP constructors
  - added THttpServerGeneric.OnHttpThreadStart property, and associated
    TNotifyThreadEvent event prototype
  - handle 'Range: bytes=***-***' request in THttpApiServer


}

interface

{ $define DEBUG2}
{ $define DEBUG23}

{$ifdef MSWINDOWS}
  {$define USEWININET}
  /// define this to publish TWinINet / TWinHttp / TWinHttpAPI classes
{$else}
  {$undef USEWININET} // WinINet / WinHTTP / HttpAPI expect a Windows system
{$endif}

{$ifdef DEBUG2}
{.$define DEBUG}
{$endif}

{$define USETHREADPOOL}
// define this to use TSynThreadPool for faster multi-connection
// with Thread Pool: 3394 requests / second (each request received 4 KB of data)
// without the Pool: 140/s in the IDE (i.e. one core), 2637/s on a dual core

uses
{$ifndef LINUX}
  Windows,
  SynWinSock,
  {$ifdef USEWININET}
    WinInet,
  {$endif}
{$else}
  {$undef USEWININET}
  {$ifdef CONDITIONALEXPRESSIONS}
    Types,
  {$endif}
  LibC,
{$endif}
{$ifndef LVCL}
  Contnrs,
{$endif}
  SysUtils,
  Classes;

type
{$ifndef UNICODE}
  /// define RawByteString, as it does exist in Delphi 2009 and up
  // - to be used for byte storage into an AnsiString
  RawByteString = AnsiString;
{$endif}

{$ifndef CONDITIONALEXPRESSIONS}
  // not defined in Delphi 5 or older
  PPointer = ^Pointer;
  TTextLineBreakStyle = (tlbsLF, tlbsCRLF);
{$endif}

{$ifndef FPC}
  /// FPC 64 compatibility integer type
  PtrInt = integer;
  /// FPC 64 compatibility pointer type
  PPtrInt = ^PtrInt;
  {$ifdef UNICODE}
  PtrUInt = NativeUInt;
  {$else}
  PtrUInt = Cardinal;
  {$endif}
  PPtrUInt = ^PtrUInt;
{$endif}

  /// exception thrown by the classes of this unit
  ECrtSocket = class(Exception)
  public
    constructor Create(const Msg: string); overload;
    constructor Create(const Msg: string; Error: integer); overload;
  end;

  TCrtSocketClass = class of TCrtSocket;

  /// the available available network transport layer
  // - either TCP/IP, UDP/IP or Unix sockets
  TCrtSocketLayer = (cslTCP, cslUDP, cslUNIX);

  /// Fast low-level Socket implementation
  // - direct access to the OS (Windows, Linux) network layer
  // - use Open constructor to create a client to be connected to a server
  // - use Bind constructor to initialize a server
  // - use direct access to low level Windows or Linux network layer
  // - use SockIn and SockOut (after CreateSock*) to read or write data
  //  as with standard Delphi text files (see SendEmail implementation)
  // - if app is multi-threaded, use faster SockSend() instead of SockOut^
  //  for direct write access to the socket; but SockIn^ is much faster than
  // SockRecv() thanks to its internal buffer, even on multi-threaded app
  // (at least under Windows, it may be up to 10 times faster)
  // - but you can decide whatever to use none, one or both SockIn/SockOut
  // - our classes are much faster than the Indy or Synapse implementation
  TCrtSocket = class
  public
    /// initialized after Open() with socket
    Sock: TSocket;
    /// initialized after Open() with Server name
    Server: AnsiString;
    /// initialized after Open() with port number
    Port: AnsiString;
    /// after CreateSockIn, use Readln(SockIn,s) to read a line from the opened socket
    SockIn: ^TextFile;
    /// after CreateSockOut, use Writeln(SockOut,s) to send a line to the opened socket
    SockOut: ^TextFile;
    /// if higher than 0, read loop will wait for incoming data till
    // TimeOut milliseconds (default value is 10000) - used also in SockSend()
    TimeOut: cardinal;
    /// total bytes received
    BytesIn,
    /// total bytes sent
    BytesOut: cardinal;
    /// connect to aServer:aPort
    constructor Open(const aServer, aPort: AnsiString; aLayer: TCrtSocketLayer=cslTCP);
    /// bind to aPort
    constructor Bind(const aPort: AnsiString; aLayer: TCrtSocketLayer=cslTCP);
    /// raise an ECrtSocket exception on error (called by above constructors)
    procedure OpenBind(const aServer, aPort: AnsiString; doBind: boolean;
      aSock: integer=-1; aLayer: TCrtSocketLayer=cslTCP);
    /// initialize SockIn for receiving with read[ln](SockIn^,...)
    // - data is buffered, filled as the data is available
    // - read(char) or readln() is indeed very fast
    // - multithread applications would also use this SockIn pseudo-text file
    // - by default, expect CR+LF as line feed (i.e. the HTTP way)
    procedure CreateSockIn(LineBreak: TTextLineBreakStyle=tlbsCRLF);
    /// initialize SockOut for sending with write[ln](SockOut^,....)
    // - data is sent (flushed) after each writeln() - it's a compiler feature
    // - use rather SockSend() + SockSendFlush to send headers at once e.g.
    // since writeln(SockOut^,..) flush buffer each time
    procedure CreateSockOut;
    /// close the opened socket, and corresponding SockIn/SockOut
    destructor Destroy; override;
    /// read Length bytes from SockIn buffer + Sock if necessary
    // - if SockIn is available, it first gets data from SockIn^.Buffer,
    // then directly receive data from socket
    // - can be used also without SockIn: it will call directly SockRecv() in such case
    function SockInRead(Content: PAnsiChar; Length: integer): integer;
    /// check the connection status of the socket
    function SockConnected: boolean;
    /// wait till some data is pending in the receiving queue within TimeOut milliseconds
    // - returns 1 if there is some data to be read
    // - returns 0 if there is no data to be read
    // - returns <0 on any socket error
    function SockCanRead(aTimeOut: cardinal): integer;
    /// wait till some data can be sent within TimeOut milliseconds
    // - returns >0 if data can be written
    // - returns 0 if data can not be written
    // - returns <0 on any socket error
    function SockCanWrite(aTimeOut: cardinal): integer;
    /// simulate writeln() with direct use of Send(Sock, ..)
    // - usefull on multi-treaded environnement (as in THttpServer.Process)
    // - no temp buffer is used
    // - handle RawByteString, ShortString, Char, Integer parameters
    // - raise ECrtSocket exception on socket error
    procedure SockSend(const Values: array of const); overload;
    /// simulate writeln() with a single line
    procedure SockSend(const Line: RawByteString=''); overload;
    /// flush all pending data to be sent
    procedure SockSendFlush;
    /// fill the Buffer with Length bytes
    // - use TimeOut milliseconds wait for incoming data
    // - bypass the SockIn^ buffers
    // - raise ECrtSocket exception on socket error
    procedure SockRecv(Buffer: pointer; Length: integer);
    /// returns the socket input stream as a string
    // - specify the Max time to wait until some data is available for reading
    // - if the TimeOut parameter is 0, wait until something is available
    function SockReceiveString(aTimeOut : integer = 300): RawByteString;
    /// fill the Buffer with Length bytes
    // - use TimeOut milliseconds wait for incoming data
    // - bypass the SockIn^ buffers
    // - return false on any error, true on success
    function TrySockRecv(Buffer: pointer; Length: integer): boolean;
    /// call readln(SockIn^,Line) or simulate it with direct use of Recv(Sock, ..)
    // - char are read one by one
    // - use TimeOut milliseconds wait for incoming data
    // - raise ECrtSocket exception on socket error
    // - by default, will handle #10 or #13#10 as line delimiter (as normal text
    // files), but you can delimit lines using #13 if CROnly is TRUE 
    procedure SockRecvLn(out Line: RawByteString; CROnly: boolean=false); overload;
    /// call readln(SockIn^) or simulate it with direct use of Recv(Sock, ..)
    // - char are read one by one
    // - use TimeOut milliseconds wait for incoming data
    // - raise ECrtSocket exception on socket error
    // - line content is ignored
    procedure SockRecvLn; overload;
    /// append P^ data into SndBuf (used by SockSend(), e.g.)
    // - call SockSendFlush to send it through the network via SndLow()
    procedure Snd(P: pointer; Len: integer);
    /// direct send data through network
    // - raise a ECrtSocket exception on any error
    // - bypass the SndBuf or SockOut^ buffers
    procedure SndLow(P: pointer; Len: integer);
    /// direct send data through network
    // - return false on any error, true on success
    // - bypass the SndBuf or SockOut^ buffers
    function TrySndLow(P: pointer; Len: integer): boolean;
    /// direct send data through network
    // - raise a ECrtSocket exception on any error
    // - bypass the SndBuf or SockOut^ buffers
    // - raw Data is sent directly to OS: no CR/CRLF is appened to the block
    procedure Write(const Data: RawByteString);
  private
    SockInEof: boolean;
    /// updated by every Snd()
    SndBuf: RawByteString;
    SndBufLen: integer;
    /// close and shutdown the connection (called from Destroy)
    procedure Close;
  end;

  /// event used to compress or uncompress some data during HTTP protocol
  // - should always return the protocol name for ACCEPT-ENCODING: header
  // e.g. 'gzip' or 'deflate' for standard HTTP format, but you can add
  // your own (like 'synlzo' or 'synlz')
  // - the data is compressed (if Compress=TRUE) or uncompressed (if
  // Compress=FALSE) in the Data variable (i.e. it is modified in-place)
  // - to be used with THttpSocket.RegisterCompress method
  // - type is a generic AnsiString, which should be in practice a
  // RawByteString or a RawByteString 
  THttpSocketCompress = function(var Data: RawByteString; Compress: boolean): RawByteString;

  /// used to maintain a list of known compression algorithms
  THttpSocketCompressRec = record
    /// the compression name, as in ACCEPT-ENCODING: header (gzip,deflate,synlz)
    Name: AnsiString;
    /// the function handling compression and decompression
    Func: THttpSocketCompress;
  end;

  THttpSocketCompressRecDynArray = array of THttpSocketCompressRec;

  THttpSocketCompressSet = set of 0..31;

  /// parent of THttpClientSocket and THttpServerSocket classes
  // - contain properties for implementing the HTTP/1.1 protocol
  // - handle chunking of body content
  // - can optionaly compress and uncompress on the fly the data, with
  // standard gzip/deflate or custom (synlzo/synlz) protocols
  THttpSocket = class(TCrtSocket)
  protected
    /// true if the TRANSFER-ENCODING: CHUNKED was set in headers
    Chunked: boolean;
    /// to call GetBody only once
    fBodyRetrieved: boolean;
    /// used by RegisterCompress method
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: RawByteString;
    /// GetHeader set index of protocol in fCompress[], from ACCEPT-ENCODING: 
    fCompressHeader: THttpSocketCompressSet;
    /// same as HeaderValue('Content-Encoding'), but retrieved during Request
    // and mapped into the fCompress[] array
    fContentCompress: integer;
    /// retrieve the HTTP headers into Headers[] and fill most properties below
    procedure GetHeader;
    /// retrieve the HTTP body (after uncompression if necessary) into Content
    procedure GetBody;
    /// compress the data, adding corresponding headers via SockSend()
    // - always add a 'Content-Length: ' header entry (even if length=0)
    // - e.g. 'Content-Encoding: synlz' header if compressed using synlz
    // - and if Data is not '', will add 'Content-Type: ' header
    procedure CompressDataAndWriteHeaders(const OutContentType: RawByteString;
      var OutContent: RawByteString);
  public
    /// TCP/IP prefix to mask HTTP protocol
    // - if not set, will create full HTTP/1.0 or HTTP/1.1 compliant content
    // - in order to make the TCP/IP stream not HTTP compliant, you can specify
    // a prefix which will be put before the first header line: in this case,
    // the TCP/IP stream won't be recognized as HTTP, and will be ignored by
    // most AntiVirus programs, and increase security - but you won't be able
    // to use an Internet Browser nor AJAX application for remote access any more
    TCPPrefix: RawByteString;
    /// will contain the first header line:
    // - 'GET /path HTTP/1.1' for a GET request with THttpServer, e.g.
    // - 'HTTP/1.0 200 OK' for a GET response after Get() e.g.
    Command: RawByteString;
    /// will contain the header lines after a Request - use HeaderValue() to get one
    Headers: array of RawByteString;
    /// will contain the data retrieved from the server, after the Request
    Content: RawByteString;
    /// same as HeaderValue('Content-Length'), but retrieved during Request
    // - is overriden with real Content length during HTTP body retrieval
    ContentLength: integer;
    /// same as HeaderValue('Content-Type'), but retrieved during Request
    ContentType: RawByteString;
    /// same as HeaderValue('Connection')='close', but retrieved during Request
    ConnectionClose: boolean;
    /// add an header entry, returning the just entered entry index in Headers[]s
    function HeaderAdd(const aValue: RawByteString): integer;
    /// set all Header values at once, from CRLF delimited text
    procedure HeaderSetText(const aText: RawByteString);
    /// get all Header values at once, as CRLF delimited text
    function HeaderGetText: RawByteString; virtual;
    /// HeaderValue('Content-Type')='text/html', e.g.
    function HeaderValue(aName: RawByteString): RawByteString;
    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlzo/synlz) protocols
    // - returns true on success, false if this function or this
    // ACCEPT-ENCODING: header was already registered
    // - the first registered algorithm will be the prefered one for compression
    function RegisterCompress(aFunction: THttpSocketCompress): boolean;
  end;

  THttpServer = class;

  /// HTTP/1.1 server class used by THttpServer main server Thread
  THttpServerSocket = class(THttpSocket)
  private
  public
    /// contains the method ('GET','POST'.. e.g.) after GetRequest()
    Method: RawByteString;
    /// contains the URL ('/' e.g.) after GetRequest()
    URL: RawByteString;
    /// true if the client is HTTP/1.1 and 'Connection: Close' is not set
    // (default HTTP/1.1 behavior is keep alive, unless 'Connection: Close'
    // is specified, cf. RFC 2068 page 108: "HTTP/1.1 applications that do not
    // support persistent connections MUST include the "close" connection option
    // in every message")
    KeepAliveClient: boolean;
    /// create the socket according to a server
    // - will register the THttpSocketCompress functions from the server
    constructor Create(aServer: THttpServer);
    /// main object function called after aClientSock := Accept + Create:
    // - get initialize the socket with the supplied accepted socket
    // - caller will then use the GetRequest method below to
    // get the request
    procedure InitRequest(aClientSock: TSocket);
    /// main object function called after aClientSock := Accept + Create:
    // - get Command, Method, URL, Headers and Body (if withBody is TRUE)
    // - get sent data in Content (if ContentLength<>0)
    // - return false if the socket was not connected any more, or if
    // any exception occured during the process
    function GetRequest(withBody: boolean=true): boolean;
    /// get all Header values at once, as CRLF delimited text
    // - this overriden version will add the 'RemoteIP: 1.2.3.4' header
    function HeaderGetText: RawByteString; override;
  end;

  /// REST and HTTP/1.1 compatible client class
  // - this component is HTTP/1.1 compatible, according to RFC 2068 document
  // - the REST commands (GET/POST/PUT/DELETE) are directly available
  // - open connection with the server with inherited Open(server,port) function
  // - if KeepAlive>0, the connection is not broken: a further request (within
  // KeepAlive milliseconds) will use the existing connection if available,
  // or recreate a new one if the former is outdated or reset by server
  // (will retry only once); this is faster, uses less resources (especialy
  // under Windows), and is the recommended way to implement a HTTP/1.1 server
  // - on any error (timeout, connection closed) will retry once to get the value
  // - don't forget to use Free procedure when you are finished
  THttpClientSocket = class(THttpSocket)
  public
    /// by default, the client is identified as IE 5.5, which is very
    // friendly welcome by most servers :(
    // - you can specify a custom value here
    UserAgent: RawByteString;

    /// after an Open(server,port), return 200 if OK, http status error otherwize - get
    // the page data in Content
    function Get(const url: RawByteString; KeepAlive: cardinal=0; const header: RawByteString=''): integer;
    /// after an Open(server,port), return 200 if OK, http status error otherwize - only
    // header is read from server: Content is always '', but Headers are set
    function Head(const url: RawByteString; KeepAlive: cardinal=0; const header: RawByteString=''): integer;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwize
    function Post(const url, Data, DataType: RawByteString; KeepAlive: cardinal=0;
      const header: RawByteString=''): integer;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwize
    function Put(const url, Data, DataType: RawByteString; KeepAlive: cardinal=0;
      const header: RawByteString=''): integer;
    /// after an Open(server,port), return 200,202,204 if OK, http status error otherwize
    function Delete(const url: RawByteString; KeepAlive: cardinal=0; const header: RawByteString=''): integer;

    /// low-level HTTP/1.1 request
    // - call by all REST methods above
    // - after an Open(server,port), return 200,202,204 if OK, http status error otherwize
    // - retry is false by caller, and will be recursively called with true to retry once
    function Request(const url, method: RawByteString; KeepAlive: cardinal;
      const header, Data, DataType: RawByteString; retry: boolean): integer;
  end;

  /// HTTP response Thread
  // - Execute procedure get the request and calculate the answer
  // - you don't have to overload the protected THttpServerResp Execute method:
  // override THttpServer.Request() function or, if you need a lower-level access
  // (change the protocol, e.g.) THttpServer.Process() method itself
  THttpServerResp = class(TThread)
  protected
    fServer: THttpServer;
    fServerSock: THttpServerSocket;
    fClientSock: TSocket;
    /// main thread loop: read request from socket, send back answer
    procedure Execute; override;
  public
    /// initialize the response thread for the corresponding incoming socket
    // - this version will get the request directly from an incoming socket
    constructor Create(aSock: TSocket; aServer: THttpServer); overload;
    /// initialize the response thread for the corresponding incoming socket
    // - this version will handle KeepAlive, for such an incoming request
    constructor Create(aServerSock: THttpServerSocket; aServer: THttpServer); overload;
    /// release used socket and memory
    destructor Destroy; override;
  end;

  /// a simple Thread Pool, used for fast handling HTTP requests
  // - will handle multi-connection with less overhead than creating a thread
  // for each incoming request
  // - this Thread Pool is implemented over I/O Completion Ports, which is a faster
  // method than keeping a TThread list, and resume them on request: I/O completion
  // just has the thread running while there is pending tasks, with no pause/resume
  TSynThreadPool = object
  protected
    FRequestQueue: THandle;
    FThreads: array of THandle;
  public
    /// initialize a thread pool with the supplied number of threads
    // - WorkerFunc should be e.g. private THttpServerWorkerFunction for
    // handling a THttpServer request
    // - up to 64 threads can be associated to a Thread Pool
    function Initialize(WorkerFunc: TThreadFunc; NumberOfThreads: Integer=32): Boolean;
    /// shut down the Thread pool, releasing all associated threads
    function Shutdown: Boolean;
  end;

  /// a simple Thread Pool, used for fast handling HTTP requests of a THttpServer
  // - will create a THttpServerResp response thread, if the incoming request
  // is identified as HTTP/1.1 keep alive
  TSynThreadPoolTHttpServer = object(TSynThreadPool)
  public
    /// initialize a thread pool with the supplied number of threads
    // - will use private THttpServerWorkerFunction as WorkerFunc
    // - up to 64 threads can be associated to a Thread Pool
    function Initialize(NumberOfThreads: Integer=32): Boolean; reintroduce;
    /// add an incoming HTTP request to the Thread Pool
    function Push(aServer: THttpServer; aClientSock: TSocket): Boolean;
  end;

  /// event handler used by THttpServerGeneric.OnRequest property
  // - InURL/InMethod/InHeaders/InContent properties are input parameters
  // - OutContent/OutContentType/OutCustomHeader are output parameters
  // - result of the function is the HTTP error code (200 if OK, e.g.)
  // - OutCustomHeader will handle Content-Type/Location
  // - if OutContentType is HTTP_RESP_STATICFILE (i.e. '!STATICFILE'),
  // then OutContent is the UTF-8 file name of a file which must be sent to the
  // client via http.sys (much faster than manual buffering/sending)
  TOnHttpServerRequest = function(
      const InURL, InMethod, InHeaders, InContent, InContentType: RawByteString;
      out OutContent, OutContentType, OutCustomHeader: RawByteString): cardinal of object;

  /// event prototype used e.g. by THttpServerGeneric.OnHttpThreadStart
  TNotifyThreadEvent = procedure(Sender: TThread) of object;

  /// generic HTTP server
  THttpServerGeneric = class(TThread)
  protected
    /// optional event handler for the virtual Request method
    fOnRequest: TOnHttpServerRequest;
    /// list of all registered compression algorithms
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: RawByteString;
    fOnHttpThreadStart: TNotifyThreadEvent;
    {$ifndef LVCL}
    fOnHttpThreadTerminate: TNotifyEvent;
    procedure DoTerminate; override;
    {$endif}
    /// server main loop: just launch fOnHttpThreadStart event (if any)
    // - should be called by all overriden methods
    procedure Execute; override;
  public
    /// override this function to customize your http server
    // - InURL/InMethod/InContent properties are input parameters
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - result of the function is the HTTP error code (200 if OK, e.g.)
    // - OutCustomHeader will handle Content-Type/Location
    // - if OutContentType is HTTP_RESP_STATICFILE (i.e. '!STATICFILE'),
    // then OutContent is the UTF-8 file name of a file which must be sent to the
    // client via http.sys (much faster than manual buffering/sending)
    // - default implementation is to call the OnRequest event (if existing)
    // - warning: this process must be thread-safe (can be called by several
    // threads simultaneously)
    function Request(const InURL, InMethod, InHeaders, InContent, InContentType: RawByteString;
      out OutContent, OutContentType, OutCustomHeader: RawByteString): cardinal; virtual;
    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlzo/synlz) protocols
    // - the first registered algorithm will be the prefered one for compression
    procedure RegisterCompress(aFunction: THttpSocketCompress); virtual;
    /// event handler called by the default implementation of the
    // virtual Request method
    // - warning: this process must be thread-safe (can be called by several
    // threads simultaneously)
    property OnRequest: TOnHttpServerRequest read fOnRequest write fOnRequest;
    /// event handler called when the Thread is just initiated
    // - called in the thread context at first place in THttpServerGeneric.Execute
    property OnHttpThreadStart: TNotifyThreadEvent read fOnHttpThreadStart write fOnHttpThreadStart;
    /// event handler called when the Thread is terminating, in the thread context
    // - the TThread.OnTerminate event will be called within a Synchronize()
    // wrapper, so it won't fit our purpose
    // - to be used e.g. to call CoUnInitialize from thread in which CoInitialize
    // was made, for instance via a method defined as such:
    // ! procedure TMyServer.OnHttpThreadTerminate(Sender: TObject);
    // ! begin // TSQLDBConnectionPropertiesThreadSafe
    // !   fMyConnectionProps.EndCurrentThread;
    // ! end;
    property OnHttpThreadTerminate: TNotifyEvent
    {$ifdef LVCL}
      read fOnTerminate write fOnTerminate;
    {$else}
      read fOnHttpThreadTerminate write fOnHttpThreadTerminate;
    {$endif}
  end;

  {/ HTTP server using fast http.sys kernel-mode server
   - The HTTP Server API enables applications to communicate over HTTP without
   using Microsoft Internet Information Server (IIS). Applications can register
   to receive HTTP requests for particular URLs, receive HTTP requests, and send
   HTTP responses. The HTTP Server API includes SSL support so that applications
   can exchange data over secure HTTP connections without IIS. It is also
   designed to work with I/O completion ports.
   - The HTTP Server API is supported on Windows Server 2003 operating systems
   and on Windows XP with Service Pack 2 (SP2). Be aware that Microsoft IIS 5
   running on Windows XP with SP2 is not able to share port 80 with other HTTP
   applications running simultaneously. }
  THttpApiServer = class(THttpServerGeneric)
  protected
    /// the internal request queue
		fReqQueue: THandle;
    /// contain clones list
    fClones: TObjectList;
    /// list of all registered URL (Unicode-encoded)
    fRegisteredUrl: array of RawByteString;
    /// server main loop - don't change directly
    // - will call the Request public virtual method with the appropriate
    // parameters to retrive the content
    procedure Execute; override;
    /// create a clone
    constructor CreateClone(From: THttpApiServer);
  public
    /// initialize the HTTP Service
    // - will raise an exception if http.sys is not available (e.g. before
    // Windows XP SP2) or if the request queue creation failed
    // - if you override this contructor, put the AddUrl() methods within,
    // and you can set CreateSuspended to TRUE
    // - if you will call AddUrl() methods later, set CreateSuspended to FALSE,
    // then call explicitely the Resume method, after all AddUrl() calls, in
    // order to start the server
    constructor Create(CreateSuspended: Boolean);
    /// release all associated memory and handles
    destructor Destroy; override;
    /// will clone this thread into multiple other threads
    // - could speed up the process on multi-core CPU
    // - will work only if the OnProcess property was set (this is the case
    // e.g. in TSQLite3HttpServer.Create() constructor)
    // - maximum value is 256 - higher should not be worth it
    procedure Clone(ChildThreadCount: integer);
    /// register the URLs to Listen On
    // - e.g. AddUrl('root','888')
    // - aDomainName could be either a fully qualified case-insensitive domain
    // name, an IPv4 or IPv6 literal string, or a wildcard ('+' will bound
    // to all domain names for the specified port, '*' will accept the request
    // when no other listening hostnames match the request for that port)
    // - return 0 (NO_ERROR) on success, an error code if failed: under Vista
    // and Seven, you could have ERROR_ACCESS_DENIED if the process is not
    // running with enough rights (by default, UAC requires administrator rights
    // for adding an URL to http.sys registration list) - solution is to call
    // the THttpApiServer.AddUrlAuthorize class method during program setup 
    // - if this method is not used within an overriden constructor, default
    // Create must have be called with CreateSuspended = TRUE and then call the
    // Resume method after all Url have been added
    function AddUrl(const aRoot, aPort: RawByteString; Https: boolean=false;
      const aDomainName: RawByteString='*'): integer;
    /// will authorize a specified URL prefix
    // - will allow to call AddUrl() later for any user on the computer
    // - if aRoot is left '', it will authorize any root for this port
    // - must be called with Administrator rights: this class function is to be
    // used in a Setup program for instance, especially under Vista or Seven,
    // to reserve the Url for the server
    // - add a new record to the http.sys URL reservation store
    // - return '' on success, an error message otherwise
    // - will first delete any matching rule for this URL prefix
    // - if OnlyDelete is true, will delete but won't add the new authorization;
    // in this case, any error message at deletion will be returned 
    class function AddUrlAuthorize(const aRoot, aPort: RawByteString; Https: boolean=false;
      const aDomainName: RawByteString='*'; OnlyDelete: boolean=false): string;
    /// will register a compression algorithm
    // - overriden method which will handle any cloned instances
    procedure RegisterCompress(aFunction: THttpSocketCompress); override;
  end;

  /// main HTTP server Thread
  // - bind to a port and listen to incoming requests
  // - assign this requests to THttpServerResp threads
  // - it implements a HTTP/1.1 compatible server, according to RFC 2068 specifications
  // - if the client is also HTTP/1.1 compatible, KeepAlive connection is handled:
  //  multiple requests will use the existing connection and thread;
  //  this is faster and uses less resources, especialy under Windows
  // - a Thread Pool is used internaly to speed up HTTP/1.0 connections
  // - don't forget to use Free procedure when you are finished
  THttpServer = class(THttpServerGeneric)
  protected
    /// used to protect Process() call
    ProcessCS: TRTLCriticalSection;
{$ifdef USETHREADPOOL}
    /// the associated Thread Pool
    ThreadPool: TSynThreadPoolTHttpServer;
{$endif}
    /// server main loop - don't change directly
    procedure Execute; override;
    /// this method is called on every new client connection, i.e. every time
    // a THttpServerResp thread is created with a new incoming socket
    procedure OnConnect; virtual;
    /// this method is called on every client disconnection to update stats
    procedure OnDisconnect; virtual;
    /// override this function in order to low-level process the request;
    // default process is to get headers, and call public function Request
    procedure Process(ClientSock: THttpServerSocket); virtual;
  public
    /// contains the main server Socket
    // - it's a raw TCrtSocket, which only need a socket to be bound, listening
    // and accept incoming request
    // - THttpServerSocket are created on the fly for every request, then
    // a THttpServerResp thread is created for handling this THttpServerSocket
    Sock: TCrtSocket;
    /// will contain the total number of connection to the server
    // - it's the global count since the server started
    ServerConnectionCount: cardinal;
    /// time, in milliseconds, for the HTTP.1/1 connections to be kept alive;
    // default is 3000 ms
    ServerKeepAliveTimeOut: cardinal;
    /// TCP/IP prefix to mask HTTP protocol
    // - if not set, will create full HTTP/1.0 or HTTP/1.1 compliant content
    // - in order to make the TCP/IP stream not HTTP compliant, you can specify
    // a prefix which will be put before the first header line: in this case,
    // the TCP/IP stream won't be recognized as HTTP, and will be ignored by
    // most AntiVirus programs, and increase security - but you won't be able
    // to use an Internet Browser nor AJAX application for remote access any more
    TCPPrefix: RawByteString;

    /// create a Server Thread, binded and listening on a port
    // - this constructor will raise a EHttpServer exception if binding failed
    // - you can specify a number of threads to be initialized to handle
    // incoming connections (default is 32, which may be sufficient for most
    // cases, maximum is 64)
    constructor Create(const aPort: AnsiString
      {$ifdef USETHREADPOOL}; ServerThreadPoolCount: integer=32{$endif});
    /// release all memory and handlers
    destructor Destroy; override;
  end;

{$ifdef USEWININET}
  {/ a class to handle HTTP/1.1 request using either WinINet, either WinHTTP API
    - has a common behavior as THttpClientSocket()
    - this abstract class will be implemented e.g. with TWinINet or TWinHttp }
  TWinHttpAPI = class
  protected
    fServer: AnsiString;
    fProxyName: AnsiString;
    fProxyByPass: AnsiString;
    fPort: cardinal;
    fHttps: boolean;
    fKeepAlive: cardinal;
    /// used by RegisterCompress method
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: RawByteString;
    /// set index of protocol in fCompress[], from ACCEPT-ENCODING: header
    fCompressHeader: THttpSocketCompressSet;
    /// used for internal connection
    fSession, fConnection, fRequest: HINTERNET;
    procedure InternalConnect; virtual; abstract;
    procedure InternalRequest(const method, aURL: RawByteString); virtual; abstract;
    procedure InternalCloseRequest; virtual; abstract;
    procedure InternalAddHeader(const hdr: RawByteString); virtual; abstract;
    procedure InternalSendRequest(const aData: RawByteString); virtual; abstract;
    function InternalGetInfo(Info: DWORD): RawByteString; virtual; abstract;
    function InternalGetInfo32(Info: DWORD): DWORD; virtual; abstract;
    function InternalReadData(var Data: RawByteString; Read: integer): cardinal; virtual; abstract;
  public
    /// connect to http://aServer:aPort or https://aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy
    constructor Create(const aServer, aPort: AnsiString; aHttps: boolean;
      const aProxyName: AnsiString=''; const aProxyByPass: AnsiString='');
    /// low-level HTTP/1.1 request
    // - after an Create(server,port), return 200,202,204 if OK,
    // http status error otherwize
    function Request(const url, method: RawByteString; KeepAlive: cardinal;
      const InHeader, InData, InDataType: RawByteString;
      out OutHeader, OutData: RawByteString): integer;
    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlzo/synlz) protocols
    // - returns true on success, false if this function or this
    // ACCEPT-ENCODING: header was already registered
    // - the first registered algorithm will be the prefered one for compression
    function RegisterCompress(aFunction: THttpSocketCompress): boolean;
    /// the remote server host name, as stated specified to the class constructor
    property Server: AnsiString read fServer;
    /// the remote server port number, as specified to the class constructor
    property Port: cardinal read fPort;
    /// if the remote server uses HTTPS, as specified to the class constructor
    property Https: boolean read fHttps;
    /// the remote server optional proxy, as specified to the class constructor
    property ProxyName: AnsiString read fProxyName;
    /// the remote server optional proxy by-pass list, as specified to the class
    // constructor
    property ProxyByPass: AnsiString read fProxyByPass;
  end;

  {/ a class to handle HTTP/1.1 request using the WinINet API
   - has a common behavior as THttpClientSocket()
   - The Microsoft Windows Internet (WinINet) application programming interface
     (API) enables applications to access standard Internet protocols, such as
     FTP and HTTP/HTTPS.
   - by design, the WinINet API should not be used from a service
   - note: WinINet is MUCH slower than THttpClientSocket: do not use this, only
     if you find some performance improvements on some networks }
  TWinINet = class(TWinHttpAPI)
  protected
    // those internal methods will raise an EWinINet exception on error
    procedure InternalConnect; override;
    procedure InternalRequest(const method, aURL: RawByteString); override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: RawByteString); override;
    procedure InternalSendRequest(const aData: RawByteString); override;
    function InternalGetInfo(Info: DWORD): RawByteString; override;
    function InternalGetInfo32(Info: DWORD): DWORD; override;
    function InternalReadData(var Data: RawByteString; Read: integer): cardinal; override;
  public
    /// relase the connection
    destructor Destroy; override;
  end;

  /// WinINet exception type
  EWinINet = class(Exception)
  public
    /// create a WinINet exception, with the error message as text
    constructor Create;
  end;

  {/ a class to handle HTTP/1.1 request using the WinHTTP API
   - has a common behavior as THttpClientSocket() but seems to be faster
     over a network and is able to retrieve the current proxy settings
     (if available) and handle secure https connection - so it seems to be the
     class to use in your client programs
   - WinHTTP does not share any proxy settings with Internet Explorer.
     The WinHTTP proxy configuration is set by either
     proxycfg.exe on Windows XP and Windows Server 2003 or earlier, either
     netsh.exe on Windows Vista and Windows Server 2008 or later; for instance,
     you can run "proxycfg -u" or "netsh winhttp import proxy source=ie" to use
     the current user's proxy settings for Internet Explorer (under 64 bit
     Vista/Seven, to configure applications using the 32 bit WinHttp settings,
     call netsh or proxycfg bits from %SystemRoot%\SysWOW64 folder explicitely)
   - Microsoft Windows HTTP Services (WinHTTP) is targeted at middle-tier and
     back-end server applications that require access to an HTTP client stack }
  TWinHTTP = class(TWinHttpAPI)
  protected
    // those internal methods will raise an EOSError exception on error 
    procedure InternalConnect; override;
    procedure InternalRequest(const method, aURL: RawByteString); override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: RawByteString); override;
    procedure InternalSendRequest(const aData: RawByteString); override;
    function InternalGetInfo(Info: DWORD): RawByteString; override;
    function InternalGetInfo32(Info: DWORD): DWORD; override;
    function InternalReadData(var Data: RawByteString; Read: integer): cardinal; override;
  public
    /// relase the connection
    destructor Destroy; override;
  end;

  /// type of a TWinHttpAPI class
  TWinHttpAPIClass = class of TWinHttpAPI;
  
{$endif}

const
  /// used by THttpApiServer.Request for http.sys to send a static file
  // - the OutCustomHeader should contain the proper 'Content-type: ....'
  // corresponding to the file 
  HTTP_RESP_STATICFILE = '!STATICFILE';

/// create a TCrtSocket, returning nil on error
// (usefull to easily catch socket error exception ECrtSocket)
function Open(const aServer, aPort: AnsiString): TCrtSocket;

/// create a THttpClientSocket, returning nil on error
// (usefull to easily catch socket error exception ECrtSocket)
function OpenHttp(const aServer, aPort: AnsiString): THttpClientSocket;

/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
function HttpGet(const server, port: AnsiString; const url: RawByteString): RawByteString;

/// send some data to a remote web server, using the HTTP/1.1 protocol and POST method
function HttpPost(const server, port: AnsiString; const url, Data, DataType: RawByteString): boolean;

/// send an email using the SMTP protocol
// - retry true on success
function SendEmail(const Server: AnsiString; const From, CSVDest, Subject, Text: RawByteString;
  const Headers: RawByteString=''; const User: RawByteString=''; const Pass: RawByteString='';
  const Port: AnsiString='25'): boolean;

/// retrieve the HTTP reason text from a code
// - e.g. StatusCodeToReason(200)='OK'
function StatusCodeToReason(Code: integer): RawByteString;

/// retrieve the IP adress from a computer name
function ResolveName(const Name: AnsiString): AnsiString;

/// Base64 encoding of a string
function Base64Encode(const s: RawByteString): RawByteString;

/// Base64 decoding of a string
function Base64Decode(const s: RawByteString): RawByteString;


{$ifdef Win32}
/// remotly get the MAC address of a computer, from its IP Address
// - only works under Win2K and later
// - return the MAC address as a 12 hexa chars ('0050C204C80A' e.g.)
function GetRemoteMacAddress(const IP: AnsiString): RawByteString;
{$endif}


implementation

function StatusCodeToReason(Code: integer): RawByteString;
begin
  case Code of
    100: result := 'Continue';
    200: result := 'OK';
    201: result := 'Created';
    202: result := 'Accepted';
    203: result := 'Non-Authoritative Information';
    204: result := 'No Content';
    300: result := 'Multiple Choices';
    301: result := 'Moved Permanently';
    302: result := 'Found';
    303: result := 'See Other';
    304: result := 'Not Modified';
    307: result := 'Temporary Redirect';
    400: result := 'Bad Request';
    401: result := 'Unauthorized';
    403: result := 'Forbidden';
    404: result := 'Not Found';
    405: result := 'Method Not Allowed';
    406: result := 'Not Acceptable';
    500: result := 'Internal Server Error';
    503: result := 'Service Unavailable';
    else str(Code,result);
  end;
end;

function Hex2Dec(c: AnsiChar): byte;
begin
  case c of
  'A'..'Z': result := Ord(c) - (Ord('A') - 10);
  'a'..'z': result := Ord(c) - (Ord('a') - 10);
  '0'..'9': result := Ord(c) - Ord('0');
  else result := 255;
  end;
end;

// Base64 string encoding
function Base64Encode(const s: RawByteString): RawByteString;
procedure Encode(rp, sp: PAnsiChar; len: integer);
const
  b64: array[0..63] of AnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var i: integer;
    c: cardinal;
begin
  for i := 1 to len div 3 do begin
    c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8 + ord(sp[2]);
    rp[0] := b64[(c shr 18) and $3f];
    rp[1] := b64[(c shr 12) and $3f];
    rp[2] := b64[(c shr 6) and $3f];
    rp[3] := b64[c and $3f];
    inc(rp,4);
    inc(sp,3);
  end;
  case len mod 3 of
    1: begin
      c := ord(sp[0]) shl 16;
      rp[0] := b64[(c shr 18) and $3f];
      rp[1] := b64[(c shr 12) and $3f];
      rp[2] := '=';
      rp[3] := '=';
    end;
    2: begin
      c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8;
      rp[0] := b64[(c shr 18) and $3f];
      rp[1] := b64[(c shr 12) and $3f];
      rp[2] := b64[(c shr 6) and $3f];
      rp[3] := '=';
    end;
  end;
end;
var len: integer;
begin
  result:='';
  len := length(s);
  if len = 0 then exit;
  SetLength(result, ((len + 2) div 3) * 4);
  Encode(pointer(result),pointer(s),len);
end;

function Base64Decode(const s: RawByteString): RawByteString;
var i, j, len: integer;
    sp, rp: PAnsiChar;
    c, ch: integer;
begin
  result:= '';
  len := length(s);
  if (len = 0) or (len mod 4 <> 0) then
    exit;
  len := len shr 2;
  SetLength(result, len * 3); 
  sp := pointer(s); 
  rp := pointer(result); 
  for i := 1 to len do begin 
    c := 0; 
    j := 0; 
    while true do begin
      ch := ord(sp[j]);
      case chr(ch) of
        'A'..'Z' : c := c or (ch - ord('A'));
        'a'..'z' : c := c or (ch - (ord('a')-26));
        '0'..'9' : c := c or (ch - (ord('0')-52));
        '+' : c := c or 62;
        '/' : c := c or 63;
        else
        if j=3 then begin
          rp[0] := AnsiChar(c shr 16);
          rp[1] := AnsiChar(c shr 8);
          SetLength(result, len*3-1);
          exit;
        end else begin
          rp[0] := AnsiChar(c shr 10);
          SetLength(result, len*3-2);
          exit;
        end;
      end;
      if j=3 then break;
      inc(j);
      c := c shl 6;
    end;
    rp[2] := AnsiChar(c);
    c := c shr 8;
    rp[1] := AnsiChar(c);
    c := c shr 8;
    rp[0] := AnsiChar(c);
    inc(rp,3);
    inc(sp,4);
  end;  
end;

function ResolveName(const Name: AnsiString): AnsiString;
var l: TStringList;
begin
  l := TStringList.Create;
  try
    // use AF_INET+PF_INET instead of AF_UNSPEC+PF_UNSPEC: IP6 is buggy!
    ResolveNameToIP(Name, AF_INET, PF_INET, SOCK_STREAM, l);
    if l.Count=0 then
      result := Name else
      result := AnsiString(l[0]);
  finally
    l.Free;
  end;
end;

function CallServer(const Server, Port: AnsiString; doBind: boolean;
   aLayer: TCrtSocketLayer): TSocket;
var Sin: TVarSin;
    IP: AnsiString;
    li: TLinger;
    SOCK_TYPE, IPPROTO: integer;
{$ifdef LINUX}
    serveraddr: sockaddr;
{$endif}
begin
  result := -1;
  case aLayer of
    cslTCP: begin
      SOCK_TYPE := SOCK_STREAM;
      IPPROTO := IPPROTO_TCP;
    end;
    cslUDP: begin
      SOCK_TYPE := SOCK_DGRAM;
      IPPROTO := IPPROTO_UDP;
    end;
    cslUNIX: begin
{$ifndef LINUX}
      exit; // not handled under Win32
{$else} // special version for UNIX sockets 
      result := socket(AF_UNIX,SOCK_STREAM,0);
      if result<0 then
        exit;
      if doBind then begin
        fillchar(serveraddr,sizeof(serveraddr),0);
http://publib.boulder.ibm.com/infocenter/iseries/v5r3/index.jsp?topic=/rzab6/rzab6uafunix.htm
        serveraddr.
        if (bind(result,@serveraddr,sizeof(serveraddr))<0) or
           (listen(result,SOMAXCONN)<0) then begin
          close(sd);
          result := -1;
        end;
      end;
      exit;
{$endif}
    end;
    else exit; // make this stupid compiler happy
  end;
  IP := ResolveName(Server);
  // use AF_INET+PF_INET instead of AF_UNSPEC+PF_UNSPEC: IP6 is buggy!
  if SetVarSin(Sin, IP, Port, AF_INET, PF_INET, SOCK_TYPE, true)<>0 then
    exit;
  result := Socket(integer(Sin.AddressFamily), SOCK_TYPE, IPPROTO);
  if result=-1 then
    exit;
  if doBind then begin
    // Socket should remain open for 5 seconds after a closesocket() call
    li.l_onoff := Ord(true);
    li.l_linger := 5;
    SetSockOpt(result, SOL_SOCKET, SO_LINGER, @li, SizeOf(li));
    // bind and listen to this port
    if (Bind(result, Sin)<>0) or
       ((aLayer<>cslUDP) and (Listen(result, SOMAXCONN)<>0)) then begin
      CloseSocket(result);
      result := -1;
    end;
  end else
  if Connect(result,Sin)<>0 then begin
     CloseSocket(result);
     result := -1;
  end;
end;

function OutputSock(var F: TTextRec): integer;
var Index, Size: integer;
    Sock: TCRTSocket;
begin
  if F.BufPos<>0 then begin
    result := -1; // on socket error -> raise ioresult error
    Sock := TCrtSocket(F.Handle);
    if (Sock=nil) or (Sock.Sock=-1) then
      exit; // file closed
    Index := 0;
    repeat
      Size := Send(Sock.Sock, @F.BufPtr[Index], F.BufPos, 0);
      if Size<=0 then
        exit;
      inc(Sock.BytesOut, Size);
      dec(F.BufPos,Size);
      inc(Index,Size);
    until F.BufPos=0;
  end;
  result := 0; // no error
end;

function InputSock(var F: TTextRec): Integer;
// SockIn pseudo text file fill its internal buffer only with available data
// -> no unwanted wait time is added
// -> very optimized use for readln() in HTTP stream
var Size: integer;
    Sock: TCRTSocket;
begin
  F.BufEnd := 0;
  F.BufPos := 0;
  result := -1; // on socket error -> raise ioresult error
  Sock := TCrtSocket(F.Handle);
  if (Sock=nil) or (Sock.Sock=-1) then
    exit; // file closed = no socket -> error
  if Sock.TimeOut<>0 then begin // wait for pending data?
    Size := Sock.SockCanRead(Sock.TimeOut); // Size>0 if some data pending in
    if Size<=0 then begin // socket error or time out
      Sock.SockInEof := true; // mark end of SockIn
      if Size<0 then // socket error
        result := -WSAGetLastError(); // update ioresult and raise {$I+} exception
      exit;
    end;
    IOCtlSocket(Sock.Sock, FIONREAD, Size); // get exact count
    if Size>integer(F.BufSize) then
      Size := F.BufSize;
  end else
    Size := F.BufSize;
  Size := Recv(Sock.Sock, F.BufPtr, Size, 0);
  // Recv() may return Size=0 if no data is pending, but no TCP/IP error
  if Size>=0 then begin
    F.BufEnd := Size;
    inc(Sock.BytesIn, Size);
    result := 0; // no error
  end else begin
    Sock.SockInEof := true; // error -> mark end of SockIn
    result := -WSAGetLastError();
    // result <0 will update ioresult and raise an exception if {$I+}
  end;
end;

function CloseSock(var F: TTextRec): integer;
var Sock: TCRTSocket;
begin
  Sock := TCrtSocket(F.Handle);
  if Sock<>nil then
    Sock.Close;
  F.Handle := 0; // Sock := nil
  Result := 0;
end;

function OpenSock(var F: TTextRec): integer;
begin
  F.BufPos := 0;
  F.BufEnd := 0;
  if F.Mode=fmInput then begin // ReadLn
    F.InOutFunc := @InputSock;
    F.FlushFunc := nil;
  end else begin               // WriteLn
    F.Mode := fmOutput;
    F.InOutFunc := @OutputSock;
    F.FlushFunc := @OutputSock;
  end;
  F.CloseFunc := @CloseSock;
  Result := 0;
end;


{ TCrtSocket }

constructor TCrtSocket.Bind(const aPort: AnsiString; aLayer: TCrtSocketLayer=cslTCP);
begin
  OpenBind('0.0.0.0',aPort,true,-1,aLayer); // raise an ECrtSocket exception on error
end;

constructor TCrtSocket.Open(const aServer, aPort: AnsiString; aLayer: TCrtSocketLayer);
begin
  TimeOut := 5000; // default read timeout is 5 sec
  OpenBind(aServer,aPort,false,-1,aLayer); // raise an ECrtSocket exception on error
end;

procedure TCrtSocket.Close;
begin
  if (SockIn<>nil) or (SockOut<>nil) then begin
    ioresult; // reset ioresult value if SockIn/SockOut were used
    if SockIn<>nil then begin
      TTextRec(SockIn^).BufPos := 0;  // reset input buffer
      TTextRec(SockIn^).BufEnd := 0;
    end;
    if SockOut<>nil then begin
      TTextRec(SockOut^).BufPos := 0; // reset output buffer
      TTextRec(SockOut^).BufEnd := 0;
    end;
  end;
  if Sock=-1 then
    exit; // no opened connection to close
  Shutdown(Sock,1);
  CloseSocket(Sock);
  Sock := -1; // don't change Server or Port, since may try to reconnect
end;

procedure TCrtSocket.OpenBind(const aServer, aPort: AnsiString;
  doBind: boolean; aSock: integer=-1; aLayer: TCrtSocketLayer=cslTCP);
const BINDTXT: array[boolean] of string = ('open','bind');
begin
  if aPort='' then
    Port := '80' else // default port is 80 (HTTP)
    Port := aPort;
  if aSock<0 then
    Sock := CallServer(aServer,Port,doBind,aLayer) else // OPEN or BIND
    Sock := aSock; // ACCEPT mode -> socket is already created by caller
  if Sock=-1 then
    raise ECrtSocket.CreateFmt('Socket %s creation error on %s:%s (%d)',
      [BINDTXT[doBind],aServer,Port,WSAGetLastError]);
  Server := aServer;
end;

const CRLF: array[0..1] of AnsiChar = (#13,#10);

procedure TCrtSocket.SockSend(const Values: array of const);
var i: integer;
    tmp: shortstring;
begin
  for i := 0 to high(Values) do
  with Values[i] do
  case VType of
    vtString:     Snd(@VString^[1], pByte(VString)^);
    vtAnsiString: Snd(VAnsiString, length(RawByteString(VAnsiString)));
{$ifdef UNICODE}
    vtUnicodeString: begin
      tmp := shortstring(UnicodeString(VUnicodeString)); // convert into ansi (max length 255)
      Snd(@tmp[1],length(tmp));
    end;
{$endif}
    vtPChar:      Snd(VPChar, StrLen(VPChar));
    vtChar:       Snd(@VChar, 1);
    vtWideChar:   Snd(@VWideChar,1); // only ansi value
    vtInteger:    begin
      Str(VInteger,tmp);
      Snd(@tmp[1],length(tmp));
    end;
  end;
  Snd(@CRLF, 2);
end;

procedure TCrtSocket.SockSend(const Line: RawByteString);
begin
  if Line<>'' then
    Snd(pointer(Line),length(Line));
  Snd(@CRLF, 2);
end;

procedure TCrtSocket.SockSendFlush;
begin
  if SndBufLen=0 then
    exit;
  SndLow(pointer(SndBuf), SndBufLen);
  SndBufLen := 0;
end;

procedure TCrtSocket.SndLow(P: pointer; Len: integer);
begin
  if not TrySndLow(P,Len) then
    raise ECrtSocket.Create('SndLow');
end;

function TCrtSocket.TrySndLow(P: pointer; Len: integer): boolean;
var SentLen: integer;
begin
  result := false;
  if (self=nil) or (Len<=0) or (P=nil) then
    exit;
  if (TimeOut<>0) and (SockCanWrite(TimeOut)<=0) then
    exit;
  repeat
    SentLen := Send(Sock, P, Len, 0);
    if SentLen<0 then
      exit;
    dec(Len,SentLen);
    if Len<=0 then break;
    inc(BytesOut,SentLen);
    inc(PtrUInt(P),SentLen);
  until false;
  result := true;
end;

procedure TCrtSocket.Write(const Data: RawByteString);
begin
  SndLow(pointer(Data),length(Data));
end;

function TCrtSocket.SockCanRead(aTimeOut: cardinal): integer;
var TimeV: TTimeVal;
    FDSet: TFDSet;
begin
  // init select() parameters
  TimeV.tv_usec := (aTimeout mod 1024) * 1024;
  TimeV.tv_sec := aTimeout div 1024;
  FDSet.fd_count := 1; // is changed by select() -> recreate each time
  FDSet.fd_array[0] := Sock;
  result := select(Sock+1, @FDSet, nil, nil, @TimeV);
end;

function TCrtSocket.SockCanWrite(aTimeOut: cardinal): integer;
var TimeV: TTimeVal;
    FDSet: TFDSet;
begin
  // init select() parameters
  TimeV.tv_usec := (aTimeout mod 1024) * 1024;
  TimeV.tv_sec := aTimeout div 1024;
  FDSet.fd_count := 1; // is changed by select() -> recreate each time
  FDSet.fd_array[0] := Sock;
  // ask if any data can be written
  result := select(Sock+1, nil, @FDSet, nil, @TimeV);
end;

function TCrtSocket.SockInRead(Content: PAnsiChar; Length: integer): integer;
// read Length bytes from SockIn^ buffer + Sock if necessary
begin
  // get data from SockIn buffer, if any (faster than ReadChar)
  if SockIn<>nil then
  with TTextRec(SockIn^) do begin
    result := BufEnd-BufPos;
    if result>0 then begin
      if result>Length then
        result := Length;
      move(BufPtr[BufPos],Content^,result);
      inc(BufPos,result);
      inc(Content,result);
      dec(Length,result);
    end;
  end else
    result := 0;
  // direct receiving of the triming bytes from socket
  if Length>0 then begin
    SockRecv(Content,Length);
    inc(result,Length);
  end;
end;

destructor TCrtSocket.Destroy;
begin
  Close;
  if SockIn<>nil then
    Freemem(SockIn);
  if SockOut<>nil then
    Freemem(SockOut);
  inherited;
end;

procedure TCrtSocket.Snd(P: pointer; Len: integer);
begin
  if Len<=0 then
    exit;
  if PByte(SndBuf)=nil then
    if Len<2048 then // 2048 is about FASTMM4 small block size
      SetLength(SndBuf,2048) else
      SetLength(SndBuf,Len) else
    if Len+SndBufLen>pInteger(PAnsiChar(pointer(SndBuf))-4)^ then
      SetLength(SndBuf,pInteger(PAnsiChar(pointer(SndBuf))-4)^+Len+2048);
  move(P^,PAnsiChar(pointer(SndBuf))[SndBufLen],Len);
  inc(SndBufLen,Len);
end;

const
  SOCKBUFSIZE = 1024; // big enough for headers (content will be read directly)

procedure TCrtSocket.CreateSockIn(LineBreak: TTextLineBreakStyle);
begin
  if (Self=nil) or (SockIn<>nil) then
    exit; // initialization already occured
  Getmem(SockIn,SOCKBUFSIZE+(sizeof(TTextRec)-sizeof(TTextRec(SockIn^).buffer)));
  with TTextRec(SockIn^) do begin
    Handle := integer(self);
    Mode := fmClosed;
    BufSize := SOCKBUFSIZE;
    BufPtr := @Buffer;
    OpenFunc := @OpenSock;
  end;
{$ifdef CONDITIONALEXPRESSIONS}
  SetLineBreakStyle(SockIn^,LineBreak); // http does break lines with #13#10
{$endif}
  Reset(SockIn^);
end;

procedure TCrtSocket.CreateSockOut;
begin
  if SockOut<>nil then
    exit; // initialization already occured
  Getmem(SockOut,SOCKBUFSIZE+(sizeof(TTextRec)-sizeof(TTextRec(SockIn^).Buffer)));
  with TTextRec(SockOut^) do begin
    Handle := integer(self);
    Mode := fmClosed;
    BufSize := SOCKBUFSIZE;
    BufPtr := @Buffer;
    OpenFunc := @OpenSock;
  end;
{$ifdef CONDITIONALEXPRESSIONS}
  SetLineBreakStyle(SockOut^,tlbsCRLF);
{$endif}
  Rewrite(SockOut^);
end;

procedure TCrtSocket.SockRecv(Buffer: pointer; Length: integer);
begin
  if not TrySockRecv(Buffer,Length) then
    raise ECrtSocket.Create('SockRecv');
end;

function TCrtSocket.TrySockRecv(Buffer: pointer; Length: integer): boolean;
var Size: PtrInt;
begin
  result := false;
  if self=nil then exit;
  if (Buffer<>nil) and (Length>0) then
  repeat
    if TimeOut<>0 then begin // wait for pending data?
      Size := SockCanRead(TimeOut); // Size>0 if some data pending in
      if Size<=0 then // socket error or time out
        exit;
    end;
    Size := Recv(Sock, Buffer, Length, 0);
    if Size<=0 then
      exit;
    inc(BytesIn, Size);
    dec(Length,Size);
    inc(PByte(Buffer),Size);
  until Length=0;
  result := true;
end;

procedure TCrtSocket.SockRecvLn(out Line: RawByteString; CROnly: boolean=false);
procedure RecvLn(var Line: RawByteString);
var P: PAnsiChar;
    LP, L: PtrInt;
    tmp: array[0..1023] of AnsiChar; // avoid ReallocMem() every char
begin
  P := @tmp;
  Line := '';
  repeat
    SockRecv(P,1); // this is very slow under Windows -> use SockIn^ instead
    if P^<>#13 then // at least NCSA 1.3 does send a #10 only -> ignore #13
      if P^=#10 then begin
        if Line='' then // get line
          SetString(Line,tmp,P-tmp) else begin
          LP := P-tmp; // append to already read chars
          L := length(Line);
          Setlength(Line,L+LP);
          move(tmp,(PAnsiChar(pointer(Line))+L)^,LP);
        end;
        exit;
      end else
      if P=@tmp[1023] then begin // tmp[] buffer full?
        L := length(Line); // -> append to already read chars
        Setlength(Line,L+1024);
        move(tmp,(PAnsiChar(pointer(Line))+L)^,1024);
        P := tmp;
      end else
        inc(P);
  until false;
end;
var c: AnsiChar;
   Error: integer;
begin
  if CROnly then begin // slow but accurate version which expect #13 as line end
    // SockIn^ expect either #10, either #13#10 -> a dedicated version is needed
    repeat
      SockRecv(@c,1); // this is slow but works
      if c in [#0,#13] then
        exit; // end of line
      Line := Line+c; // will do the work anyway
    until false;
  end else
  if SockIn<>nil then begin
    {$I-}
    readln(SockIn^,Line); // example: HTTP/1.0 200 OK
    Error := ioresult;
    if Error<>0 then
      raise ECrtSocket.Create('SockRecvLn',Error);
    {$I+}
  end else
    RecvLn(Line); // slow under Windows -> use SockIn^ instead
end;

procedure TCrtSocket.SockRecvLn;
var c: AnsiChar;
    Error: integer;
begin
  if SockIn<>nil then begin
    {$I-}
    readln(SockIn^);
    Error := ioresult;
    if Error<>0 then
      raise ECrtSocket.Create('SockRecvLn',Error);
    {$I+}
  end else
    repeat
      SockRecv(@c,1);
    until c=#10;
end;

function TCrtSocket.SockConnected: boolean;
var Sin: TVarSin;
begin
  result := GetPeerName(Sock,Sin)=0;
end;

function TCrtSocket.SockReceiveString(aTimeOut: integer): RawByteString;
var Size, L, Read: integer;
begin
  result := '';
  if self=nil then
    exit;
  if (aTimeOut<>0) and (SockCanRead(aTimeOut)<=0) then
    exit;
  L := 0;
  repeat
    sleep(0);
    if IOCtlSocket(Sock, FIONREAD, Size)<>0 then // get exact count
      exit;
    if Size=0 then // connection broken
      if (TimeOut=0) and (result='') then begin
        // TimeOut=0 -> wait till something
        SockCanRead(100); // 100 ms delay in infinite loop
        continue;
      end else
        break;
    SetLength(result,L+Size); // append to result
    Read := recv(Sock,PAnsiChar(pointer(result))+L,Size,0);
    inc(L,Read);
    if Read<Size then
      SetLength(result,L); // e.g. Read=0 may happen
  until false;
end;


{ THttpClientSocket }

function THttpClientSocket.Delete(const url: RawByteString; KeepAlive: cardinal;
  const header: RawByteString): integer;
begin
  result := Request(url,'DELETE',KeepAlive,header,'','',false);
end;

function THttpClientSocket.Get(const url: RawByteString; KeepAlive: cardinal=0; const header: RawByteString=''): integer;
begin
  result := Request(url,'GET',KeepAlive,header,'','',false);
end;

function THttpClientSocket.Head(const url: RawByteString; KeepAlive: cardinal;
  const header: RawByteString): integer;
begin
  result := Request(url,'HEAD',KeepAlive,header,'','',false);
end;

function THttpClientSocket.Post(const url, Data, DataType: RawByteString; KeepAlive: cardinal;
  const header: RawByteString): integer;
begin
  result := Request(url,'POST',KeepAlive,header,Data,DataType,false);
end;

function THttpClientSocket.Put(const url, Data, DataType: RawByteString;
  KeepAlive: cardinal; const header: RawByteString): integer;
begin
  result := Request(url,'PUT',KeepAlive,header,Data,DataType,false);
end;

function IdemPChar(p, up: pAnsiChar): boolean;
// if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
var c: AnsiChar;
begin
  result := false;
  if (p=nil) or (up=nil) then
    exit;
  while up^<>#0 do begin
    c := p^;
    if up^<>c then
      if c in ['a'..'z'] then begin
        dec(c,32);
        if up^<>c then
          exit;
      end else exit;
    inc(up);
    inc(p);
  end;
  result := true;
end;

function GetNextItem(var P: PAnsiChar; Sep: AnsiChar = ','): RawByteString;
// return next CSV string in P, nil if no more
var S: PAnsiChar;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while (S^<>#0) and (S^<>Sep) do
      inc(S);
    SetString(result,P,S-P);
    if S^<>#0 then
     P := S+1 else
     P := nil;
  end;
end;

function GetNextItemUInt64(var P: PAnsiChar): Int64;
var c: PtrUInt;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  result := byte(P^)-48;  // caller ensured that P^ in ['0'..'9']
  inc(P);
  repeat
    c := byte(P^)-48;
    if c>9 then
      break else
      result := result*10+c;
    inc(P);
  until false;
end; // P^ will point to the first non digit char

function GetNextLine(var P: PAnsiChar): RawByteString;
var S: PAnsiChar;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while S^>=' ' do
      inc(S);
    SetString(result,P,S-P);
    while (S^<>#0) and (S^<' ') do inc(S); // ignore e.g. #13 or #10
    if S^<>#0 then
      P := S else
      P := nil;
  end;
end;

{$ifdef UNICODE}
// rewrite some functions to avoid unattempted ansi<->unicode conversion

function Trim(const S: RawByteString): RawByteString;
asm  // fast implementation by John O'Harrow
  test eax,eax                   {S = nil?}
  xchg eax,edx
  jz   System.@LStrClr           {Yes, Return Empty String}
  mov  ecx,[edx-4]               {Length(S)}
  cmp  byte ptr [edx],' '        {S[1] <= ' '?}
  jbe  @@TrimLeft                {Yes, Trim Leading Spaces}
  cmp  byte ptr [edx+ecx-1],' '  {S[Length(S)] <= ' '?}
  jbe  @@TrimRight               {Yes, Trim Trailing Spaces}
  jmp  System.@LStrLAsg          {No, Result := S (which occurs most time)}
@@TrimLeft:                      {Strip Leading Whitespace}
  dec  ecx
  jle  System.@LStrClr           {All Whitespace}
  inc  edx
  cmp  byte ptr [edx],' '
  jbe  @@TrimLeft
@@CheckDone:
  cmp  byte ptr [edx+ecx-1],' '
{$ifdef UNICODE}
  jbe  @@TrimRight
  push 65535 // RawByteString code page for Delphi 2009 and up
  call  System.@LStrFromPCharLen // we need a call, not a direct jmp
  ret
{$else}
  ja   System.@LStrFromPCharLen
{$endif}
@@TrimRight:                     {Strip Trailing Whitespace}
  dec  ecx
  jmp  @@CheckDone
end;

function UpperCase(const S: RawByteString): RawByteString;
procedure Upper(Source, Dest: PAnsiChar; L: cardinal);
var Ch: AnsiChar; // this sub-call is shorter and faster than 1 plain proc
begin
  repeat
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then
      dec(Ch, 32);
    Dest^ := Ch;
    dec(L);
    inc(Source);
    inc(Dest);
  until L=0;
end;
var L: cardinal;
begin
  result := '';
  L := Length(S);
  if L=0 then
    exit;
  SetLength(result, L);
  Upper(pointer(S),pointer(result),L);
end;

{$endif}

function GetCardinal(P: PAnsiChar): cardinal; overload;
var c: cardinal;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  if P^=' ' then repeat inc(P) until P^<>' ';
  c := byte(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then
        break else
        result := result*10+c;
      inc(P);
    until false;
  end;
end;

function GetCardinal(P,PEnd: PAnsiChar): cardinal; overload;
var c: cardinal;
begin
  result := 0;
  if (P=nil) or (P>=PEnd) then
    exit;
  if P^=' ' then repeat
    inc(P);
    if P=PEnd then exit;
  until P^<>' ';
  c := byte(P^)-48;
  if c>9 then
    exit;
  result := c;
  inc(P);
  while P<PEnd do begin
    c := byte(P^)-48;
    if c>9 then
      break else
      result := result*10+c;
    inc(P);
  end;
end;

const
  DEFAULT_AGENT = 'Mozilla/4.0 (compatible; MSIE 5.5; Windows; FREE)';
  
  
function THttpClientSocket.Request(const url, method: RawByteString;
  KeepAlive: cardinal; const Header, Data, DataType: RawByteString; retry: boolean): integer;
procedure DoRetry(Error: integer);
begin
  if retry then // retry once -> return error if already retried
    result := Error else begin
    Close; // close this connection
    try
      OpenBind(Server,Port,false); // then retry this request with a new socket
      result := Request(url,method,KeepAlive,Header,Data,DataType,true);
    except
      on Exception do
        result := Error;
    end;
  end;
end;
var P: PAnsiChar;
    aURL, aData: RawByteString;
begin
{$ifdef WIN32}
  if SockIn=nil then // done once
    CreateSockIn; // use SockIn by default if not already initialized: 2x faster
{$endif}
  Content := '';
  {$ifdef DEBUG2}system.write(#13#10,method,' ',url);
  if Retry then system.Write(' RETRY');{$endif}
  if Sock=-1 then
    DoRetry(404) else // socket closed (e.g. KeepAlive=0) -> reconnect
  try
  try
{$ifdef DEBUG23}system.write(' Send');{$endif}
    // send request - we use SockSend because writeln() is calling flush()
    // -> all header will be sent at once
    if TCPPrefix<>'' then
      SockSend(TCPPrefix);
    if (url='') or (url[1]<>'/') then
      aURL := '/'+url else // need valid url according to the HTTP/1.1 RFC
      aURL := url;
{$ifdef DEBUGAPI}  writeln('? ',method,' ',aurl); {$endif}
    SockSend([method, ' ', aURL, ' HTTP/1.1'#13#10+
      'Host: ', Server, ':', Port, #13#10'Accept: */*']);
    if UserAgent='' then
      SockSend('User-Agent: '+DEFAULT_AGENT) else
      SockSend(['User-Agent: ',UserAgent]);
    aData := Data; // need var for Data to be eventually compressed 
    CompressDataAndWriteHeaders(DataType,aData);
    if KeepAlive>0 then
      SockSend(['Keep-Alive: ',KeepAlive,#13#10'Connection: Keep-Alive']) else
      SockSend('Connection: Close');
    if header<>'' then
      SockSend(header);
    if fCompressAcceptEncoding<>'' then
      SockSend(fCompressAcceptEncoding);
    SockSend; // send CRLF
    {$ifdef DEBUG23} SndBuf[SndBufLen+1] := #0;
      system.Writeln(#13#10'HeaderOut ',PAnsiChar(SndBuf));{$endif}
    SockSendFlush; // flush all pending data (i.e. headers) to network
    if aData<>'' then // for POST and PUT methods: content to be sent
      SndLow(pointer(aData),length(aData)); // no CRLF at the end of data
{$ifdef DEBUG23}system.write('OK ');{$endif}
    // get headers
    SockRecvLn(Command); // will raise ECrtSocket on any error
    if TCPPrefix<>'' then
      if Command<>TCPPrefix then begin
        result :=  505;
        exit;
      end else
      SockRecvLn(Command);
{$ifdef DEBUG23}system.write(Command);{$endif}
    P := pointer(Command);
    if IdemPChar(P,'HTTP/1.') then begin
      result := GetCardinal(P+9); // get http numeric status code
      if result=0 then begin
        result :=  505;
        exit;
      end;
      while result=100 do begin
        repeat // 100 CONTINUE will just be ignored client side
          SockRecvLn(Command);
          P := pointer(Command);
        until IdemPChar(P,'HTTP/1.');  // ignore up to next command
        result := GetCardinal(P+9);
      end;
      if P[7]='0' then
        KeepAlive := 0; // HTTP/1.0 -> force connection close
    end else begin // error on reading answer
      DoRetry(505); // 505=wrong format
      exit;
    end;
    GetHeader; // read all other headers
{$ifdef DEBUG23}system.write('OK Body');{$endif}
    if not IdemPChar(pointer(method),'HEAD') then
      GetBody; // get content if necessary (not HEAD method)
{$ifdef DEBUGAPI}writeln('? ',Command,' ContentLength=',length(Content));
    if result<>200 then writeln('? ',Content,#13#10,HeaderGetText); {$endif}
  except
    on Exception do
      DoRetry(404);
  end;
  finally
    if KeepAlive=0 then
      Close;
  end;
end;

function Open(const aServer, aPort: AnsiString): TCrtSocket;
begin
  try
    result := TCrtSocket.Open(aServer,aPort);
  except
    on ECrtSocket do
      result := nil;
  end;
end;

function OpenHttp(const aServer, aPort: AnsiString): THttpClientSocket;
begin
  try
    result := THttpClientSocket.Open(aServer,aPort);
  except
    on ECrtSocket do
      result := nil;
  end;
end;

function HttpGet(const server, port: AnsiString; const url: RawByteString): RawByteString;
var Http: THttpClientSocket;
begin
  result := '';
  Http := OpenHttp(server,port);
  if Http<>nil then
  try
    if Http.Get(url)=200 then
      result := Http.Content;
  finally
    Http.Free;
  end;
end;

function HttpPost(const server, port: AnsiString; const url, Data, DataType: RawByteString): boolean;
var Http: THttpClientSocket;
begin
  result := false;
  Http := OpenHttp(server,port);
  if Http<>nil then
  try
    result := Http.Post(url,Data,DataType) in [200,201,204];
  finally
    Http.Free;
  end;
end;


function SendEmail(const Server: AnsiString; const From, CSVDest, Subject, Text: RawByteString;
  const Headers: RawByteString=''; const User: RawByteString=''; const Pass: RawByteString='';
  const Port: AnsiString='25'): boolean;
var TCP: TCrtSocket;
procedure Expect(const Answer: RawByteString);
var Res: RawByteString;
begin
  repeat
    readln(TCP.SockIn^,Res);
  until (Length(Res)<4)or(Res[4]<>'-');
  if not IdemPChar(pointer(Res),pointer(Answer)) then
    raise Exception.Create(string(Res));
end;
procedure Exec(const Command, Answer: RawByteString);
begin
  writeln(TCP.SockOut^,Command);
  Expect(Answer)
end;
var P: PAnsiChar;
    rec, ToList: RawByteString;
begin
  result := false;
  P := pointer(CSVDest);
  if P=nil then exit;
  TCP := Open(Server, Port);
  if TCP<>nil then
  try
    TCP.CreateSockIn; // we use SockIn and SockOut here
    TCP.CreateSockOut;
    Expect('220');
    if (User<>'') and (Pass<>'') then begin
      Exec('EHLO '+Server,'25');
      Exec('AUTH LOGIN','334');
      Exec(Base64Encode(User),'334');
      Exec(Base64Encode(Pass),'235');
    end else
      Exec('HELO '+Server,'25');
    writeln(TCP.SockOut^,'MAIL FROM:<',From,'>'); Expect('250');
    ToList := 'To: ';
    repeat
      rec := trim(GetNextItem(P));
      if rec='' then continue;
      if pos(RawByteString('<'),rec)=0 then
        rec := '<'+rec+'>';
      Exec('RCPT TO:'+rec,'25');
      ToList := ToList+rec+', ';
    until P=nil;
    Exec('DATA','354');
    writeln(TCP.SockOut^,'Subject: ',Subject,#13#10,
      ToList,#13#10'Content-Type: text/plain; charset=ISO-8859-1'#13#10+
      'Content-Transfer-Encoding: 8bit'#13#10,
      Headers,#13#10#13#10,Text);
    Exec('.','25');
    writeln(TCP.SockOut^,'QUIT');
    result := true;
  finally
    TCP.Free;
  end;
end;


var
  WsaDataOnce: TWSADATA;

{ THttpServer }

constructor THttpServer.Create(const aPort: AnsiString
      {$ifdef USETHREADPOOL}; ServerThreadPoolCount: integer=32{$endif});
var aSock: TCrtSocket;
begin
  aSock := TCrtSocket.Bind(aPort); // BIND + LISTEN
  inherited Create(false);
  Sock := aSock;
  InitializeCriticalSection(ProcessCS);
  ServerKeepAliveTimeOut := 3000; // HTTP.1/1 KeepAlive is 3 seconds by default
{$ifdef USETHREADPOOL}
  ThreadPool.Initialize(ServerThreadPoolCount); // create a pool of threads
{$endif}
end;

destructor THttpServer.Destroy;
begin
  Terminate; // THttpServerResp.Execute expects Terminated if we reached here
{$ifdef USETHREADPOOL}
  ThreadPool.Shutdown; // will release all associated threads and I/O completion
{$endif}
  // ThreadID := 0 -> don't call WaitFor in Destroy: Execute.Accept() is blocking
{$ifdef Linux}
  pthread_detach(ThreadID); // manualy do it here
{$endif}
  PInteger(@ThreadID)^ := 0; // trick to access a read-only property
  DeleteCriticalSection(ProcessCS);
  Sock.Free;
  inherited Destroy;         // direct Thread abort, no wait till ended
end;

{.$define MONOTHREAD}
// define this not to create a thread at every connection (not recommended)

procedure THttpServer.Execute;
var ClientSock: TSocket;
    Sin: TVarSin;
{$ifdef MONOTHREAD}
    ClientCrtSock: THttpServerSocket;
{$endif}
begin
  // THttpServerGeneric thread preparation: launch any OnHttpThreadStart event
  inherited Execute;
  // main server process loop 
  if Sock.Sock>0 then
    while not Terminated do begin
      ClientSock := Accept(Sock.Sock,Sin);
      if Terminated or (Sock=nil) then begin
        Shutdown(ClientSock,1);
        CloseSocket(ClientSock);
        break; // don't accept input if server is down
      end;
      OnConnect;
{$ifdef MONOTHREAD}
      ClientCrtSock := THttpServerSocket.Create(self);
      try
        ClientCrtSock.InitRequest(ClientSock);
        if ClientCrtSock.GetRequest then
          Process(ClientCrtSock);
        OnDisconnect;
        Shutdown(ClientSock,1);
        CloseSocket(ClientSock)
      finally
        ClientCrtSock.Free;
      end;
{$else}
{$ifdef USETHREADPOOL}
      if not ThreadPool.Push(Self,ClientSock) then
{$endif}// default implementation creates one thread for each incoming socket
        THttpServerResp.Create(ClientSock, self);
{$endif}
      end;
end;

procedure THttpServer.OnConnect;
begin
  inc(ServerConnectionCount);
end;

procedure THttpServer.OnDisconnect;
begin
  // nothing to do by default
end;

const
  XPOWEREDNAME = 'X-Powered-By';
  XPOWEREDVALUE = 'SynCrtSock http://synopse.info';

procedure THttpServer.Process(ClientSock: THttpServerSocket);
var OutContent, OutContentType, OutCustomHeader: RawByteString;
    P: PAnsiChar;
    Code: cardinal;
    s: RawByteString;
begin
  if ClientSock.Headers=nil then // we didn't get the request = socket read error
    exit; // -> send will probably fail -> nothing to send back
  if Terminated then
    exit;
  // calc answer
  with ClientSock do
    Code := Request(URL,Method,HeaderGetText,Content,ContentType,
        OutContent,OutContentType,OutCustomHeader);
  if Terminated then
    exit;
  // send response (multi-thread OK) at once
  if (Code<200) or (ClientSock.Headers=nil) then
    Code := 404;
  if not(Code in [200,201]) and (OutContent='') then begin
    OutCustomHeader := '';
    OutContentType := 'text/html'; // create message to display
    OutContent := RawByteString(ClassName+' Server Error '+IntToStr(Code)
      +'<hr>'+StringReplace(string(s),#13#10,'<br>',[rfReplaceAll]));
  end;
  // 1. send HTTP status command
  if ClientSock.TCPPrefix<>'' then
    ClientSock.SockSend(ClientSock.TCPPrefix);
  if ClientSock.KeepAliveClient then
    ClientSock.SockSend(['HTTP/1.1 ',Code,' OK']) else
    ClientSock.SockSend(['HTTP/1.0 ',Code,' OK']);
  // 2. send headers
  // 2.1. custom headers from Request() method
  P := pointer(OutCustomHeader);
  while P<>nil do begin
    s := GetNextLine(P);
    if s<>'' then begin // no void line (means headers ending)
      ClientSock.SockSend(s);
      if IdemPChar(pointer(s),'CONTENT-ENCODING:') then
        integer(ClientSock.fCompressHeader) := 0; // custom encoding: don't compress
    end;
  end;
  // 2.2. generic headers
  ClientSock.SockSend([XPOWEREDNAME+': '+XPOWEREDVALUE+#13#10'Server: ',ClassName]);
  ClientSock.CompressDataAndWriteHeaders(OutContentType,OutContent);
  if ClientSock.KeepAliveClient then begin
    if ClientSock.fCompressAcceptEncoding<>'' then
      ClientSock.SockSend(ClientSock.fCompressAcceptEncoding);
    ClientSock.SockSend('Connection: Keep-Alive'#13#10); // #13#10 -> end headers
  end else
    ClientSock.SockSend; // headers must end with a void line
  ClientSock.SockSendFlush; // flush all pending data (i.e. headers) to network
  // 3. sent HTTP body content (if any)
  if OutContent<>'' then // no CRLF at the end of data
    ClientSock.SndLow(pointer(OutContent),length(OutContent)); // direct send to socket
end;


{ THttpServerResp }

constructor THttpServerResp.Create(aSock: TSocket; aServer: THttpServer);
begin
  inherited Create(false);
  FreeOnTerminate := true;
  fServer := aServer;
  fClientSock := aSock;
  fServerSock := THttpServerSocket.Create(aServer);
end;

constructor THttpServerResp.Create(aServerSock: THttpServerSocket; aServer: THttpServer);
begin
  inherited Create(false);
  FreeOnTerminate := true;
  fServer := aServer;
  fServerSock := aServerSock;
end;

destructor THttpServerResp.Destroy;
begin
  fServerSock.Free;
  if fClientSock<>0 then begin
    // if Destroy happens before fServerSock.GetRequest() in Execute below
    Shutdown(fClientSock,1);
    CloseSocket(fClientSock);
  end;
  inherited Destroy;
end;

procedure THttpServerResp.Execute;
procedure HandleRequestsProcess;
var c: char;
    StartTick, StopTick, Tick: cardinal;
    Size: integer;
const LOOPWAIT = 64; // ms sleep beetwen connections
begin
  try
    repeat
      StartTick := GetTickCount;
      StopTick := StartTick+fServer.ServerKeepAliveTimeOut;
      repeat
        if (fServer=nil) or fServer.Terminated then
          exit; // server is down -> close connection
        if (fServerSock.SockCanRead(LOOPWAIT)>0) and
           (IOCtlSocket(fServerSock.Sock, FIONREAD, Size)=0) and (Size>0) then begin
          Size := Recv(fServerSock.Sock,@c,1,MSG_PEEK);
          // Recv() may return Size=0 if no data is pending, but no TCP/IP error
          if Size<0 then
            exit; // socket error -> disconnect the client
          if (fServer=nil) or fServer.Terminated then
            exit; // server is down -> disconnect the client 
          if Size=0 then
            // no data available -> wait for keep alive timeout
            sleep(0) else begin
            // get request and headers
            if not fServerSock.GetRequest(True) then
              // fServerSock connection was down or headers not correct
              exit;
            // calc answer and send response
            fServer.Process(fServerSock);
            // keep connection only if necessary
            if fServerSock.KeepAliveClient then
              break else
              exit;
          end;
        end;
        Tick := GetTickCount;
        if Tick<StartTick then // time wrap after continuous run for 49.7 days
          break; // reset Ticks count + retry
        if Tick>StopTick then
          exit; // reached time out -> close connection
       until false;
    until false;
  except
    on E: Exception do
      ; // any exception will silently disconnect the client 
  end;
end;
var aSock: TSocket;
begin
  if fClientSock<>0 then begin
    // direct call from incoming socket
    aSock := fClientSock;
    fClientSock := 0; // mark no need to Shutdown and close fClientSock
    fServerSock.InitRequest(aSock); // now fClientSock is in fServerSock
    if fServer<>nil then
      HandleRequestsProcess;
  end else begin
    // call from TSynThreadPoolTHttpServer -> handle first request
    if not fServerSock.fBodyRetrieved then
      fServerSock.GetBody;
    fServer.Process(fServerSock);
    if (fServer<>nil) and fServerSock.KeepAliveClient then
      HandleRequestsProcess; // process further kept alive requests
  end;
  if fServer<>nil then
    fServer.OnDisconnect;
  if (fServer<>nil) and (fServer.Sock<>nil) then
  try
    EnterCriticalSection(fServer.ProcessCS); // fServer thread protection
    inc(fServer.Sock.BytesIn,fServerSock.BytesIn);
    inc(fServer.Sock.BytesOut,fServerSock.BytesOut);
  finally
    LeaveCriticalSection(fServer.ProcessCS);
  end;
end;


{ THttpSocket }

function PCharToHex32(p: PAnsiChar): cardinal;
var v0,v1: byte;
begin
  result := 0;
  if p<>nil then begin
    while p^=' ' do inc(p);
    repeat
      v0 := Hex2Dec(p[0]);
      if v0=255 then break; // not in '0'..'9','a'..'f'
      v1 := Hex2Dec(p[1]);
      inc(p);
      if v1=255 then begin
        result := (result shl 4)+v0; // only one char left
        break;
      end;
      v0 := v0 shl 4;
      result := result shl 8;
      inc(v0,v1);
      inc(p);
      inc(result,v0);
    until false;
  end;
end;

procedure THttpSocket.GetBody;
var Line: RawByteString; // 32 bits chunk length in hexa
    LinePChar: array[0..31] of AnsiChar;
    Len, LContent, Error: integer;
begin
  fBodyRetrieved := true;
{$ifdef DEBUG23}system.writeln('GetBody ContentLength=',ContentLength);{$endif}
  Content := '';
  {$I-}
  // direct read bytes, as indicated by Content-Length or Chunked
  if Chunked then begin // we ignore the Length
    LContent := 0; // current read position in Content
    repeat
      if SockIn<>nil then begin
        readln(SockIn^,LinePChar);      // use of a static PChar is faster
        Error := ioresult;
        if Error<>0 then
          raise ECrtSocket.Create('GetBody1',Error);
        Len := PCharToHex32(LinePChar); // get chunk length in hexa
      end else begin
        SockRecvLn(Line);
        Len := PCharToHex32(pointer(Line)); // get chunk length in hexa
      end;
      if Len=0 then begin // ignore next line (normaly void)
        SockRecvLn;
        break;
      end;
      SetLength(Content,LContent+Len); // reserve memory space for this chunk
      SockInRead(pointer(PAnsiChar(pointer(Content))+LContent),Len) ; // append chunk data
      inc(LContent,Len);
      SockRecvLn; // ignore next #13#10
    until false;
  end else
  if ContentLength>0 then begin
    SetLength(Content,ContentLength); // not chuncked: direct read
    SockInRead(pointer(Content),ContentLength); // works with SockIn=nil or not
  end else
  if ContentLength<0 then begin // ContentLength=-1 if no Content-Length
    // no Content-Length nor Chunked header -> read until eof()
    if SockIn<>nil then 
      while not eof(SockIn^) do begin
        readln(SockIn^,Line);
        if Content='' then
          Content := Line else
          Content := Content+#13#10+Line;
      end;
    ContentLength := length(Content); // update Content-Length
    exit;
  end;
  // optionaly uncompress content
  if cardinal(fContentCompress)<cardinal(length(fCompress)) then
    if fCompress[fContentCompress].Func(Content,false)='' then
      // invalid content
      raise ECrtSocket.CreateFmt('%s uncompress',[fCompress[fContentCompress].Name]);
  ContentLength := length(Content); // update Content-Length
  if SockIn<>nil then begin
    Error := ioresult;
    if Error<>0 then
      raise ECrtSocket.Create('GetBody2',Error);
  end;
  {$I+}
end;

/// decode 'CONTENT-ENCODING: ' parameter from registered compression list
function SetCompressHeader(const Compress: THttpSocketCompressRecDynArray;
  P: PAnsiChar): THttpSocketCompressSet;
var i: integer;
    aName: AnsiString;
    Beg: PAnsiChar;
begin
  integer(result) := 0;
  if P<>nil then
    repeat
      while P^ in [' ',','] do inc(P);
      Beg := P; // 'gzip;q=1.0, deflate' -> aName='gzip' then 'deflate'
      while not (P^ in [';',',',#0]) do inc(P);
      SetString(aName,Beg,P-Beg);
      for i := 0 to high(Compress) do
        if aName=Compress[i].Name then
          include(result,i);
      while not (P^ in [',',#0]) do inc(P);
    until P^=#0;
end;

function RegisterCompressFunc(var Compress: THttpSocketCompressRecDynArray;
  aFunction: THttpSocketCompress; var aAcceptEncoding: RawByteString): AnsiString;
var i, n: integer;
    dummy, aName: RawByteString;
begin
  result := '';
  if @aFunction=nil then
    exit;
  aName := aFunction(dummy,true);
  n := length(Compress);
  if n=32 then
    exit; // fCompressHeader is 0..31 (casted as integer)
  for i := 0 to n-1 do
    with Compress[i] do
      if (@Func=@aFunction) or (Name=aName) then
        exit;
  setLength(Compress,n+1);
  with Compress[n] do begin
    Name := aName;
    {$ifdef FPC}
    Func := aFunction;
    {$else}
    @Func := @aFunction;
    {$endif}
  end;
  if aAcceptEncoding='' then
    aAcceptEncoding := 'Accept-Encoding: '+aName else
    aAcceptEncoding := aAcceptEncoding+','+aName;
  result := aName;
end;

procedure THttpSocket.GetHeader;
var s: RawByteString;
    i, n: integer;
    P: PAnsiChar;
begin
  fBodyRetrieved := false;
  ContentType := '';
  ContentLength := -1;
  fContentCompress := -1;
  ConnectionClose := false;
  Chunked := false;
  n := 0;
  repeat
    SockRecvLn(s);
    if s='' then
      break; // headers end with a void line
    if length(Headers)<=n then
      SetLength(Headers,n+10);
    Headers[n] := s;
    inc(n);
    {$ifdef DEBUG23}system.Writeln(ClassName,'.HeaderIn ',s);{$endif}
    P := pointer(s);
    if IdemPChar(P,'CONTENT-LENGTH:') then
      ContentLength := GetCardinal(pointer(PAnsiChar(pointer(s))+16)) else
    if IdemPChar(P,'CONTENT-TYPE:') then
      ContentType := trim(copy(s,14,128)) else
    if IdemPChar(P,'TRANSFER-ENCODING: CHUNKED') then
      Chunked := true else
    if IdemPChar(P,'CONNECTION: CLOSE') then
      ConnectionClose := true else
    if fCompress<>nil then
      if IdemPChar(P,'ACCEPT-ENCODING:') then
        fCompressHeader := SetCompressHeader(fCompress,P+16) else
      if IdemPChar(P,'CONTENT-ENCODING: ') then begin
        i := 18;
        while s[i+1]=' ' do inc(i);
        delete(s,1,i);
        for i := 0 to high(fCompress) do
          if fCompress[i].Name=s then begin
            fContentCompress := i;
            break;
          end;
      end;
  until false;
  SetLength(Headers,n);
end;

function THttpSocket.HeaderAdd(const aValue: RawByteString): integer;
begin
  result := length(Headers);
  SetLength(Headers,result+1);
  Headers[result] := aValue;
end;

procedure THttpSocket.HeaderSetText(const aText: RawByteString);
var P, PDeb: PAnsiChar;
    n: integer;
begin
  P := pointer(aText);
  n := 0;
  if P<>nil then
    repeat
      PDeb := P;
      while P^>#13 do inc(P);
      if PDeb<>P then begin // add any not void line
        if length(Headers)<=n then
          SetLength(Headers,n+10);
        SetString(Headers[n],PDeb,P-PDeb);
        inc(n);
      end;
      while (P^=#13) or (P^=#10) do inc(P);
    until P^=#0;
  SetLength(Headers,n);
end;

function THttpSocket.HeaderGetText: RawByteString;
var i,L,n: integer;
    V: PtrInt;
    P: PAnsiChar;
begin
  // much faster than for i := 0 to Count-1 do result := result+Headers[i]+#13#10;
  result := '';
  n := length(Headers);
  if n=0 then
    exit;
  L := n*2; // #13#10 size
  dec(n);
  for i := 0 to n do
    if pointer(Headers[i])<>nil then
      inc(L,PInteger(PAnsiChar(pointer(Headers[i]))-4)^); // fast add length(List[i])
  SetLength(result,L);
  P := pointer(result);
  for i := 0 to n do begin
    V := PtrInt(PAnsiChar(Headers[i]));
    if V<>0 then begin
      L := PInteger(V-4)^;  // L := length(List[i])
      move(pointer(V)^,P^,L);
      inc(P,L);
    end;
    PWord(P)^ := 13+10 shl 8;
    inc(P,2);
  end;
end;

function THttpSocket.HeaderValue(aName: RawByteString): RawByteString;
var i: integer;
begin
  if Headers<>nil then begin
    aName := UpperCase(aName)+':';
    for i := 0 to high(Headers) do
      if IdemPChar(pointer(Headers[i]),pointer(aName)) then begin
        result := trim(copy(Headers[i],length(aName)+1,maxInt));
        exit;
      end;
  end;
  result := '';
end;

function THttpSocket.RegisterCompress(aFunction: THttpSocketCompress): boolean;
begin
  result := RegisterCompressFunc(fCompress,aFunction,fCompressAcceptEncoding)<>'';
end;

const
  // below this size (in bytes), no compression will be done (not worth it)
  COMPRESS_MIN_SIZE = 1024;

function CompressDataAndGetHeaders(Accepted: THttpSocketCompressSet;
  var Handled: THttpSocketCompressRecDynArray; const OutContentType: RawByteString;
  var OutContent: RawByteString): RawByteString;
var i: integer;
    OutContentTypeP: PAnsiChar absolute OutContentType;
begin
  if (integer(Accepted)<>0) and (OutContentType<>'') and (Handled<>nil) and
     (length(OutContent)>=COMPRESS_MIN_SIZE) and
     (IdemPChar(OutContentTypeP,'TEXT/') or
      ((IdemPChar(OutContentTypeP,'APPLICATION/') and
                (IdemPChar(OutContentTypeP+12,'JSON') or
                 IdemPChar(OutContentTypeP+12,'XML'))))) then
    for i := 0 to high(Handled) do
    if i in Accepted then begin
      // compression of the OutContent + update header
      result := Handled[i].Func(OutContent,true);
      exit; // first in fCompress[] is prefered
    end;
  result := '';
end;

procedure THttpSocket.CompressDataAndWriteHeaders(const OutContentType: RawByteString;
  var OutContent: RawByteString);
var OutContentEncoding: RawByteString;
begin
  if integer(fCompressHeader)<>0 then begin
    OutContentEncoding := CompressDataAndGetHeaders(fCompressHeader,fCompress,
      OutContentType,OutContent);
    if OutContentEncoding<>'' then
        SockSend(['Content-Encoding: ',OutContentEncoding]);
  end;
  SockSend(['Content-Length: ',length(OutContent)]); // needed even 0
  if (OutContent<>'') and (OutContentType<>'') then
    SockSend(['Content-Type: ',OutContentType]);
end;


{ THttpServerSocket }

procedure THttpServerSocket.InitRequest(aClientSock: TSocket);
var li: TLinger;
begin
{$ifdef WIN32}
  CreateSockIn; // use SockIn by default if not already initialized: 2x faster
{$endif}
  OpenBind('','',false,aClientSock); // open aClientSock for reading
  // Socket should remain open for 5 seconds after a closesocket() call
  li.l_onoff := Ord(true);
  li.l_linger := 5;
  SetSockOpt(aClientSock, SOL_SOCKET, SO_LINGER, @li, SizeOf(li));
end;

function THttpServerSocket.HeaderGetText: RawByteString;
var Name: TVarSin;
    IP: RawByteString;
begin
  result := inherited HeaderGetText;
  if GetSockName(Sock,Name)<>0 then
    exit;
  IP := GetSinIP(Name);
  if IP<>'' then
    result := result+'RemoteIP: '+IP+#13#10;
end;

function THttpServerSocket.GetRequest(withBody: boolean=true): boolean;
var P: PAnsiChar;
    StartTix, EndTix: cardinal;
begin
  try
    StartTix := GetTickCount;
    // 1st line is command: 'GET /path HTTP/1.1' e.g.
    SockRecvLn(Command);
    if TCPPrefix<>'' then
      if TCPPrefix<>Command then begin
        result := false;
        exit
      end else
      SockRecvLn(Command);
    P := pointer(Command);
    Method := GetNextItem(P,' '); // 'GET'
    URL := GetNextItem(P,' ');    // '/path'
    KeepAliveClient := IdemPChar(P,'HTTP/1.1');
    Content := '';
    // get headers and content
    GetHeader;
    if ConnectionClose then
      KeepAliveClient := false;
    if (ContentLength<0) and KeepAliveClient then
      ContentLength := 0; // HTTP/1.1 and no content length -> no eof
    EndTix := GetTickCount;
    result := EndTix<StartTix+5000; // 5 sec for header -> DOS / TCP SYN Flood
    // if time wrap after 49.7 days -> EndTix<StartTix -> always accepted
    if result and withBody then
      GetBody;
  except
    on E: Exception do
      result := false; // mark error
  end;
end;

constructor THttpServerSocket.Create(aServer: THttpServer);
var i: integer;
begin
  inherited Create;
  if aServer<>nil then begin
    for i := 0 to high(aServer.fCompress) do
      RegisterCompress(aServer.fCompress[i].Func);
    TCPPrefix := aServer.TCPPrefix;
  end;
end;


{ ECrtSocket }

constructor ECrtSocket.Create(const Msg: string);
begin
  Create(Msg,WSAGetLastError());
end;

constructor ECrtSocket.Create(const Msg: string; Error: integer);
begin
  inherited CreateFmt('%s %d',[Msg,-Error]);
end;


{$ifdef Win32}
function GetRemoteMacAddress(const IP: AnsiString): RawByteString;
// implements http://msdn.microsoft.com/en-us/library/aa366358
type
  TSendARP = function(DestIp: DWORD; srcIP: DWORD; pMacAddr: pointer; PhyAddrLen: Pointer): DWORD; stdcall;
const
  HexChars: array[0..15] of AnsiChar = '0123456789ABCDEF';
var dwRemoteIP: DWORD;
    PhyAddrLen: Longword;
    pMacAddr : array [0..7] of byte;
    I: integer;
    P: PAnsiChar;
    SendARPLibHandle: THandle;
    SendARP: TSendARP;
begin
  result := '';
  SendARPLibHandle := LoadLibrary('iphlpapi.dll');
  if SendARPLibHandle<>0 then
  try
    SendARP := TSendARP(GetProcAddress(SendARPLibHandle,'SendARP'));
    if @SendARP=nil then
      exit; // we are not under 2K or later
    dwremoteIP := inet_addr(pointer(IP));
    if dwremoteIP<>0 then begin
      PhyAddrLen := 8;
      if SendARP(dwremoteIP, 0, @pMacAddr, @PhyAddrLen)=NO_ERROR then begin
        if PhyAddrLen=6 then begin
          SetLength(result,12);
          P := pointer(result);
          for i := 0 to 5 do begin
            P[0] := HexChars[pMacAddr[i] shr 4];
            P[1] := HexChars[pMacAddr[i] and $F];
            inc(P,2);
          end;
        end;
      end;
    end;
  finally
    FreeLibrary(SendARPLibHandle);
  end;
end;
{$endif}


{ TSynThreadPool }

const
  // Posted to the completion port when shutting down
  SHUTDOWN_FLAG = cardinal(-1);

function TSynThreadPool.Initialize(WorkerFunc: TThreadFunc;
  NumberOfThreads: Integer): Boolean;
var i: integer;
    ThreadID: DWORD;
begin
  result := false;
  if FRequestQueue<>0 then
    exit; // initialize is performed once
  if NumberOfThreads>63 then
    NumberOfThreads := 64; // maximum count for WaitForMultipleObjects()
  // Create IO completion port to queue the HTTP requests
  FRequestQueue := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, NumberOfThreads);
  if FRequestQueue=INVALID_HANDLE_VALUE then begin
    FRequestQueue := 0;
    exit;
  end;
  // Now create the worker threads
  Setlength(fThreads,NumberOfThreads);
  for i := 0 to high(fThreads) do
    FThreads[i] := BeginThread(nil,0,WorkerFunc,Pointer(FRequestQueue),0,ThreadID);
  result := true;
end;

function TSynThreadPool.Shutdown: Boolean;
var i: integer;
begin
  result := false;
  if FRequestQueue<>0 then begin
    // Tell the threads we're shutting down
    for i := 0 to high(fThreads) do
      PostQueuedCompletionStatus(FRequestQueue, 0, 0, POverLapped(SHUTDOWN_FLAG));
    // Wait for threads to finish, with 30 seconds TimeOut
    WaitForMultipleObjects(length(FThreads), Pointer(FThreads), True, 30000);
    // Close the request queue handle
    CloseHandle(FRequestQueue);
    FRequestQueue := 0;
    // Close the thread handles
    for i := 0 to high(FThreads) do
      CloseHandle(FThreads[I]);
  end;
  SetLength(FThreads, 0); // we need to do it manualy (memory leak in TThread) 
end;


{ TSynThreadPoolTHttpServer }

const
  // if body content length is bigger than 16 KB, a dedicated thread is used
  BIGBODYSIZE = 16384;

type
  TSynThreadPoolContextTHttpServer = record
    Server: THttpServer;
    ClientSock: TSocket;
  end;
  PSynThreadPoolContextTHttpServer = ^TSynThreadPoolContextTHttpServer;

function THttpServerWorkerFunction(Param: Pointer): Integer;
var Context: PSynThreadPoolContextTHttpServer;
{$ifdef ISDELPHIXE2}
{ function GetQueuedCompletionStatus(CompletionPort: THandle;
  var lpNumberOfBytesTransferred; var lpCompletionKey: UIntPtr;
  var lpOverlapped: POverlapped; dwMilliseconds: DWORD): BOOL; stdcall; }
    ContextVar: UIntPtr absolute Context;
    CompletionKey: UIntPtr;
{$else}
{ function GetQueuedCompletionStatus(CompletionPort: THandle;
  var lpNumberOfBytesTransferred, lpCompletionKey: DWORD;
  var lpOverlapped: POverlapped; dwMilliseconds: DWORD): BOOL; stdcall; }
    ContextVar: DWORD absolute Context;
  {$ifdef UNICODE}
    CompletionKey: DWORD_PTR;
  {$else}
    CompletionKey: DWORD;
  {$endif}
{$endif}
    Overlapped: POverlapped;
    FRequestQueue: THandle absolute Param;
    ServerSock: THttpServerSocket;
begin
  while GetQueuedCompletionStatus(FRequestQueue, ContextVar, CompletionKey, OverLapped, INFINITE) do
  try
    if PtrUInt(OverLapped)=SHUTDOWN_FLAG then
      break; // exit thread
    if Context<>nil then
    try
      ServerSock := THttpServerSocket.Create(Context^.Server);
      try
        ServerSock.InitRequest(Context^.ClientSock);
        // get Header of incoming request
        if ServerSock.GetRequest(false) then
          // connection and header seem valid -> process request further 
          if ServerSock.KeepAliveClient or
             (ServerSock.ContentLength>BIGBODYSIZE) then begin
            // HTTP/1.1 Keep Alive -> process in background thread
            // or posted data > 16 KB -> get Body in background thread
            THttpServerResp.Create(ServerSock,Context^.Server);
            ServerSock := nil; // THttpServerResp will do ServerSock.Free
          end else begin
            // no Keep Alive = multi-connection -> process in the Thread Pool
            ServerSock.GetBody; // we need to get it now
            Context^.Server.Process(ServerSock);
            Context^.Server.OnDisconnect;
            // no Shutdown here: will be done client-side
          end;
      finally
        ServerSock.Free;
      end;
    finally
      Freemem(Context); // release context memory
    end;
  except
    on Exception do
      ; // we should handle all exceptions in this loop
  end;
  Result := 0;
end;

function TSynThreadPoolTHttpServer.Initialize(NumberOfThreads: Integer=32): Boolean;
begin
  {$ifdef FPC}
  result := inherited Initialize(@THttpServerWorkerFunction,NumberOfThreads);
  {$else}
  result := inherited Initialize(THttpServerWorkerFunction,NumberOfThreads);
  {$endif}
end;

function TSynThreadPoolTHttpServer.Push(aServer: THttpServer; aClientSock: TSocket): Boolean;
var Context: PSynThreadPoolContextTHttpServer;
begin
  result := false;
  if FRequestQueue=0 then
    exit;
  GetMem(Context,SizeOf(Context^));
  Context^.Server := aServer;
  Context^.ClientSock := aClientSock;
  result := PostQueuedCompletionStatus(FRequestQueue,Cardinal(Context),0,nil);
  if not Result then
    FreeMem(Context); // avoid memory leak
end;


{ THttpApiServer }

{$MINENUMSIZE 4}
{$A+}

type
  // HTTP version used
  HTTP_VERSION = packed record
    MajorVersion: word;
    MinorVersion: word;
  end;

  // the req* values identify Request Headers, and resp* Response Headers
  THttpHeader = (
    reqCacheControl,
    reqConnection,
    reqDate,
    reqKeepAlive,
    reqPragma,
    reqTrailer,
    reqTransferEncoding,
    reqUpgrade,
    reqVia,
    reqWarning,
    reqAllow,
    reqContentLength,
    reqContentType,
    reqContentEncoding,
    reqContentLanguage,
    reqContentLocation,
    reqContentMd5,
    reqContentRange,
    reqExpires,
    reqLastModified,
    reqAccept,
    reqAcceptCharset,
    reqAcceptEncoding,
    reqAcceptLanguage,
    reqAuthorization,
    reqCookie,
    reqExpect,
    reqFrom,
    reqHost,
    reqIfMatch,
    reqIfModifiedSince,
    reqIfNoneMatch,
    reqIfRange,
    reqIfUnmodifiedSince,
    reqMaxForwards,
    reqProxyAuthorization,
    reqReferer,
    reqRange,
    reqTe,
    reqTranslate,
    reqUserAgent
{$ifndef CONDITIONALEXPRESSIONS}
   );
const
  respAcceptRanges = THttpHeader(20);
  respLocation = THttpHeader(23);
  respWwwAuthenticate = THttpHeader(29);
type
{$else}  ,
    respAcceptRanges = 20,
    respAge,
    respEtag,
    respLocation,
    respProxyAuthenticate,
    respRetryAfter,
    respServer,
    respSetCookie,
    respVary,
    respWwwAuthenticate);
{$endif}

  THttpVerb = (
    hvUnparsed,
    hvUnknown,
    hvInvalid,
    hvOPTIONS,
    hvGET,
    hvHEAD,
    hvPOST,
    hvPUT,
    hvDELETE,
    hvTRACE,
    hvCONNECT,
    hvTRACK,  // used by Microsoft Cluster Server for a non-logged trace
    hvMOVE,
    hvCOPY,
    hvPROPFIND,
    hvPROPPATCH,
    hvMKCOL,
    hvLOCK,
    hvUNLOCK,
    hvSEARCH,
    hvMaximum );

  THttpChunkType = (
    hctFromMemory,
    hctFromFileHandle,
    hctFromFragmentCache);

  THttpServiceConfigID = (
    hscIPListenList,
    hscSSLCertInfo,
    hscUrlAclInfo,      
    hscMax);
  THttpServiceConfigQueryType = (
    hscQueryExact,
    hscQueryNext,
    hscQueryMax);

  ULONGLONG = Int64;
  HTTP_OPAQUE_ID = ULONGLONG;
  HTTP_URL_CONTEXT = HTTP_OPAQUE_ID;
  HTTP_REQUEST_ID = HTTP_OPAQUE_ID;
  HTTP_CONNECTION_ID = HTTP_OPAQUE_ID;
  HTTP_RAW_CONNECTION_ID = HTTP_OPAQUE_ID;

  // Pointers overlap and point into pFullUrl. nil if not present.
  HTTP_COOKED_URL = record
    FullUrlLength: word;     // in bytes not including the #0
    HostLength: word;        // in bytes not including the #0
    AbsPathLength: word;     // in bytes not including the #0
    QueryStringLength: word; // in bytes not including the #0
    pFullUrl: PWideChar;     // points to "http://hostname:port/abs/.../path?query"
    pHost: PWideChar;        // points to the first char in the hostname
    pAbsPath: PWideChar;     // Points to the 3rd '/' char
    pQueryString: PWideChar; // Points to the 1st '?' char or #0
  end;

  HTTP_TRANSPORT_ADDRESS = record
    pRemoteAddress: PSOCKADDR;
    pLocalAddress: PSOCKADDR;
  end;

  HTTP_UNKNOWN_HEADER = record
    NameLength: word;          // in bytes not including the #0
    RawValueLength: word;      // in bytes not including the n#0
    pName: PAnsiChar;          // The header name (minus the ':' character)
    pRawValue: PAnsiChar;      // The header value
  end;
  PHTTP_UNKNOWN_HEADER = ^HTTP_UNKNOWN_HEADER;

  HTTP_KNOWN_HEADER = record
    RawValueLength: word;     // in bytes not including the #0
    pRawValue: PAnsiChar;
  end;
  PHTTP_KNOWN_HEADER = ^HTTP_KNOWN_HEADER;

  HTTP_RESPONSE_HEADERS = record
    // number of entries in the unknown HTTP headers array
    UnknownHeaderCount: word;
    // array of unknown HTTP headers
    pUnknownHeaders: PHTTP_UNKNOWN_HEADER;
    // Reserved, must be 0
    TrailerCount: word;
    // Reserved, must be nil
    pTrailers: pointer;
    // Known headers
    KnownHeaders: array[low(THttpHeader)..respWwwAuthenticate] of HTTP_KNOWN_HEADER;
  end;

  HTTP_REQUEST_HEADERS = record
    // number of entries in the unknown HTTP headers array
    UnknownHeaderCount: word;
    // array of unknown HTTP headers
    pUnknownHeaders: PHTTP_UNKNOWN_HEADER;
    // Reserved, must be 0
    TrailerCount: word;
    // Reserved, must be nil
    pTrailers: pointer;
    // Known headers
    KnownHeaders: array[low(THttpHeader)..reqUserAgent] of HTTP_KNOWN_HEADER;
  end;

  HTTP_BYTE_RANGE = record
    StartingOffset: ULARGE_INTEGER;
    Length: ULARGE_INTEGER;
  end;

  HTTP_DATA_CHUNK = record
    case DataChunkType: THttpChunkType of
    hctFromMemory: (
    FromMemory: record
      Reserved1: ULONG;
      pBuffer: pointer;
      BufferLength: ULONG;
      Reserved2: ULONG;
      Reserved3: ULONG;
    end; );
    hctFromFileHandle: (
    FromFileHandle: record
      ByteRange: HTTP_BYTE_RANGE;
      FileHandle: THandle;
    end; );
    hctFromFragmentCache: (
    FromFragmentCache: record
      FragmentNameLength: word;      // in bytes not including the #0
      pFragmentName: PWideChar;
    end; );
  end;
  PHTTP_DATA_CHUNK = ^HTTP_DATA_CHUNK;

  HTTP_SSL_CLIENT_CERT_INFO = record
    CertFlags: ULONG;
    CertEncodedSize: ULONG;
    pCertEncoded: PUCHAR;
    Token: THandle;
    CertDeniedByMapper: boolean;
  end;
  PHTTP_SSL_CLIENT_CERT_INFO = ^HTTP_SSL_CLIENT_CERT_INFO;

  HTTP_SSL_INFO = record
    ServerCertKeySize: word;
    ConnectionKeySize: word;
    ServerCertIssuerSize: ULONG;
    ServerCertSubjectSize: ULONG;
    pServerCertIssuer: PAnsiChar;
    pServerCertSubject: PAnsiChar;
    pClientCertInfo: PHTTP_SSL_CLIENT_CERT_INFO;
    SslClientCertNegotiated: ULONG;
  end;
  PHTTP_SSL_INFO = ^HTTP_SSL_INFO;

  HTTP_SERVICE_CONFIG_URLACL_KEY = record
    pUrlPrefix: PWideChar;
  end;
  HTTP_SERVICE_CONFIG_URLACL_PARAM = record
    pStringSecurityDescriptor: PWideChar;
  end;
  HTTP_SERVICE_CONFIG_URLACL_SET = record
    KeyDesc: HTTP_SERVICE_CONFIG_URLACL_KEY;
    ParamDesc: HTTP_SERVICE_CONFIG_URLACL_PARAM;
  end;
  HTTP_SERVICE_CONFIG_URLACL_QUERY = record
    QueryDesc: THttpServiceConfigQueryType;
    KeyDesc: HTTP_SERVICE_CONFIG_URLACL_KEY;
    dwToken: DWORD;
  end;

  /// structure used to handle data associated with a specific request
  HTTP_REQUEST = record
    // either 0 (Only Header), either HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY
    Flags: cardinal;
    // An identifier for the connection on which the request was received
    ConnectionId: HTTP_CONNECTION_ID;
    // A value used to identify the request when calling
    // HttpReceiveRequestEntityBody, HttpSendHttpResponse, and/or
    // HttpSendResponseEntityBody
    RequestId: HTTP_REQUEST_ID;
    // The context associated with the URL prefix
    UrlContext: HTTP_URL_CONTEXT;
    // The HTTP version number
    Version: HTTP_VERSION;
    // An HTTP verb associated with this request
    Verb: THttpVerb;
    // The length of the verb string if the Verb field is hvUnknown
    // (in bytes not including the last #0)
    UnknownVerbLength: word;
    // The length of the raw (uncooked) URL (in bytes not including the last #0)
    RawUrlLength: word;
     // Pointer to the verb string if the Verb field is hvUnknown
    pUnknownVerb: PAnsiChar;
    // Pointer to the raw (uncooked) URL
    pRawUrl: PAnsiChar;
    // The canonicalized Unicode URL
    CookedUrl: HTTP_COOKED_URL;
    // Local and remote transport addresses for the connection
    Address: HTTP_TRANSPORT_ADDRESS;
    // The request headers.
    Headers: HTTP_REQUEST_HEADERS;
    // The total number of bytes received from network for this request
    BytesReceived: ULONGLONG;
    EntityChunkCount: word;
    pEntityChunks: PHTTP_DATA_CHUNK;
    RawConnectionId: HTTP_RAW_CONNECTION_ID;
    // SSL connection information
    pSslInfo: PHTTP_SSL_INFO; 
  end;
  PHTTP_REQUEST = ^HTTP_REQUEST;

  HTTP_RESPONSE = object
  public
    Flags: cardinal;
    // The raw HTTP protocol version number
    Version: HTTP_VERSION;
    // The HTTP status code (e.g., 200)
    StatusCode: word;
    // in bytes not including the '\0'
    ReasonLength: word;
    // The HTTP reason (e.g., "OK"). This MUST not contain non-ASCII characters
    // (i.e., all chars must be in range 0x20-0x7E).
    pReason: PAnsiChar;
    // The response headers
    Headers: HTTP_RESPONSE_HEADERS;
    // number of elements in pEntityChunks[] array
    EntityChunkCount: word;
    // pEntityChunks points to an array of EntityChunkCount HTTP_DATA_CHUNKs
    pEntityChunks: PHTTP_DATA_CHUNK;
    // will set both StatusCode and Reason
    // - OutStatus is a temporary variable
    // - if DataChunkForErrorContent is set, it will be used to add a content
    // body in the response with the textual representation of the error code
    procedure SetStatus(code: integer; var OutStatus: RawByteString;
      DataChunkForErrorContent: PHTTP_DATA_CHUNK=nil; const ErrorMsg: RawByteString='');
    // will set the content of the reponse, and ContentType header
    procedure SetContent(var DataChunk: HTTP_DATA_CHUNK; const Content: RawByteString;
      const ContentType: RawByteString='text/html');
    /// will set all header values from lines
    // - Content-Type/Content-Encoding/Location will be set in KnownHeaders[]
    // - all other headers will be set in temp UnknownHeaders[0..MaxUnknownHeader]
    procedure SetHeaders(P: PAnsiChar; UnknownHeaders: PHTTP_UNKNOWN_HEADER;
      MaxUnknownHeader: integer);
  end;
  PHTTP_RESPONSE = ^HTTP_RESPONSE;


const
  HTTP_VERSION_UNKNOWN: HTTP_VERSION = (MajorVersion: 0; MinorVersion: 0);
  HTTP_VERSION_0_9: HTTP_VERSION = (MajorVersion: 0; MinorVersion: 9);
  HTTP_VERSION_1_0: HTTP_VERSION = (MajorVersion: 1; MinorVersion: 0);
  HTTP_VERSION_1_1: HTTP_VERSION = (MajorVersion: 1; MinorVersion: 1);
  HTTPAPI_VERSION_1: HTTP_VERSION = (MajorVersion: 1; MinorVersion: 0);
  HTTPAPI_VERSION_2: HTTP_VERSION = (MajorVersion: 2; MinorVersion: 0);
  // if set, available entity body is copied along with the request headers
  // into pEntityChunks
  HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY = 1;
  // there is more entity body to be read for this request
  HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS = 1;
  // initialization for applications that use the HTTP Server API
  HTTP_INITIALIZE_SERVER = 1;
  // initialization for applications that use the HTTP configuration functions
  HTTP_INITIALIZE_CONFIG = 2;
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa364496
  HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER = 1;
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa364499
  HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES = 1;

type
  THttpAPI = packed record
    Module: THandle;
    {/ The HttpInitialize function initializes the HTTP Server API driver, starts it,
    if it has not already been started, and allocates data structures for the
    calling application to support response-queue creation and other operations.
    Call this function before calling any other functions in the HTTP Server API. }
    Initialize: function(Version: HTTP_VERSION; Flags: cardinal;
      pReserved: pointer=nil): HRESULT; stdcall;
    {/ The HttpTerminate function cleans up resources used by the HTTP Server API
    to process calls by an application. An application should call HttpTerminate
    once for every time it called HttpInitialize, with matching flag settings. }
    Terminate: function(Flags: cardinal;
      Reserved: integer=0): HRESULT; stdcall;
    {/ The HttpCreateHttpHandle function creates an HTTP request queue for the
    calling application and returns a handle to it. }
    CreateHttpHandle: function(var ReqQueueHandle: THandle;
      Reserved: integer=0): HRESULT; stdcall;
    {/ The HttpAddUrl function registers a given URL so that requests that match
    it are routed to a specified HTTP Server API request queue. An application
    can register multiple URLs to a single request queue using repeated calls to
    HttpAddUrl.
    - a typical url prefix is 'http://+:80/vroot/', 'https://+:80/vroot/' or
      'http://adatum.com:443/secure/database/' - here the '+' is called a
      Strong wildcard, i.e. will match every IP or server name }
    AddUrl: function(ReqQueueHandle: THandle; UrlPrefix: PWideChar;
      Reserved: integer=0): HRESULT; stdcall;
    {/ Unregisters a specified URL, so that requests for it are no longer
      routed to a specified queue. }
    RemoveUrl: function(ReqQueueHandle: THandle; UrlPrefix: PWideChar): HRESULT; stdcall;
    {/ retrieves the next available HTTP request from the specified request queue }
    ReceiveHttpRequest: function(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID;
      Flags: cardinal; var pRequestBuffer: HTTP_REQUEST; RequestBufferLength: ULONG;
      var pBytesReceived: ULONG; pOverlapped: pointer=nil): HRESULT; stdcall;
    {/ sent the response to a specified HTTP request }
    SendHttpResponse: function(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID;
      Flags: integer; var pHttpResponse: HTTP_RESPONSE; pReserved1: pointer;
      var pBytesSent: cardinal; pReserved2: pointer=nil; Reserved3: ULONG=0;
      pOverlapped: pointer=nil; pReserved4: pointer=nil): HRESULT; stdcall;
    {/ receives additional entity body data for a specified HTTP request }
    ReceiveRequestEntityBody: function(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID;
      Flags: ULONG; pBuffer: pointer; BufferLength: cardinal; var pBytesReceived: cardinal;
      pOverlapped: pointer=nil): HRESULT; stdcall;
    {/ set specified data, such as IP addresses or SSL Certificates, from the
      HTTP Server API configuration store}
    SetServiceConfiguration: function(ServiceHandle: THandle;
      ConfigId: THttpServiceConfigID; pConfigInformation: pointer;
      ConfigInformationLength: ULONG; pOverlapped: pointer=nil): HRESULT; stdcall;
    {/ deletes specified data, such as IP addresses or SSL Certificates, from the
      HTTP Server API configuration store}
    DeleteServiceConfiguration: function(ServiceHandle: THandle;
      ConfigId: THttpServiceConfigID; pConfigInformation: pointer;
      ConfigInformationLength: ULONG; pOverlapped: pointer=nil): HRESULT; stdcall;
  end;

var
  Http: THttpAPI;

const
  HttpNames: array[0..9] of PChar = (
    'HttpInitialize','HttpTerminate','HttpCreateHttpHandle',
    'HttpAddUrl', 'HttpRemoveUrl', 'HttpReceiveHttpRequest',
    'HttpSendHttpResponse', 'HttpReceiveRequestEntityBody',
    'HttpSetServiceConfiguration', 'HttpDeleteServiceConfiguration');

function ToUnicode(const Ansi: RawByteString): RawByteString;
var n, i: integer;
begin
  if Ansi='' then
    result := '' else begin
    n := length(Ansi);
    SetLength(result,n*2+1);
    for i := 0 to n do // to n = including last #0
      PWordArray(pointer(result))^[i] := PByteArray(pointer(Ansi))^[i]; // fast ANSI 7 bit conversion
  end;
end;

function RegURL(aRoot, aPort: RawByteString; Https: boolean;
  aDomainName: RawByteString): RawByteString;
const Prefix: array[boolean] of RawByteString = ('http://','https://');
begin
  if aPort='' then
    aPort := '80';
  aRoot := trim(aRoot);
  aDomainName := trim(aDomainName);
  if aDomainName='' then begin
    result := '';
    exit;
  end;
  if aRoot<>'' then begin
    if aRoot[1]<>'/' then
      insert('/',aRoot,1);
    if aRoot[length(aRoot)]<>'/' then
      aRoot := aRoot+'/';
  end else
    aRoot := '/'; // allow for instance 'http://*:2869/'
  aRoot := Prefix[Https]+aDomainName+':'+aPort+aRoot;
  result := ToUnicode(aRoot);
end;

function THttpApiServer.AddUrl(const aRoot, aPort: RawByteString; Https: boolean;
  const aDomainName: RawByteString): integer;
var s: RawByteString;
    n: integer;
begin
  result := -1;
  if (Self=nil) or (fReqQueue=0) or (Http.Module=0) then
    exit;
  s := RegURL(aRoot, aPort, Https, aDomainName);
  if s='' then
    exit; // invalid parameters
  result := Http.AddUrl(fReqQueue,pointer(s));
  if result=NO_ERROR then begin
    n := length(fRegisteredUrl);
    SetLength(fRegisteredUrl,n+1);
    fRegisteredUrl[n] := s;
  end;
end;

procedure HttpApiInitialize;
var i: Integer;
    P: PPointer;
begin
  if Http.Module<>0 then
    exit; // already loaded
  try
    if Http.Module=0 then begin
      Http.Module := LoadLibrary('httpapi.dll');
      if Http.Module<=255 then
        raise Exception.Create('Unable to find httpapi.dll');
      {$ifdef FPC}
      P := @Http.Initialize;
      {$else}
      P := @@Http.Initialize;
      {$endif}
      for i := 0 to high(HttpNames) do begin
        P^ := GetProcAddress(Http.Module,HttpNames[i]);
        if P^=nil then
          raise Exception.CreateFmt('Unable to find %s in httpapi.dll',[HttpNames[i]]);
        inc(P);
      end;
    end;
  except
    on E: Exception do begin
      if Http.Module>255 then begin
        FreeLibrary(Http.Module);
        Http.Module := 0;
      end;
      raise E;
    end;
  end;
end;

const
  /// will allow AddUrl() registration to everyone
  // - 'GA' (GENERIC_ALL) to grant all access
  // - 'S-1-1-0'	defines a group that includes all users
  HTTPADDURLSECDESC: PWideChar = 'D:(A;;GA;;;S-1-1-0)';

type
  EHttpApiServer = class(Exception)
  public
    constructor Create(NameIndex, Error: integer);
  end;

constructor EHttpApiServer.Create(NameIndex, Error: integer);
begin
  inherited CreateFmt('%s failed: %s (%d)',
    [HttpNames[NameIndex],SysErrorMessage(Error),Error])
end;


class function THttpApiServer.AddUrlAuthorize(const aRoot, aPort: RawByteString;
  Https: boolean; const aDomainName: RawByteString; OnlyDelete: boolean): string;
var prefix: RawByteString;
    Error: HRESULT;
    Config: HTTP_SERVICE_CONFIG_URLACL_SET;
begin
  try
    HttpApiInitialize;
    prefix := RegURL(aRoot, aPort, Https, aDomainName);
    if prefix='' then
      result := 'Invalid parameters' else begin
      Error := Http.Initialize(HTTPAPI_VERSION_1,HTTP_INITIALIZE_CONFIG);
      if Error<>NO_ERROR then
        raise EHttpApiServer.Create(0,Error);
      try
        fillchar(Config,sizeof(Config),0);
        Config.KeyDesc.pUrlPrefix := pointer(prefix);
        // first delete any existing information
        Error := Http.DeleteServiceConfiguration(0,hscUrlAclInfo,@Config,Sizeof(Config));
        // then add authorization rule
        if not OnlyDelete then begin
          Config.KeyDesc.pUrlPrefix := pointer(prefix);
          Config.ParamDesc.pStringSecurityDescriptor := HTTPADDURLSECDESC;
          Error := Http.SetServiceConfiguration(0,hscUrlAclInfo,@Config,Sizeof(Config));
        end;
        if (Error<>NO_ERROR) and (Error<>ERROR_ALREADY_EXISTS) then
          raise EHttpApiServer.Create(8,Error);
        result := ''; // success
      finally
        Http.Terminate(HTTP_INITIALIZE_CONFIG);
      end;
    end;
  except
    on E: Exception do
      result := E.Message;
  end;
end;

procedure THttpApiServer.Clone(ChildThreadCount: integer);
var i: integer;
begin
  if (fReqQueue=0) or not Assigned(OnRequest) or (ChildThreadCount<=0) then
    exit; // nothing to clone (need a queue and a process event)
  if ChildThreadCount>256 then
    ChildThreadCount := 256; // not worth adding
  for i := 1 to ChildThreadCount do
    fClones.Add(THttpApiServer.CreateClone(self));
end;

constructor THttpApiServer.Create(CreateSuspended: Boolean);
var Error: HRESULT;
begin
  inherited Create(true);
  HttpApiInitialize; // will raise an exception in case of failure
  Error := Http.Initialize(HTTPAPI_VERSION_1,HTTP_INITIALIZE_SERVER);
  if Error<>NO_ERROR then
    raise EHttpApiServer.Create(0,Error);
  Error := Http.CreateHttpHandle(fReqQueue);
  if Error<>NO_ERROR then
    raise EHttpApiServer.Create(2,Error);
  fClones := TObjectList.Create;
  if not CreateSuspended then
    Suspended := False;
end;

constructor THttpApiServer.CreateClone(From: THttpApiServer);
begin
  inherited Create(false);
  fReqQueue := From.fReqQueue;
  fOnRequest := From.OnRequest;
  fCompress := From.fCompress;
  OnHttpThreadTerminate := From.OnHttpThreadTerminate;
  fCompressAcceptEncoding := From.fCompressAcceptEncoding;
end;

destructor THttpApiServer.Destroy;
var i: Integer;
begin
  if fClones<>nil then begin  // fClones=nil for clone threads
    if fReqQueue<>0 then begin 
      for i := 0 to high(fRegisteredUrl) do
        Http.RemoveUrl(fReqQueue,Pointer(fRegisteredUrl[i]));
      CloseHandle(fReqQueue); // will break all THttpApiServer.Execute
      Http.Terminate(HTTP_INITIALIZE_SERVER);
    end;
    fClones.Free;
  end;
  inherited Destroy;
end;

{$ifndef CONDITIONALEXPRESSIONS}
function Utf8ToAnsi(const UTF8: AnsiString): AnsiString;
begin
  result := UTF8; // no conversion
end;

procedure RaiseLastOSError;
var
  LastError: Integer;
  Error: EWin32Error;
begin
  LastError := GetLastError;
  if LastError <> 0 then
    Error := EWin32Error.CreateFmt('Error system %d: %s', [LastError,
      SysErrorMessage(LastError)]) else
    Error := EWin32Error.Create('Unknown OS Error');
  Error.ErrorCode := LastError;
  raise Error;
end;
{$endif}

const
  KNOWNHEADERS_NAME: array[reqCacheControl..reqUserAgent] of string[19] = (
    'Cache-Control','Connection','Date','Keep-Alive','Pragma','Trailer',
    'Transfer-Encoding','Upgrade','Via','Warning','Allow','Content-Length',
    'Content-Type','Content-Encoding','Content-Language','Content-Location',
    'Content-MD5','Content-Range','Expires','Last-Modified','Accept',
    'Accept-Charset','Accept-Encoding','Accept-Language','Authorization',
    'Cookie','Expect','From','Host','If-Match','If-Modified-Since',
    'If-None-Match','If-Range','If-Unmodified-Since','Max-Forwards',
    'Proxy-Authorization','Referer','Range','TE','Translate','User-Agent');

function RetrieveHeaders(const Head: HTTP_REQUEST_HEADERS;
  const Address: PSOCKADDR): RawByteString;
var i, L: integer;
    H: THttpHeader;
    P: PHTTP_UNKNOWN_HEADER;
    D: PAnsiChar;
    IP: RawByteString;
begin
  assert(low(KNOWNHEADERS_NAME)=low(Head.KnownHeaders));
  assert(high(KNOWNHEADERS_NAME)=high(Head.KnownHeaders));
  // compute headers length
  L := 0;
  for H := low(KNOWNHEADERS_NAME) to high(KNOWNHEADERS_NAME) do
    if Head.KnownHeaders[H].RawValueLength<>0 then
      inc(L,Head.KnownHeaders[H].RawValueLength+ord(KNOWNHEADERS_NAME[H][0])+4);
  P := Head.pUnknownHeaders;
  if P<>nil then
    for i := 1 to Head.UnknownHeaderCount do begin
      inc(L,P^.NameLength+P^.RawValueLength+4); // +4 for each ': '+#13#10
      inc(P);
    end;
  // set headers content
  SetString(result,nil,L);
  D := pointer(result);
  for H := low(Head.KnownHeaders) to high(Head.KnownHeaders) do
    if Head.KnownHeaders[H].RawValueLength<>0 then begin
      move(KNOWNHEADERS_NAME[H][1],D^,ord(KNOWNHEADERS_NAME[H][0]));
      inc(D,ord(KNOWNHEADERS_NAME[H][0]));
      PWord(D)^ := ord(':')+ord(' ')shl 8;
      inc(D,2);
      move(Head.KnownHeaders[H].pRawValue^,D^,Head.KnownHeaders[H].RawValueLength);
      inc(D,Head.KnownHeaders[H].RawValueLength);
      PWord(D)^ := 13+10 shl 8;
      inc(D,2);
    end;
  P := Head.pUnknownHeaders;
  if P<>nil then
    for i := 1 to Head.UnknownHeaderCount do begin
      move(P^.pName,D^,P^.NameLength);
      inc(D,P^.NameLength);
      PWord(D)^ := ord(':')+ord(' ')shl 8;
      inc(D,2);
      move(P^.pRawValue,D^,P^.RawValueLength);
      inc(D,P^.RawValueLength);
      inc(P);
      PWord(D)^ := 13+10 shl 8;
      inc(D,2);
    end;
  assert(D-pointer(result)=L);
  if Address<>nil then begin
    IP := GetSinIP(PVarSin(Address)^);
    if IP<>'' then
      result := result+'RemoteIP: '+IP+#13#10;
  end;
end;

const
  VERB_TEXT: array[hvOPTIONS..hvSEARCH] of RawByteString = (
    'OPTIONS', 'GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE',
    'CONNECT', 'TRACK', 'MOVE', 'COPY', 'PROPFIND', 'PROPPATCH',
    'MKCOL', 'LOCK', 'UNLOCK', 'SEARCH');
   
procedure THttpApiServer.Execute;
var Req: PHTTP_REQUEST;
    ReqID: HTTP_REQUEST_ID;
    ReqBuf, RespBuf: RawByteString;
    i: integer;
    flags, bytesRead, bytesSent: cardinal;
    Err: HRESULT;
    InCompressAccept: THttpSocketCompressSet;
    InContentLength, InContentLengthRead: cardinal;
    InContentEncoding, InAcceptEncoding, Range: RawByteString;
    OutContentEncoding: RawByteString;
    InURL, InMethod, InHeaders, InContent, InContentType: RawByteString;
    OutContent, OutContentType, OutCustomHeader, OutStatus: RawByteString;
    OutContentTypeP: PAnsiChar absolute OutContentType;
    FileHandle: THandle;
    Resp: PHTTP_RESPONSE;
    BufRead, R: PAnsiChar;
    Heads: array of HTTP_UNKNOWN_HEADER;
    RangeStart, RangeLength: Int64;
    DataChunk: HTTP_DATA_CHUNK;

  procedure SendError(StatusCode: cardinal; const ErrorMsg: string);
  begin
    Resp^.SetStatus(StatusCode,OutStatus,@DataChunk,RawByteString(ErrorMsg));
    Http.SendHttpResponse(fReqQueue,Req^.RequestId,0,Resp^,nil,bytesSent);
  end;

begin
  // THttpServerGeneric thread preparation: launch any OnHttpThreadStart event
  inherited Execute;
  // reserve working buffers
  SetLength(Heads,64);
  SetLength(RespBuf,sizeof(Resp^));
  Resp := pointer(RespBuf);
  SetLength(ReqBuf,16384+sizeof(HTTP_REQUEST)); // space for Req^ + 16 KB of headers
  Req := pointer(ReqBuf);
  // main loop
  ReqID := 0;
  repeat
    // read header
    fillchar(Req^,sizeof(HTTP_REQUEST),0);
    Err := Http.ReceiveHttpRequest(fReqQueue,ReqID,0,Req^,length(ReqBuf),bytesRead);
    if Terminated then
      break;
    case Err of
      NO_ERROR:
      try
        // parse header
        InURL := Req^.pRawUrl;
        if Req^.Verb in [low(VERB_TEXT)..high(VERB_TEXT)] then
          InMethod := VERB_TEXT[Req^.Verb] else
          SetString(InMethod,Req^.pUnknownVerb,Req^.UnknownVerbLength);
        with Req^.Headers.KnownHeaders[reqContentType] do
          SetString(InContentType,pRawValue,RawValueLength);
        with Req^.Headers.KnownHeaders[reqAcceptEncoding] do
          SetString(InAcceptEncoding,pRawValue,RawValueLength);
        InCompressAccept := SetCompressHeader(fCompress,pointer(InAcceptEncoding));
        InHeaders := RetrieveHeaders(Req^.Headers,Req^.Address.pRemoteAddress);
        {$ifdef DEBUGAPI}writeln(InMethod,' ',InURL,#13#10,InHeaders);{$endif}
        // retrieve body
        InContent := '';
        if HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS and Req^.Flags<>0 then begin
          with Req^.Headers.KnownHeaders[reqContentLength] do
            InContentLength := GetCardinal(pRawValue,pRawValue+RawValueLength);
          if InContentLength<>0 then begin
            SetLength(InContent,InContentLength);
            BufRead := pointer(InContent);
            InContentLengthRead := 0;
            repeat
              BytesRead := 0;
              if Win32MajorVersion>5 then // speed optimization for Vista+
                flags := HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER else
                flags := 0;
              Err := Http.ReceiveRequestEntityBody(fReqQueue,Req^.RequestId,flags,
                BufRead,InContentLength-InContentLengthRead,BytesRead);
              inc(InContentLengthRead,BytesRead);
              if Err=ERROR_HANDLE_EOF then begin
                if InContentLengthRead<InContentLength then
                  SetLength(InContent,InContentLengthRead);
                Err := NO_ERROR;
                break; // should loop until returns ERROR_HANDLE_EOF
              end;
              if Err<>NO_ERROR then
                break;
              inc(BufRead,BytesRead);
            until InContentLengthRead=InContentLength;
            if Err<>NO_ERROR then begin
              SendError(406,SysErrorMessage(Err));
              continue;
            end;
            with Req^.Headers.KnownHeaders[reqContentEncoding] do
              SetString(InContentEncoding,pRawValue,RawValueLength);
            if InContentEncoding<>'' then
            for i := 0 to high(fCompress) do
              if fCompress[i].Name=InContentEncoding then begin
                fCompress[i].Func(InContent,false); // uncompress
                break;
              end;
          end;
        end;
        try
          // compute response
          fillchar(Resp^,sizeof(Resp^),0);
          Resp^.SetStatus(Request(InURL,InMethod,InHeaders,InContent,InContentType,
            OutContent,OutContentType,OutCustomHeader),OutStatus);
          if Terminated then
            exit;
          // send response
          Resp^.Version := Req^.Version;
          if fCompressAcceptEncoding<>'' then
            OutCustomHeader := OutCustomHeader+#13#10+fCompressAcceptEncoding;
          Resp^.SetHeaders(pointer(OutCustomHeader),pointer(Heads),high(Heads));
          if OutContentType=HTTP_RESP_STATICFILE then begin
            // response is file -> let http.sys serve it (OutContent is UTF-8)
            FileHandle := FileOpen(
              {$ifdef UNICODE}UTF8ToUnicodeString{$else}Utf8ToAnsi{$endif}(OutContent),
              fmOpenRead or fmShareDenyNone);
            if PtrInt(FileHandle)<0 then
              SendError(404,SysErrorMessage(GetLastError)) else
            try
              DataChunk.DataChunkType := hctFromFileHandle;
              DataChunk.FromFileHandle.FileHandle := FileHandle;
              flags := 0;
              DataChunk.FromFileHandle.ByteRange.StartingOffset.QuadPart := 0;
              Int64(DataChunk.FromFileHandle.ByteRange.Length.QuadPart) := -1; // to eof
              with Req^.Headers.KnownHeaders[reqRange] do
                if (RawValueLength>6) and IdemPChar(pRawValue,'BYTES=') and
                   (pRawValue[6] in ['0'..'9']) then begin
                  SetString(Range,pRawValue+6,RawValueLength-6); // need #0 end
                  R := pointer(Range);
                  RangeStart := GetNextItemUInt64(R);
                  if R^='-' then begin
                    inc(R);
                    flags := HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES;
                    DataChunk.FromFileHandle.ByteRange.StartingOffset := ULARGE_INTEGER(RangeStart);
                    if R^ in ['0'..'9'] then begin
                      RangeLength := GetNextItemUInt64(R)-RangeStart+1;
                      if RangeLength>=0 then // "bytes=0-499" -> start=0, len=500
                        DataChunk.FromFileHandle.ByteRange.Length := ULARGE_INTEGER(RangeLength);
                    end; // "bytes=1000-" -> start=1000, len=-1 (to eof)
                  end;
                end;
              Resp^.EntityChunkCount := 1;
              Resp^.pEntityChunks := @DataChunk;
              Http.SendHttpResponse(fReqQueue,Req^.RequestId,flags,Resp^,nil,bytesSent);
            finally
              FileClose(FileHandle);
            end;
          end else begin
            // response is in OutContent -> sent it
            if integer(fCompress)<>0 then
            with Resp^.Headers.KnownHeaders[reqContentEncoding] do
            if RawValueLength=0 then begin
              // no previous encoding -> try if any compression
              OutContentEncoding := CompressDataAndGetHeaders(InCompressAccept,
                fCompress,OutContentType,OutContent);
              pRawValue := pointer(OutContentEncoding);
              RawValueLength := length(OutContentEncoding);
            end;
            Resp^.SetContent(DataChunk,OutContent,OutContentType);
            Http.SendHttpResponse(fReqQueue,Req^.RequestId,0,Resp^,nil,bytesSent);
          end;
        except
          on E: Exception do
            // handle any exception raised during process: show must go on!
            SendError(500,E.Message); 
        end;
      finally    
        ReqId := 0; // reset Request ID to handle the next request
      end;
      ERROR_MORE_DATA: begin
        // The input buffer was too small to hold the request headers
        // Increase the buffer size and call the API again
        ReqID := Req^.RequestId;
        SetLength(ReqBuf,bytesRead);
        Req := pointer(ReqBuf);
      end;
      ERROR_CONNECTION_INVALID:
        if ReqID=0 then
          break else
          // TCP connection was corrupted by the peer -> ignore + next request
          ReqID := 0;
      else break;
    end;
  until Terminated;
end;

procedure THttpApiServer.RegisterCompress(aFunction: THttpSocketCompress);
var i: integer;
begin
  inherited;
  if fClones<>nil then
    for i := 0 to fClones.Count-1 do
      THttpApiServer(fClones.List{$ifdef FPC}^{$endif}[i]).RegisterCompress(aFunction);
end;


{ HTTP_RESPONSE }

procedure HTTP_RESPONSE.SetContent(var DataChunk: HTTP_DATA_CHUNK;
  const Content, ContentType: RawByteString);
begin
  if Content='' then
    exit;
  fillchar(DataChunk,sizeof(DataChunk),0);
  DataChunk.DataChunkType := hctFromMemory;
  DataChunk.FromMemory.pBuffer := pointer(Content);
  DataChunk.FromMemory.BufferLength := length(Content);
  EntityChunkCount := 1;
  pEntityChunks := @DataChunk;
  Headers.KnownHeaders[reqContentType].RawValueLength := length(ContentType);
  Headers.KnownHeaders[reqContentType].pRawValue := pointer(ContentType);
end;

procedure HTTP_RESPONSE.SetHeaders(P: PAnsiChar;
  UnknownHeaders: PHTTP_UNKNOWN_HEADER; MaxUnknownHeader: integer);
const XPN: PAnsiChar = XPOWEREDNAME;
      XPV: PAnsiChar = XPOWEREDVALUE;
var Known: THttpHeader;
begin
  with UnknownHeaders^ do begin
    pName := XPN;
    NameLength := length(XPOWEREDNAME);
    pRawValue := XPV;
    RawValueLength := length(XPOWEREDVALUE);
  end;
  Headers.pUnknownHeaders := UnknownHeaders;
  Headers.UnknownHeaderCount := 1;
  inc(UnknownHeaders);
  if P<>nil then
  repeat
    while P^ in [#13,#10] do inc(P);
    if P^=#0 then
      break;
    if IdemPChar(P,'CONTENT-TYPE:') then
      Known := reqContentType else
    if IdemPChar(P,'CONTENT-ENCODING:') then
      Known := reqContentEncoding else
    if IdemPChar(P,'LOCATION:') then
      Known := respLocation else
      Known := reqCacheControl; // mark not found
    if Known<>reqCacheControl then
    with Headers.KnownHeaders[Known] do begin
      while P^<>':' do inc(P);
      inc(P); // jump ':'
      while P^=' ' do inc(P);
      pRawValue := P;
      while P^>=' ' do inc(P);
      RawValueLength := P-pRawValue;
    end else begin
      UnknownHeaders^.pName := P;
      while (P^>=' ') and (P^<>':') do inc(P);
      if P^=':' then
        with UnknownHeaders^ do begin
          NameLength := P-pName;
          repeat inc(P) until P^<>' ';
          pRawValue := P;
          repeat inc(P) until P^<' ';
          RawValueLength := P-pRawValue;
          if Headers.UnknownHeaderCount<MaxUnknownHeader then begin
            inc(UnknownHeaders);
            inc(Headers.UnknownHeaderCount);
          end;
        end else
        while P^>=' ' do inc(P);
    end;
  until false;
end;

procedure HTTP_RESPONSE.SetStatus(code: integer; var OutStatus: RawByteString;
 DataChunkForErrorContent: PHTTP_DATA_CHUNK; const ErrorMsg: RawByteString);
begin
  StatusCode := code;
  OutStatus := StatusCodeToReason(code);
  ReasonLength := length(OutStatus);
  pReason := pointer(OutStatus);
  if DataChunkForErrorContent<>nil then
    SetContent(DataChunkForErrorContent^,'<h1>'+OutStatus+'</h1>'+ErrorMsg);
end;


{ THttpServerGeneric }

procedure THttpServerGeneric.RegisterCompress(aFunction: THttpSocketCompress);
begin
  RegisterCompressFunc(fCompress,aFunction,fCompressAcceptEncoding);
end;

function THttpServerGeneric.Request(const InURL, InMethod, InHeaders, InContent,
  InContentType: RawByteString; out OutContent, OutContentType,
  OutCustomHeader: RawByteString): cardinal;
begin
  if Assigned(OnRequest) then
    result := OnRequest(InURL,InMethod,InHeaders,InContent,InContentType,
      OutContent,OutContentType,OutCustomHeader) else
    result := 404; // 404 NOT FOUND
end;

procedure THttpServerGeneric.Execute;
begin
  if Assigned(fOnHttpThreadStart) then
    fOnHttpThreadStart(self);
end;

{$ifndef LVCL}
procedure THttpServerGeneric.DoTerminate;
begin
  if Assigned(fOnHttpThreadTerminate) then
    fOnHttpThreadTerminate(self);
  inherited DoTerminate;
end;
{$endif}


{$ifdef USEWININET}

{ TWinHttpAPI }

constructor TWinHttpAPI.Create(const aServer, aPort: AnsiString; aHttps: boolean;
  const aProxyName: AnsiString=''; const aProxyByPass: AnsiString='');
begin
  fPort := GetCardinal(pointer(aPort));
  if fPort=0 then
    if aHttps then
      fPort := INTERNET_DEFAULT_HTTPS_PORT else
      fPort := INTERNET_DEFAULT_HTTP_PORT;
  fServer := aServer;
  fHttps := aHttps;
  fProxyName := aProxyName;
  fProxyByPass := aProxyByPass;
  InternalConnect; // should raise an exception on error
end;

function TWinHttpAPI.RegisterCompress(aFunction: THttpSocketCompress): boolean;
begin
  result := RegisterCompressFunc(fCompress,aFunction,fCompressAcceptEncoding)<>'';
end;

function TWinHttpAPI.Request(const url, method: RawByteString;
  KeepAlive: cardinal; const InHeader, InData, InDataType: RawByteString;
  out OutHeader, OutData: RawByteString): integer;
var aData, aDataEncoding, aAcceptEncoding, aURL: RawByteString;
    Bytes, DataLen, Read: DWORD;
    i: integer;
begin
  if (url='') or (url[1]<>'/') then
    aURL := '/'+url else // need valid url according to the HTTP/1.1 RFC
    aURL := url;
  fKeepAlive := KeepAlive;
  InternalRequest(method,aURL); // should raise an exception on error
  try
    // common headers
    InternalAddHeader(InHeader);
    if InDataType<>'' then
      InternalAddHeader(RawByteString('Content-Type: ')+InDataType);
    // handle custom compression
    aData := InData;
    if integer(fCompressHeader)<>0 then begin
      aDataEncoding := CompressDataAndGetHeaders(fCompressHeader,fCompress,
        InDataType,aData);
      if aDataEncoding<>'' then
        InternalAddHeader(RawByteString('Content-Encoding: ')+aDataEncoding);
    end;
    if fCompressAcceptEncoding<>'' then
      InternalAddHeader(fCompressAcceptEncoding);
    // send request to remote server
    InternalSendRequest(aData);
    // retrieve status and headers (HTTP_QUERY* and WINHTTP_QUERY* do match)
    result := InternalGetInfo32(HTTP_QUERY_STATUS_CODE);
    OutHeader := InternalGetInfo(HTTP_QUERY_RAW_HEADERS_CRLF);
    aDataEncoding := InternalGetInfo(HTTP_QUERY_CONTENT_ENCODING);
    aAcceptEncoding := InternalGetInfo(HTTP_QUERY_ACCEPT_ENCODING);
    // retrieve received content (if any)
    DataLen := InternalGetInfo32(HTTP_QUERY_CONTENT_LENGTH);
    if DataLen<>0 then begin
      SetLength(OutData,DataLen);
      Read := 0;
      repeat
        Bytes := InternalReadData(OutData,Read);
        if Bytes=0 then begin
          SetLength(OutData,Read);
          break;
        end else
          inc(Read,Bytes);
      until Read=DataLen;
    end;
    // handle incoming answer compression
    if OutData<>'' then begin
      if aDataEncoding<>'' then
        for i := 0 to high(fCompress) do
          with fCompress[i] do
          if Name=aDataEncoding then
            if Func(OutData,false)='' then
              raise ECrtSocket.CreateFmt('%s uncompress',[Name]) else
              break; // successfully uncompressed content
      if aAcceptEncoding<>'' then
        fCompressHeader := SetCompressHeader(fCompress,pointer(aAcceptEncoding));
    end;
  finally
    InternalCloseRequest;
  end; 
end;


{ EWinINet }

constructor EWinINet.Create;
var tmp: array[byte] of Char;
    dwError, tmpLen: DWORD;
    msg: string;
    lastError: cardinal;
begin
  lastError := GetLastError;
  tmpLen := 255;
  if InternetGetLastResponseInfo({$ifdef FPC}@{$endif}dwError,tmp,tmpLen) and (tmpLen>0) then
    SetString(msg,tmp,tmpLen) else
    msg := 'Error';
  inherited CreateFmt('%s (%d)',[msg,lastError]);
end;


{ TWinINet }

destructor TWinINet.Destroy;
begin
  if fConnection<>nil then
    InternetCloseHandle(FConnection);
  if fSession<>nil then
    InternetCloseHandle(FSession);
  inherited;
end;

procedure TWinINet.InternalAddHeader(const hdr: RawByteString);
begin
  if (hdr<>'') and not HttpAddRequestHeadersA(fRequest,
     Pointer(hdr), length(hdr), HTTP_ADDREQ_FLAG_COALESCE) then
    EWinINet.Create;
end;

procedure TWinINet.InternalCloseRequest;
begin
  if fRequest<>nil then begin
    InternetCloseHandle(fRequest);
    fRequest := nil;
  end;
end;

procedure TWinINet.InternalConnect;
begin
  if fProxyName='' then
    fSession := InternetOpenA(DEFAULT_AGENT, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0) else
    fSession := InternetOpenA(DEFAULT_AGENT, INTERNET_OPEN_TYPE_PROXY,
      pointer(fProxyName), pointer(fProxyByPass), 0);
  if fSession=nil then
    raise EWinINet.Create;
  fConnection := InternetConnectA(fSession, pointer(fServer), fPort, nil, nil,
    INTERNET_SERVICE_HTTP, 0, 0);
  if fConnection=nil then
    raise EWinINet.Create;
end;

function TWinINet.InternalGetInfo(Info: DWORD): RawByteString;
var dwSize, dwIndex: DWORD;
begin
  result := '';
  dwSize := 0;
  dwIndex := 0;
  if not HttpQueryInfoA(fRequest, Info, nil, dwSize, dwIndex) and
     (GetLastError=ERROR_INSUFFICIENT_BUFFER) then begin
    SetLength(result,dwSize-1);
    if not HttpQueryInfoA(fRequest, Info, pointer(result), dwSize, dwIndex) then
      result := '';
  end;
end;

function TWinINet.InternalGetInfo32(Info: DWORD): DWORD;
var dwSize, dwIndex: DWORD;
begin
  dwSize := sizeof(result);
  dwIndex := 0;
  Info := Info or HTTP_QUERY_FLAG_NUMBER;
  if not HttpQueryInfoA(fRequest, Info, @result, dwSize, dwIndex) then
    result := 0;
end;

function TWinINet.InternalReadData(var Data: RawByteString; Read: integer): cardinal;
begin
  if not InternetReadFile(fRequest, @PByteArray(Data)[Read], length(Data)-Read, result) then
    raise EWinINet.Create;
end;

procedure TWinINet.InternalRequest(const method, aURL: RawByteString);
const ALL_ACCEPT: array[0..1] of PAnsiChar = ('*/*',nil);
var Flags: DWORD;
begin
  Flags := INTERNET_FLAG_HYPERLINK or INTERNET_FLAG_PRAGMA_NOCACHE or
    INTERNET_FLAG_RESYNCHRONIZE; // options for a true RESTful request
  if FKeepAlive<>0 then
    Flags := Flags or INTERNET_FLAG_KEEP_CONNECTION;
  FRequest := HttpOpenRequestA(FConnection, Pointer(method), Pointer(aURL), nil,
    nil, @ALL_ACCEPT, Flags,0);
  if FRequest=nil then
    EWinINet.Create;
end;

procedure TWinINet.InternalSendRequest(const aData: RawByteString);
begin
  if not HttpSendRequestA(fRequest, nil, 0, pointer(aData), length(aData)) then
    EWinINet.Create;
end;


{ TWinHTTP }

const
  winhttpdll = 'winhttp.dll';

  WINHTTP_ACCESS_TYPE_DEFAULT_PROXY = 0;
  WINHTTP_ACCESS_TYPE_NAMED_PROXY = 3;
  WINHTTP_FLAG_REFRESH = $00000100;
  WINHTTP_FLAG_SECURE = $00800000;
  WINHTTP_ADDREQ_FLAG_COALESCE = $40000000;
  WINHTTP_QUERY_FLAG_NUMBER = $20000000;

function WinHttpOpen(pwszUserAgent: PWideChar; dwAccessType: DWORD;
  pwszProxyName, pwszProxyBypass: PWideChar; dwFlags: DWORD): HINTERNET; stdcall; external winhttpdll;
function WinHttpConnect(hSession: HINTERNET; pswzServerName: PWideChar;
  nServerPort: INTERNET_PORT; dwReserved: DWORD): HINTERNET; stdcall; external winhttpdll;
function WinHttpOpenRequest(hConnect: HINTERNET; pwszVerb: PWideChar;
  pwszObjectName: PWideChar; pwszVersion: PWideChar; pwszReferer: PWideChar;
  ppwszAcceptTypes: PLPWSTR; dwFlags: DWORD): HINTERNET; stdcall; external winhttpdll;
function WinHttpCloseHandle(hInternet: HINTERNET): BOOL; stdcall; external winhttpdll;
function WinHttpAddRequestHeaders(hRequest: HINTERNET; pwszHeaders: PWideChar; dwHeadersLength: DWORD;
  dwModifiers: DWORD): BOOL; stdcall; external winhttpdll;
function WinHttpSendRequest(hRequest: HINTERNET; pwszHeaders: PWideChar;
  dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD; dwTotalLength: DWORD;
  dwContext: DWORD): BOOL; stdcall; external winhttpdll;
function WinHttpReceiveResponse(hRequest: HINTERNET;
  lpReserved: Pointer): BOOL; stdcall; external winhttpdll;
function WinHttpQueryHeaders(hRequest: HINTERNET; dwInfoLevel: DWORD; pwszName: PWideChar;
  lpBuffer: Pointer; var lpdwBufferLength, lpdwIndex: DWORD): BOOL; stdcall; external winhttpdll;
function WinHttpReadData(hRequest: HINTERNET; lpBuffer: Pointer;
  dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall; external winhttpdll;


destructor TWinHTTP.Destroy;
begin
  if fConnection<>nil then
    WinHttpCloseHandle(fConnection);
  if fSession<>nil then
    WinHttpCloseHandle(fSession);
  inherited;
end;

procedure TWinHTTP.InternalAddHeader(const hdr: RawByteString);
begin
  if (hdr<>'') and
    not WinHttpAddRequestHeaders(FRequest, Pointer(ToUnicode(hdr)), length(hdr),
      WINHTTP_ADDREQ_FLAG_COALESCE) then
    RaiseLastOSError;
end;

procedure TWinHTTP.InternalCloseRequest;
begin
  if fRequest<>nil then begin
    WinHttpCloseHandle(fRequest);
    FRequest := nil;
  end;
end;

procedure TWinHTTP.InternalConnect;
begin
  if fProxyName='' then
    fSession := WinHttpOpen(DEFAULT_AGENT, WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, nil, nil, 0) else
    fSession := WinHttpOpen(DEFAULT_AGENT, WINHTTP_ACCESS_TYPE_NAMED_PROXY,
      pointer(ToUnicode(fProxyName)), pointer(ToUnicode(fProxyByPass)), 0);
  if fSession=nil then
    RaiseLastOSError;
  fConnection := WinHttpConnect(fSession, pointer(ToUnicode(FServer)), fPort, 0);
  if fConnection=nil then
    RaiseLastOSError;
end;

function TWinHTTP.InternalGetInfo(Info: DWORD): RawByteString;
var dwSize, dwIndex: DWORD;
    tmp: RawByteString;
    i: integer;
begin
  result := '';
  dwSize := 0;
  dwIndex := 0;
  if not WinHttpQueryHeaders(fRequest, Info, nil, nil, dwSize, dwIndex) and
     (GetLastError=ERROR_INSUFFICIENT_BUFFER) then begin
    SetLength(tmp,dwSize);
    if WinHttpQueryHeaders(fRequest, Info, nil, pointer(tmp), dwSize, dwIndex) then begin
      dwSize := dwSize shr 1;
      SetLength(result,dwSize);
      for i := 0 to dwSize-1 do // fast ANSI 7 bit conversion
        PByteArray(result)^[i] := PWordArray(tmp)^[i];
    end;
  end;
end;

function TWinHTTP.InternalGetInfo32(Info: DWORD): DWORD;
var dwSize, dwIndex: DWORD;
begin
  dwSize := sizeof(result);
  dwIndex := 0;
  Info := Info or WINHTTP_QUERY_FLAG_NUMBER;
  if not WinHttpQueryHeaders(fRequest, Info, nil, @result, dwSize, dwIndex) then
    result := 0;
end;

function TWinHTTP.InternalReadData(var Data: RawByteString; Read: integer): cardinal;
begin
  if not WinHttpReadData(fRequest, @PByteArray(Data)[Read], length(Data)-Read, result) then
    RaiseLastOSError;
end;

procedure TWinHTTP.InternalRequest(const method, aURL: RawByteString);
const ALL_ACCEPT: array[0..1] of PWideChar = ('*/*',nil);
var Flags: DWORD;
begin
  Flags := WINHTTP_FLAG_REFRESH; // options for a true RESTful request
  if fHttps then
    Flags := Flags or WINHTTP_FLAG_SECURE;
  fRequest := WinHttpOpenRequest(fConnection, pointer(ToUnicode(method)),
    pointer(ToUnicode(aURL)), nil, nil, @ALL_ACCEPT, Flags);
  if fRequest=nil then
    RaiseLastOSError;
end;

procedure TWinHTTP.InternalSendRequest(const aData: RawByteString);
var L: integer;
begin
  L := length(aData);
  if not WinHttpSendRequest(fRequest, nil, 0, pointer(aData), L, L, 0) or
     not WinHttpReceiveResponse(fRequest,nil) then
    RaiseLastOSError;
end;

{$endif}


initialization
  {$ifdef DEBUGAPI}AllocConsole;{$endif}
  Assert((sizeof(HTTP_REQUEST)=464) and (sizeof(HTTP_SSL_INFO)=28) and
    (sizeof(HTTP_DATA_CHUNK)=32) and (sizeof(HTTP_REQUEST_HEADERS)=344) and
    (sizeof(HTTP_RESPONSE_HEADERS)=256) and (sizeof(HTTP_COOKED_URL)=24) and
    (sizeof(HTTP_RESPONSE)=280) and (ord(reqUserAgent)=40) and
    (ord(respLocation)=23) and (sizeof(THttpHeader)=4));
  if InitSocketInterface then
    WSAStartup(WinsockLevel, WsaDataOnce) else
    fillchar(WsaDataOnce,sizeof(WsaDataOnce),0);

finalization
  if WsaDataOnce.wVersion<>0 then
    WSACleanup;
  DestroySocketInterface;
end.
