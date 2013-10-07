/// HTTP/1.1 RESTFUL JSON mORMot Server classes
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SQLite3HttpServer;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
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


     HTTP/1.1 RESTFUL JSON Client/Server using SQLite3 database engine
    *******************************************************************

   - use internaly the JSON format for content communication
   - can be called by TSQLite3HttpClient class from direct Delphi clients
     (see SQLite3HttpClient unit)
   - can be called by any JSON-aware AJAX application
   - can optionaly compress the returned data to optimize Internet bandwidth
   - speed is very high: more than 20MB/sec R/W localy on a 1.8GHz Sempron,
     i.e. 400Mb/sec of duplex raw IP data, with about 200 µs only elapsed
     by request (direct call is 50 µs, so bottle neck is the Win32 API),
     i.e. 5000 requests per second, with 113 result rows (i.e. 4803 bytes
     of JSON data each)... try to find a faster JSON HTTP server! ;)

    Initial version: 2009 May, by Arnaud Bouchez

    Version 1.1
      - code rewrite for FPC and Delphi 2009/2010 compilation

    Version 1.3 - January 22, 2010
      - some small fixes and multi-compiler enhancements

    Version 1.4 - February 8, 2010
      - whole Synopse SQLite3 database framework released under the GNU Lesser
        General Public License version 3, instead of generic "Public Domain"
      - HTTP/1.1 RESTFUL JSON Client and Server split into two units
        (SQLite3HttpClient and SQLite3HttpServer)

    Version 1.5 - February 22, 2010
      - by default, the SQlite3 unit is now not included, in order to save
        some space

    Version 1.8
      - includes Unitary Testing class and functions
      - allows content-type changing for GET blob fields

    Version 1.12
      - TSQLite3HttpServer now handle multiple TSQLRestServer instances
        per one server (dispatching requests via the Root URI used)
      - new AddServer method, to register a TSQLRestServer after launch
      - new TSQLRestServer.OnlyJSONRequests property

    Version 1.13
      - can now use fast http.sys kernel-mode server (THttpApiServer) if
        available, and our pure Delphi THttpServer on default
      - now can compress its content using deflate or faster SynLZ algorithm
      - by default, will handle SynLZ compression for TSQLite3HttpClient
      - can make TCP/IP stream not HTTP compliant (for security visions)
      - TSQLite3HttpServer.Request now uses the new THttpServerGeneric.Request
        virtual abstract method prototype to handle THttpApiServer

    Version 1.16
      - added optional aRestAccessRights parameter in TSQLite3HttpServer.AddServer
        to override the default HTTP_DEFAULT_ACCESS_RIGHTS settings
      - added ServerThreadPoolCount parameter to TSQLite3HttpServer.Create()
        constructor, set by default to 32 - will speed up process of slow
        requests (e.g. a POST with some huge data transmitted at slow rate)
      - fixed error in case of URI similar to 'root?session_signature=...'
      - fixed incorect thread count in TSQLite3HttpServer.Create
      - regression tests are now extracted from this unit, in order to allow
        construction of a TSQLite3HTTPServer instance without the need
        of linking the SQLite3 engine to the executable

    Version 1.17
      - made URI check case-insensitive (as for official RFC)
      - TSQLite3HttpServer will now call virtual TSQLRestServer.EndCurrentThread
        method in each of its terminating threads, to release any thread-specific
        resources (for instance, external connections in SQlite3DB)

}

interface

{$define COMPRESSSYNLZ}
{ if defined, will use SynLZ for content compression
  - SynLZ is much faster than deflate/zip, so is preferred
  - can be set global for Client and Server applications
  - with SynLZ, the 440 KB JSON for TTestClientServerAccess._TSQLite3HttpClient
    is compressed into 106 KB with no speed penalty (it's even a bit faster)
    whereas deflate, even with its level set to 1 (fastest), is 25 % slower
  - is defined by default for a Delphi client }

{.$define COMPRESSDEFLATE}
{ if defined, will use deflate/zip for content compression
  - can be set global for Client and Server applications
  - SynLZ is faster but only known by Delphi clients: you can enable deflate
    when the server is connected an AJAX application (not defined by default)
  - if you define both COMPRESSSYNLZ and COMPRESSDEFLATE, the server will use
    SynLZ if available, and deflate if not called from a Delphi client }

{.$define USETCPPREFIX}
{ if defined, a prefix will be added to the TCP/IP stream so that it won't be
  valid HTTP content any more: it could increase the client/server speed with
  some anti-virus software, but the remote access won't work any more with
  Internet Browsers nor AJAX applications
  - will work only with our THttpServer pure Delphi code, of course
  - not defined by default - should be set globally to the project conditionals }

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 WITHLOG

uses
  Windows,
  SysUtils,
  Classes,
{$ifdef COMPRESSDEFLATE}
  SynZip,
{$endif}
{$ifdef COMPRESSSYNLZ}
  SynLZ,
{$endif}
  SynCrtSock,
  SynCommons,
  SQLite3Commons;


const
  /// the default access rights used by the HTTP server if none is specified
  HTTP_DEFAULT_ACCESS_RIGHTS: PSQLAccessRights = @SUPERVISOR_ACCESS_RIGHTS;

type
  /// HTTP/1.1 RESTFUL JSON SQLite3 Server class
  // - this server is multi-threaded and not blocking
  // - will first try to use fastest http.sys kernel-mode server (i.e. create a
  // THttpApiServer instance); it should work OK under XP or WS 2K3 - but
  // you need to have administrator rights under Vista or Seven: if http.sys
  // fails to initialize, it will use a pure Delphi THttpServer instance; a
  // solution is to call the THttpApiServer.AddUrlAuthorize class method during
  // program setup for the desired port, in order to allow it for every user
  // - just create it and it will serve SQL statements as UTF-8 JSON
  // - for a true AJAX server, expanded data is prefered - your code may contain:
  // ! DBServer.NoAJAXJSON := false;
  TSQLite3HttpServer = class
  protected
    fOnlyJSONRequests: boolean;
    fHttpServer: THttpServerGeneric;
    fPort, fDomainName: AnsiString;
    /// internal servers to compute responses
    fDBServers: array of record
      Server: TSQLRestServer;
      RestAccessRights: PSQLAccessRights;
    end;
    // assigned to fHttpServer.OnHttpThreadStart/Terminate e.g. to handle connections
    procedure HttpThreadStart(Sender: TThread);
    procedure HttpThreadTerminate(Sender: TObject);
    /// implement the server response - must be thread-safe
    function Request(const InURL, InMethod, InHeaders, InContent, InContentType: RawByteString;
      out OutContent, OutContentType, OutCustomHeader: RawByteString): cardinal;
    function GetDBServerCount: integer;
    function GetDBServer(Index: Integer): TSQLRestServer;
    procedure SetDBServerAccessRight(Index: integer; Value: PSQLAccessRights);
  public
    /// create a Server Thread, binded and listening on a TCP port to HTTP JSON requests
    // - raise a EHttpServer exception if binding failed
    // - specify one or more TSQLRestServer server class to be used: each
    // class must have an unique Model.Root value, to identify which TSQLRestServer
    // instance must handle a particular request from its URI
    // - port is an AnsiString, as expected by the WinSock API
    // - aDomainName is the URLprefix to be used for HttpAddUrl API call:
    // it could be either a fully qualified case-insensitive domain name
    // an IPv4 or IPv6 literal string, or a wildcard ('+' will bound
    // to all domain names for the specified port, '*' will accept the request
    // when no other listening hostnames match the request for that port) - this
    // parameter is ignored by the TSQLite3HttpApiServer instance
    // - if DontUseHttpApiServer is set, kernel-mode HTTP.SYS server won't be used
    // and standard Delphi code will be called instead (not recommended)
    // - by default, the PSQLAccessRights will be set to nil
    // - the ServerThreadPoolCount parameter will set the number of threads
    // to be initialized to handle incoming connections (default is 32, which
    // may be sufficient for most cases, maximum is 256)
    constructor Create(const aPort: AnsiString;
      const aServers: array of TSQLRestServer; const aDomainName: AnsiString='+';
      DontUseHttpApiServer: Boolean=false; ServerThreadPoolCount: Integer=32); reintroduce; overload;
    /// create a Server Thread, binded and listening on a TCP port to HTTP JSON requests
    // - raise a EHttpServer exception if binding failed
    // - specify one TSQLRestServer server class to be used
    // - port is an AnsiString, as expected by the WinSock API
    // - aDomainName is the URLprefix to be used for HttpAddUrl API call
    constructor Create(const aPort: AnsiString; aServer: TSQLRestServer;
      const aDomainName: AnsiString='+';
      DontUseHttpApiServer: Boolean=false; aRestAccessRights: PSQLAccessRights=nil;
      ServerThreadPoolCount: Integer=32); reintroduce; overload;
    /// release all memory, internal SQLite3 server (if any) and HTTP handlers
    destructor Destroy; override;
    /// try to register another TSQLRestServer instance to the HTTP server
    // - each TSQLRestServer class must have an unique Model.Root value, to
    // identify which instance must handle a particular request from its URI
    // - an optional aRestAccessRights parameter is available to override the
    // default HTTP_DEFAULT_ACCESS_RIGHTS access right setting - but you shall
    // better rely on the authentication feature included in the framework
    // - return true on success, false on error (e.g. duplicated Root value)
    function AddServer(aServer: TSQLRestServer; aRestAccessRights: PSQLAccessRights=nil): boolean;
    /// the associated running HTTP server instance
    // - either THttpApiServer, either THttpServer
    property HttpServer: THttpServerGeneric read fHttpServer;
    /// read-only access to the number of registered internal servers
    property DBServerCount: integer read GetDBServerCount;
    /// read-only access to all internal servers
    property DBServer[Index: integer]: TSQLRestServer read GetDBServer;
    /// write-only access to all internal servers access right
    // - can be used to override the default HTTP_DEFAULT_ACCESS_RIGHTS setting
    property DBServerAccessRight[Index: integer]: PSQLAccessRights write SetDBServerAccessRight;
    /// set this property to TRUE if the server must only respond to
    // request of MIME type APPLICATION/JSON
    // - the default is false, in order to allow direct view of JSON from
    // any browser
    property OnlyJSONRequests: boolean read fOnlyJSONRequests write fOnlyJSONRequests;
  end;


implementation


{ TSQLite3HttpServer }

function TSQLite3HttpServer.AddServer(aServer: TSQLRestServer;
  aRestAccessRights: PSQLAccessRights): boolean;
var i, n: integer;
begin
  result := False;
  if (self=nil) or (aServer=nil) or (aServer.Model=nil) then
    exit;
  for i := 0 to high(fDBServers) do
    if fDBServers[i].Server.Model.Root=aServer.Model.Root then
      exit; // register only once per URI Root address
  if fHttpServer.InheritsFrom(THttpApiServer) then
    // try to register the URL to http.sys
    if THttpApiServer(fHttpServer).
        AddUrl(aServer.Model.Root,fPort,false,fDomainName)<>NO_ERROR then
      exit;
  n := length(fDBServers);
  SetLength(fDBServers,n+1);
  fDBServers[n].Server := aServer;
  if aRestAccessRights=nil then
    aRestAccessRights := HTTP_DEFAULT_ACCESS_RIGHTS;
  fDBServers[n].RestAccessRights := aRestAccessRights;
  result := true;
end;

constructor TSQLite3HttpServer.Create(const aPort: AnsiString;
  const aServers: array of TSQLRestServer; const aDomainName: AnsiString;
  DontUseHttpApiServer: Boolean; ServerThreadPoolCount: Integer);
var i,j: integer;
    ErrMsg: string;
{$ifdef WITHLOG}
    Log: ISynLog;
{$endif}
begin
{$ifdef WITHLOG}
  Log := TSQLLog.Enter(self);
{$endif}
  inherited Create;
  fDomainName := aDomainName;
  fPort := aPort;
  if high(aServers)<0 then
    ErrMsg := 'Invalid TSQLRestServer' else
  for i := 0 to high(aServers) do
    if (aServers[i]=nil) or (aServers[i].Model=nil) then
      ErrMsg := 'Invalid TSQLRestServer';
  if ErrMsg='' then
    for i := 0 to high(aServers) do
    with aServers[i].Model do
    for j := i+1 to high(aServers) do
      if aServers[j].Model.Root=Root then
        ErrMsg:= 'Duplicated Root URI';
  if ErrMsg<>'' then
     raise EModelException.CreateFmt('%s.Create: %s',[ClassName,ErrMsg]);
  SetLength(fDBServers,length(aServers));
  for i := 0 to high(aServers) do
  with fDBServers[i] do begin
    Server := aServers[i];
    RestAccessRights := HTTP_DEFAULT_ACCESS_RIGHTS;
  end;
  {$ifndef USETCPPREFIX}
  if not DontUseHttpApiServer then
  try
    // first try to use fastest http.sys
    fHttpServer := THttpApiServer.Create(false);
    for i := 0 to high(aServers) do begin
      j := THttpApiServer(fHttpServer).AddUrl(
        aServers[i].Model.Root,aPort,false,aDomainName);
      if j<>NO_ERROR then begin
        ErrMsg := 'Impossible to register URL';
        if j=ERROR_ACCESS_DENIED then
          ErrMsg := ErrMsg+' (administrator rights needed)';
        raise ECommunicationException.CreateFmt('%s.Create: %s',[ClassName,ErrMsg]);
        break;
      end;
    end;
  except
    on E: Exception do begin
      {$ifdef WITHLOG}
      Log.Log(sllError,'% for %',[E,fHttpServer],self);
      {$endif}
      FreeAndNil(fHttpServer); // if http.sys initialization failed
    end;
  end;
  {$endif}
  if fHttpServer=nil then begin
    // http.sys failed -> create one instance of our pure Delphi server
    fHttpServer := THttpServer.Create(aPort,ServerThreadPoolCount);
    {$ifdef USETCPPREFIX}
    THttpServer(fHttpServer).TCPPrefix := 'magic';
    {$endif}
  end;
  fHttpServer.OnHttpThreadStart := HttpThreadStart;
  fHttpServer.OnRequest := Request;
  fHttpServer.OnHttpThreadTerminate := HttpThreadTerminate;
{$ifdef COMPRESSSYNLZ}
  fHttpServer.RegisterCompress(CompressSynLZ);
{$endif}
{$ifdef COMPRESSDEFLATE}
  fHttpServer.RegisterCompress(CompressDeflate);
{$endif}
  if fHttpServer.InheritsFrom(THttpApiServer) then
    // allow fast multi-threaded requests
    if ServerThreadPoolCount>1 then
      THttpApiServer(fHttpServer).Clone(ServerThreadPoolCount-1);
{$ifdef WITHLOG}
  Log.Log(sllInfo,'% initialized',[fHttpServer],self);
{$endif}
end;

constructor TSQLite3HttpServer.Create(const aPort: AnsiString;
  aServer: TSQLRestServer; const aDomainName: AnsiString;
  DontUseHttpApiServer: boolean; aRestAccessRights: PSQLAccessRights;
  ServerThreadPoolCount: integer);
begin
  Create(aPort,[aServer],aDomainName,DontUseHttpApiServer,ServerThreadPoolCount);
  if aRestAccessRights<>nil then
    DBServerAccessRight[0] := aRestAccessRights;
end;

destructor TSQLite3HttpServer.Destroy;
begin
  fHttpServer.Free;
  inherited;
end;

function TSQLite3HttpServer.GetDBServer(Index: Integer): TSQLRestServer;
begin
  if (Self<>nil) and (cardinal(Index)<cardinal(length(fDBServers))) then
    result := fDBServers[Index].Server else
    result := nil;
end;

function TSQLite3HttpServer.GetDBServerCount: integer;
begin
  result := length(fDBServers);
end;

procedure TSQLite3HttpServer.SetDBServerAccessRight(Index: integer;
  Value: PSQLAccessRights);
begin
  if Value=nil then
    Value := HTTP_DEFAULT_ACCESS_RIGHTS;
  if (Self<>nil) and (cardinal(Index)<cardinal(length(fDBServers))) then
    fDBServers[Index].RestAccessRights := Value;
end;

function TSQLite3HttpServer.Request(
  const InURL, InMethod, InHeaders, InContent, InContentType: RawByteString;
    out OutContent, OutContentType, OutCustomHeader: RawByteString): cardinal;
var URL, Root, Head: RawUTF8;
    i,j: integer;
    P: PUTF8Char;
begin
  if (InURL='') or (InMethod='') or
     (OnlyJSONRequests and
      not IdemPChar(pointer(InContentType),'APPLICATION/JSON')) then
    // wrong Input parameters or not JSON request: 400 BAD REQUEST
    result := 400 else begin
    if InURL[1]='/' then  // try any left '/' from URL
      i := 2 else
      i := 1;
    URL := copy(InURL,i,maxInt);
    j := PosEx(RawUTF8('/'),URL,1); // extract Root (root/1234)
    if j=0 then
      j := PosEx(RawUTF8('?'),URL,1); // extract Root (root?session_signature=...)
    if j=0 then
      Root := URL else
      Root := copy(URL,1,j-1);
    result := 404; // page not found by default (in case of wrong URL)
    for i := 0 to high(fDBServers) do
    with fDBServers[i] do
      if IdemPropNameU(Server.Model.Root,Root) then
      with Server.URI(URL,InMethod,InContent,RawUTF8(OutContent),Head,RestAccessRights) do begin
        result := Lo;
        P := pointer(Head);
        if IdemPChar(P,'CONTENT-TYPE: ') then begin
          // change mime type if modified in HTTP header (e.g. GET blob fields)
          OutContentType := GetNextLine(P+14,P);
          Head := P;
        end else
          // default content type is JSON
          OutContentType := JSON_CONTENT_TYPE;
        Head := Trim(Trim(Head)+#13#10'Server-InternalState: '+Int32ToUtf8(Hi));
        OutCustomHeader := Head;
        break;
      end;
  end;
end;

procedure TSQLite3HttpServer.HttpThreadTerminate(Sender: TObject);
var i: integer;
begin
  if self<>nil then
    for i := 0 to high(fDBServers) do
      fDBServers[i].Server.EndCurrentThread(self);
end;

procedure TSQLite3HttpServer.HttpThreadStart(Sender: TThread);
var i: integer;
begin
  if self<>nil then
    for i := 0 to high(fDBServers) do
      fDBServers[i].Server.BeginCurrentThread(Sender);
end;

end.
