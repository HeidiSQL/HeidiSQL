/// HTTP/1.1 RESTFUL JSON mORMot Server implementation, using FASTCGI
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SQLite3FastCgiServer;

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


 fastcgi configuration example for lighttpd under linux:
   - http://localhost/test will ask TSQLite3FastCGIServer from the content
   - we use only one process and one thread, which is OK for our framework
   - the socket is created by lighttpd, and used by the executable
   - the target executable (located at /var/bin/test) must call the
     SQLite3FastCGIMainProc() procedure
   - default implementation under linux use the libfcgi.so library:
     you'll need to exec e.g.  apt-get install libfcgi0ldbl

fastcgi.server = ( "/test/" =>
  ((  "socket" => "/tmp/test-socket",
      "bin-path" => "/var/bin/test",
  	  "min-procs" => 1,
      "max-procs" => 1,
#      "max-load-per-proc" => 1, # never used if max-procs=1
      "kill-signal" => 10  # libfcgi need to be killed with SIGUSR1(10)
  ))
)

  Version 1.5
  - new server class, using FASTCGI to communicate with an external
    dedicated HTTP Server (tested with lighttpd under Linux via CrossKylix
    compile), using the LibFastCgi external library
  - can use either the standard client library (libfcgi.dll or libfcgi.so),
    either an embedded 100% pascal FastCGI client (for Windows only) - not
    fully tested yet on real production project

}

{.$define USELIBFCGI}
// if defined, the standard client library (libfcgi.dll or libfcgi.so) is
// used, instead of the 100% pascal FastCGI client

{$ifndef MSWINDOWS}
  {$define USELIBFCGI}
  // pure pascal FastCGI client only available under Windows yet
{$endif}


interface

uses
{$ifdef Win32}
  Windows,
  SynWinSock,
{$else}
  Types,
  LibC,
{$endif}
{$ifndef USELIBFCGI}
  SynCrtSock, // the pascal version use our Synopse socket
{$endif}
  SysUtils, Classes,
  SynCommons,
  SQLite3Commons;


{{ HTTP/1.1 RESTFUL JSON SQLite3 Server classes, using FASTCGI
 - will raise an exception if the executable was not used as a FastCGI
   process, but a normal CGI process
 - call this procedure in your main block of your program: it is up to
   the HTTP server to implement the request handling }
procedure SQLite3FastCGIMainProc(Server: TSQLRestServer);



{$ifdef USELIBFCGI}

  (*   FastCGI access through the libfcgi library
         only needed libfcgi functions were defined
         see http://www.fastcgi.com for additional documentation   *)

const
  /// Unsupported Version FastCGI error code
  FCGX_UNSUPPORTED_VERSION = -2;
  /// Protocol related FastCGI error code
  FCGX_PROTOCOL_ERROR = -3;
  /// Parameters related FastCGI error code
  FCGX_PARAMS_ERROR = -4;
  /// Calling Sequence FastCGI error code
  FCGX_CALL_SEQ_ERROR = -5;

  /// Useful FCGX_Request flag
  // - Setting FCGI_FAIL_ACCEPT_ON_INTR prevents FCGX_Accept() from
  // restarting upon being interrupted
  FCGI_FAIL_ACCEPT_ON_INTR =	1;

  /// name of the dll we use to import all FastCGI low-level functions from
  // - 'libfcgi.so.0' is Ubuntu/Debian Linux standard library name: check
  // your distribution
{$IFDEF Win32}
  dllname = 'libfcgi.dll'; {$ELSE}
  dllname = 'libfcgi.so.0';
{$ENDIF}


type
  /// defines the state of a FastCGI stream
  // - The state of a stream is private and should only be accessed
  // as an 'opaque' pointer
  PFCGX_Stream = pointer;

 /// An environment (as defined by environ(7))
 // - A nil-terminated array of strings, each string having the form name=value
  FCGX_ParamArrayType = PPAnsiChar;


/// returns TRUE if this process appears to be a CGI process
// - in such case, you should not use the FastCGI implementation
// - TSQLite3FastCGIServer will raise an exception in such case
function FCGX_IsCGI: boolean; cdecl; external dllname;

{{ accept a new request (NOT multi-thread safe)
 - return 0 for successful call, -1 for error
 - Finishes the request accepted by (and frees any storage allocated by) the
   previous call to FCGX_Accept. Creates input, output, and error streams and
   assigns them to *in, *out, and *err respectively.
   Creates a parameters data structure to be accessed via getenv(3) (if
   assigned to environ) or by FCGX_GetParam and assigns it to *envp.
 - DO NOT retain pointers to the envp array or any strings
   contained in it (e.g. to the result of calling FCGX_GetParam),
   since these will be freed by the next call to FCGX_Finish or FCGX_Accept }
function FCGX_Accept(var _in, _out, _err: PFCGX_Stream; var params: FCGX_ParamArrayType): integer;
  cdecl; external dllname;

{{ read a line from the server socket
 - Reads up to n-1 consecutive bytes from the input stream into the character array str.
 - Stops before n-1 bytes have been read if '\n' or EOF is read.
 - The terminating '\n' is copied to str.  After copying the last byte into str,
   stores a '\0' terminator.
 - returns NULL if EOF is the first thing read from the input stream }
function FCGX_GetLine(str: PAnsiChar; n: integer; stream: PFCGX_Stream): PAnsiChar;
  cdecl; external dllname;

{{ read some data from the server socket
 - reads up to n consecutive bytes from the input stream into the character
   array str.  Performs no interpretation of the input bytes.
 - returns number of bytes read.  If result is smaller than n, the end of input
   has been reached }
function FCGX_GetStr(str: PAnsiChar; n: integer; stream: PFCGX_Stream): integer;
  cdecl; external dllname;

{{ write some data to the server socket
  - writes n consecutive bytes from the character array str into the output stream.
  - performs no interpretation of the output bytes.
  - returns the number of bytes written (n) for normal return,
    EOF (-1) if an error occurred }
function FCGX_PutStr(str: PAnsiChar; n: integer; stream: PFCGX_Stream): integer;
  cdecl; external dllname;

{{ obtain value of FCGI parameter in environment
 - returns the value bound to name, NULL if name not present in the environment envp
 - caller must not mutate the result or retain it past the end of this request
 - see @http://hoohoo.ncsa.illinois.edu/cgi/env.html for a Environment Variables list }
function FCGX_GetParam(const name: PAnsiChar; envp: FCGX_ParamArrayType): PAnsiChar;
  cdecl; external dllname;


{$else}

  (*   NATIVE Pascal FastCGI client implementation
        implemented under Windows only by now         *)

type
  /// FastCGI record types, i.e. the general function that the record performs
  TFCGIRecType = (
    rtBeginRequest = 1,
    rtAbortRequest,
    rtEndRequest,
    rtParams,
    rtStdIn,
    rtStdOut,
    rtStdErr,
    rtData,
    rtGetValues,
    rtGetValuesResult,
    rtUnknown);

  /// FastCGI roles, only Responder role is supported in this unit version
  TFCGIRole = (
    rUnknown,
    rResponder,
    rAuthorizer,
    rFilter);

  PFCGIHeader = ^TFCGIHeader;
  /// FastCGI header
  TFCGIHeader = packed record
    /// FastCGI protocol version, ever constant 1
    Version: byte;
    /// FastCGI record type
    RecType: TFCGIRecType;
    /// identifies the FastCGI request to which the record belongs
    // - equals zero for a management request request
    // - non zero for an application record
    // Used also to determine if the session is being multiplexed
    ID: word;
    /// FastCGI record length
    Len: word;
    /// Pad length to complete the alignment boundery that is 8 bytes on FastCGI protocol
    PadLen: byte;
    /// Pad field
    Filler: byte;
  end;

  PFCGIBeginRequest = ^TFCGIBeginRequest;
  /// rtBeginRequest record
  TFCGIBeginRequest = packed record
    /// FastCGI header
    Header: TFCGIHeader;
    /// Pad field
    Filler: byte;
    /// FastCGI role
    Role: TFCGIRole;
    /// Keep connection
    KeepConn: boolean;
    /// Pad field
    Filler2: array[1..5] of byte;
  end;

  /// FastCGI level status (and error) code for END_REQUEST record
  TFCGIProtocolStatus = (
    psRequestComplete,
    psCantMultiplexConnections,
    psOverloaded,
    psUnknownRole);

  /// FastCGI connection modes
  TFCGIListenType =
    (ltUnused, {ltFileSync, ltFileASync,} ltSocketSync=2, {ltSocketASync,}
     ltPipeSync{, ltPipeASync}); { ltSocketSync=2 so no RTTI }

  /// handle Fast CGI
  // - implements the official Fast CGI Specification available at
  // @http://www.fastcgi.com/devkit/doc/fcgi-spec.html
  // - this base type has virtual public methods ReadPacked and SendPacket,
  // implementing the named pipe or socket defined by the single file
  // descriptor sent by the web server which can be overriden by its children
  // for proper socket/pipe handling
  TFastCGIServer = class
  protected
    fServer: TSQLRestServer;
    fRequestHeaders: RawUTF8;
    fRequestMethod: RawUTF8;
    fRequestURL: RawUTF8;
    // FastCGI role for the current request
    fRequestRole: TFCGIRole;
    // current FastCGI request ID
    fRequestID: word;
    fResponseHeaders: RawUTF8;
    fResponseContent: RawUTF8;
    // global 64 KB buffer, used for chunking data to be sent
    fTempResponse: RawUTF8;
    fTempNamedPipe: RawUTF8;
    fListenType: TFCGIListenType;
    fhListen: THandle;
    fConnectionOpened: boolean;
    fSocket: TCrtSocket;
    // send a response back to the server
    // - the content is chuncked in 64 KB buffers, if necessary
    // - call SendPacket() virtual method
    // - if sent in rtStdOut, the fResponseHeaders are sent with the content; in
    // this case, if no 'content-type:' exists in fResponseHeaders, use default
    // 'text/html'
    function SendResponse(Content: RawUTF8; aRecType: TFCGIRecType): boolean;
    // send an end request, with the specified protocol status
    function SendEndRequest(Status: TFCGIProtocolStatus): boolean;
    // reset all fRequest* parameters
    procedure ResetRequest;
    property hListen: THandle read fhListen;
    property listenType: TFCGIListenType read fListenType;
    property ConnectionOpened: boolean read fConnectionOpened;
  public
    /// virtual method used to read a packet from the remote server
    // - must return '' on error
    // - by default, use the single file descriptor sent by the web server,
    // and expect to read data from the corresponding named pipe or
    // TCP/IP socket
    function ReadPacked: RawUTF8; virtual;
    /// virtual method used to send a packed to the remote server
    // - must return FALSE on error
    // - by default, use the single file descriptor sent by the web server,
    // and expect to write data to the corresponding named pipe or
    // TCP/IP socket
    function SendPacket(Buffer: pointer; BufferLen: integer): boolean; virtual;
    /// method trigerred when the Web server wants to abort the request
    // - do nothing by default - only to be implemented for Multiplex connection
    // which are not enabled with this class
    procedure LogOut; virtual;
    /// method trigerred to calculate the response
    // - expect fRequestHeaders, fRequestMethod and fRequestURL properties as input
    // - update fResponseHeaders and fResponseContent properties as output
    procedure ProcessRequest(const Request: RawUTF8);
  public
    /// create the object instance to run with the specified RESTful Server
    constructor Create(aServer: TSQLRestServer);
    /// release the associated memory and handles
    destructor Destroy; override;
    /// the main loop of the FastCGI application
    // - loop until application is terminated
    // - use the associated RESTful Server to calculate the answer
    // - call the virtual methods ReadPacked and SendPacket to handle the response
    // - the FastCGI server must have been successfully connected before calling it
    // - return true if communication was made successfully
    function Run: boolean; virtual;
    /// associated RESTful Server
    property Server: TSQLRestServer read fServer;
  end;

{$endif}


implementation

{$ifdef USELIBFCGI}

procedure SQLite3FastCGIMainProc(Server: TSQLRestServer);
var _in, _out, _err: PFCGX_Stream;
    envp: FCGX_ParamArrayType;
{function ReadString: RawUTF8;
var tmp: array[0..1023] of AnsiChar; // 1KB should be enough for HTTP headers
    L: integer;
begin
  if FCGX_GetLine(tmp,sizeof(tmp),_in)=nil then
    // EOF reached?
    result := '' else begin
    // get line content
    L := StrLen(tmp);
    while (L>0) and (tmp[L-1]<=' ') do dec(L); // in-place trimright
    SetString(result,tmp,L);
  end;
end;
function RemoteIP: RawUTF8;
begin
  result := FCGX_GetParam('REMOTE_ADDR',envp);
end;}
var Method, URL, Headers, Resp, Content: RawUTF8;
    ContentLength: integer;
begin
  if FCGX_IsCGI then
    raise Exception.CreateFmt('%s not called as a FastCGI process',[paramstr(0)]);
  if Server<>nil then
  while FCGX_Accept(_in, _out, _err, envp)>=0 do begin
    // get headers
    URL := RawUTF8(FCGX_GetParam('REQUEST_URI',envp))+'?'+
           RawUTF8(FCGX_GetParam('QUERY_STRING',envp));
    Method := FCGX_GetParam('REQUEST_METHOD',envp); // 'GET'
    ContentLength := GetCardinal(pointer(FCGX_GetParam('CONTENT_LENGTH',envp)));
    // get content
    Content := '';
    if ContentLength>0 then begin
      SetLength(Content,ContentLength);
      if FCGX_GetStr(pointer(ContentLength),ContentLength,_in)<>ContentLength then
        continue; // invalid request
    end;
    // process the request in the internal TSQLRestServer instance
    with Server.URI(URL,Method,Content,Resp,Headers,SUPERVISOR_ACCESS_RIGHTS) do begin
      ContentLength := length(Resp);
      Headers := trim(Headers); // format headers
      if Headers<>'' then
        Headers := Headers+#13#10;
      Headers := FormatUTF8(
        'Status: %'#13#10'Server-InternalState: %'#13#10+
        'X-Powered-By: TSQLite3FastCGIServer http://synopse.info'#13#10+
        'Content-Type: '+JSON_CONTENT_TYPE+#13#10+
        'Content-Length: %'#13#10+
        '%'#13#10, // headers end with a void line
        [Lo,Hi,ContentLength,Headers]);
    end;
    // send the answer back to the HTTP server
    if FCGX_PutStr(pointer(Headers),length(Headers),_out)=length(Headers) then
      if Resp<>'' then
        FCGX_PutStr(pointer(Resp),ContentLength,_out);
  end;
end;


{$else}

/// retrieve one Name:Value pair length, as encoded by the FastCGI server
// - must be called twice, first for the Name length, then for the Value length
function ReadRequestLen(PB: PByteArray; out PNew: PByteArray): integer;
begin
  if PB[0]>127 then begin
    result := (PB[0] and $7f)shl 24 + PB[1] shl 16 + PB[2] shl 8 + PB[3];
    PNew := PByteArray(PtrInt(PB)+4);
  end else begin
    result := PB[0];
    PNew := PByteArray(PtrInt(PB)+1);
  end;
end;

/// retrieve headers as encoded by the FastCGI server
// - return the headers as Name:Value pairs by line, ready to be
// searched with FindIniNameValue() function
function ReadRequestHeader(Buffer: pointer; BufferLen: integer;
  const SepChar: AnsiChar=':'): RawUTF8;
var Len0,Len1: integer;
    PB: PByteArray absolute Buffer;
    PC: PAnsiChar absolute Buffer;
    PEnd: PAnsiChar;
    PResult: PAnsiChar;
begin
  result := '';
  if (Buffer=nil) or (BufferLen<=0) then
    exit;
  PEnd := PC+BufferLen;
  repeat
    Len0 := ReadRequestLen(PB,PB);
    Len1 := ReadRequestLen(PB,PB);
    SetLength(result,length(result)+Len0+Len1+3); // 'Name:Value'#13#10
    PResult := pointer(result);
    move(PC^,PResult^,Len0);
    inc(PC,Len0);
    inc(PResult,Len0);
    PResult^ := SepChar;
    inc(PResult);
    move(PC^,PResult^,Len1);
    inc(PC,Len1);
    PWord(PResult+Len1)^ := ord(13)+ord(10)shl 8;
  until PC>=PEnd;
end;

/// returns TRUE if UpperName is inside the provided headers, as encoded by the
// FastCGI server
function HasRequestHeader(Buffer: pointer; BufferLen: integer; UpperName: PUTF8Char): boolean;
var Len: integer;
    PB: PByteArray absolute Buffer;
    PC: PUTF8Char absolute Buffer;
    PEnd: PUTF8Char;
begin
  result := false;
  if (Buffer=nil) or (BufferLen<=0) then
    exit;
  PEnd := PC+BufferLen;
  repeat
    Len := ReadRequestLen(PB,PB)+ReadRequestLen(PB,PB);
    if IdemPChar(PC,UpperName) then begin
      result := true;
      exit;
    end;
    inc(PC,Len);
  until PC>=PEnd;
end;

/// add a Name:Value parameter, encoded as expected by the FastCGI server
procedure AddParam(var Result: RawUTF8; const Name,Value: RawUTF8);
var Len: array[0..1] of cardinal;
    i: integer;
    P: PUTF8Char;
    L: cardinal;
begin
  Len[0] := length(Name);
  Len[1] := length(Value);
  L := length(Result);
  if Len[0]>127 then
    inc(L,4) else inc(L,1);
  if Len[1]>127 then
    inc(L,4) else inc(L,1);
  SetLength(result,L+Len[0]+Len[1]);
  P := pointer(result);
  for i := 0 to 1 do
    if Len[i]>127 then begin
      PCardinal(P)^ := Len[i] shr 24 + 128 +
        ((Len[i] shr 16) and 255)shl 8+
        ((Len[i] shr 8) and 255)shl 16+
        (Len[i] and 255)shl 24;
      inc(P,4);
    end else begin
      P^ := AnsiChar(Len[i]);
      inc(P);
    end;
  move(pointer(Name)^,P^,Len[0]);
  inc(P,Len[0]);
  move(pointer(Value)^,P^,Len[1]);
end;


procedure SQLite3FastCGIMainProc(Server: TSQLRestServer);
var FastCGI: TFastCGIServer;
begin
  FastCGI := TFastCGIServer.Create(Server);
  try
    FastCGI.Run;
  finally
    FastCGI.Free;
  end;
end;


{ TFastCGIServer }

const
  MAX_BUFFER = 65536;

constructor TFastCGIServer.Create(aServer: TSQLRestServer);
{$ifdef MSWINDOWS}
var oldStdIn: THandle;
    pipeMode: cardinal;
{$endif}
begin
  inherited Create;
  fServer := aServer;
  SetLength(fTempResponse,MAX_BUFFER+8); // enough place
  SetLength(fTempNamedPipe,MAX_BUFFER);
{$ifdef MSWINDOWS}
  // guess the type (named pipe or socket) of the file descriptor
  // sent by the server, in order to use it in ReadPacked and SendPacket
  oldStdIn := GetStdHandle(STD_INPUT_HANDLE);
  if (GetStdHandle(STD_OUTPUT_HANDLE)=INVALID_HANDLE_VALUE) and
     (GetStdHandle(STD_ERROR_HANDLE)=INVALID_HANDLE_VALUE) and
     (oldStdIn<>INVALID_HANDLE_VALUE) and // FastCGI call: only STDIN
     // Move the handle to a "low" number
     DuplicateHandle(GetCurrentProcess, oldStdIn,
       GetCurrentProcess, @hListen, 0, true, DUPLICATE_SAME_ACCESS) and
     SetStdHandle(STD_INPUT_HANDLE, hListen) then begin
    CloseHandle(oldStdIn);
    // Set the pipe handle state so that it operates in wait mode
    pipeMode := PIPE_READMODE_BYTE or PIPE_WAIT;
    if SetNamedPipeHandleState(hListen,pipeMode,nil,nil) then
      flistenType := ltPipeSync else begin
      flistenType := ltSocketSync;
      fSocket := TCrtSocket.Create;
    end;
    exit;
  end;
  // if we reached here, exe was not called as a FastCGI process
  raise Exception.CreateFmt('%s not called as a FastCGI process',[paramstr(0)]);
{$else}
  not implemented yet: please use libfcgi.so version, which seems stable & fast
  under Linux
{$endif}
end;

destructor TFastCGIServer.Destroy;
begin
{$ifdef MSWINDOWS}
  if listenType=ltPipeSync then
    DisconnectNamedPipe(hListen);
{$endif}
  inherited;
end;

procedure TFastCGIServer.LogOut;
begin
  ; // do nothing by default
end;

procedure TFastCGIServer.ProcessRequest(const Request: RawUTF8);
var Head: RawUTF8;
begin
  if Server=nil then
    ResetRequest else
    with Server.URI(fRequestURL, fRequestMethod, Request,
      fResponseContent, Head, @SUPERVISOR_ACCESS_RIGHTS) do
      fResponseHeaders := FormatUTF8(
        'Status: %'#13#10'Server-InternalState: %'#13#10+
        'X-Powered-By: TFastCGIServer http://synopse.info'#13#10+
        'Content-Type: '+JSON_CONTENT_TYPE+#13#10+
        'Content-Length: %'#13#10+
        '%', // a void line will be appened in SendResponse() method
        [Lo,Hi,length(fResponseContent),Head]);
end;

function TFastCGIServer.ReadPacked: RawUTF8;
{$ifdef MSWINDOWS}
var L: integer;
{$endif}
begin
  result := '';
  if (self=nil) or not ConnectionOpened then
    exit;
{$ifdef MSWINDOWS}
  case listenType of
    ltSocketSync:
      result := fSocket.SockReceiveString(0); // 0 = no Time Out
    ltPipeSync:
      repeat
        L := FileRead(hListen,pointer(fTempNamedPipe)^,length(fTempNamedPipe));
        if L<0 then exit;
        AppendBufferToRawUTF8(result,pointer(fTempNamedPipe),L);
      until (L<length(fTempNamedPipe)) and (result<>'');
    else exit;
  end;
{$endif}
end;

procedure TFastCGIServer.ResetRequest;
begin
  fRequestHeaders := '';
  fRequestMethod := '';
  fRequestURL := '';
  fResponseHeaders := '';
  fResponseContent := '';
end;

function TFastCGIServer.Run: boolean;
var Packet, StreamRequest: RawUTF8;
    PC, PEnd: PUTF8Char;
    PHead: PFCGIHeader;
    ValuesResult: RawUTF8;
    Sin: TVarSin;
begin
  result := false;
{$ifdef MSWINDOWS}
  if not ConnectionOpened then
  case listenType of
    ltPipeSync:
    if not ConnectNamedPipe(hListen,nil) then
      // allow client connected after CreateNamedPipe but before ConnectNamedPipe
      if GetLastError<>ERROR_PIPE_CONNECTED then
        exit;
    ltSocketSync: begin
      fSocket.Sock := hListen;
      if fSocket.SockCanRead(1000)=0 then
        exit;
      fSocket.Sock := accept(hListen,Sin);
      if fSocket.Sock<0 then
        exit; // invalid socket
    end;
    else exit;
  end;
{$else}
  exit;
{$endif}
  result := true;
  fConnectionOpened := true;
  fRequestID := 0;
  if Server<>nil then
  repeat
    Packet := ReadPacked;
    if Packet='' then
      break;
    PC := pointer(Packet);
    PEnd := PC+length(Packet);
    repeat
      PHead := pointer(PC);
      if PHead^.Version<>1 then begin
        SendResponse('Version',rtUnknown);
        break;
      end;
      inc(PC,sizeof(TFCGIHeader));
      if (fRequestID<>0) and (PHead^.ID<>0) and (PHead^.ID<>fRequestID) then
        SendEndRequest(psCantMultiplexConnections) else begin
        case PHead^.RecType of
          rtBeginRequest:
            with PFCGIBeginRequest(PHead)^ do
            if Role in [rResponder..rFilter] then begin
              fRequestID := Header.ID;
              fRequestRole := Role;
              ResetRequest;
              StreamRequest := '';
              fRequestHeaders := 'FCGI_ROLE:'+PTypeInfo(TypeInfo(TFCGIRole))^.
                EnumBaseType^.GetEnumNameTrimed(Role)+#13#10;
            end else
              SendEndRequest(psUnknownRole);
          rtAbortRequest: begin
            LogOut; // do nothing by default
            ResetRequest;
          end;
          rtGetValues: begin
            ValuesResult := '';
            if HasRequestHeader(PC,PHead^.Len,'FCGI_MAX_CONNS:') then
              AddParam(ValuesResult,'FCGI_MAX_CONNS','1');
            if HasRequestHeader(PC,PHead^.Len,'FCGI_MAX_REQS:') then
              AddParam(ValuesResult,'FCGI_MAX_REQS','1');
            if HasRequestHeader(PC,PHead^.Len,'FCGI_MPXS_CONNS:') then
              AddParam(ValuesResult,'FCGI_MPXS_CONNS','0');
            SendResponse(ValuesResult,rtGetValuesResult);
          end;
          rtParams, rtStdIn, rtData: // stream records
            if PHead^.Len=0 then begin
              // end of stream is marked by an empty record
              if PHead^.RecType=rtParams then begin
                // read Name-Value pair headers stream
                fRequestHeaders := ReadRequestHeader(
                  pointer(StreamRequest),length(StreamRequest));
                fRequestURL :=
                  FindIniNameValue(pointer(fRequestHeaders),'REQUEST_URI:')+'?'+
                  FindIniNameValue(pointer(fRequestHeaders),'QUERY_STRING:');
                fRequestMethod :=
                  FindIniNameValue(pointer(fRequestHeaders),'REQUEST_METHOD:'); // 'GET'
              end else begin
                // handle byte streams: stdin, data -> process in Server.URL
                ProcessRequest(StreamRequest);
                if (fResponseContent<>'')  or
                   SameTextU(fRequestMethod,'GET') or
                   SameTextU(fRequestMethod,'HEAD') then
                  SendResponse(fResponseContent,rtStdOut);
                SendEndRequest(psRequestComplete);
              end;
              StreamRequest := '';
            end else
              // append stream records
              AppendBufferToRawUTF8(StreamRequest,PC,PHead^.Len);
        else begin
          // UNKNOWN_TYPE record
          SendResponse(RawUTF8(AnsiChar(PHead^.RecType)+#0#0#0#0#0#0#0), rtUnknown);
          break;
        end;
        end; // end case PHead^.RecType
      end;
      inc(PC,PHead^.Len+PHead^.PadLen);
    until PC>=PEnd;
  until false;
end;

function TFastCGIServer.SendEndRequest(Status: TFCGIProtocolStatus): boolean;
var EndBlock: packed record
      Head: TFCGIHeader; // inline SendResponse() logic
      appStatus: integer;
      protocolStatus: TFCGIProtocolStatus;
      reserved: array[1..3] of byte; // padding for 8 bytes block length
    end;
begin
  assert(sizeof(EndBlock)-sizeof(EndBlock.Head)=8);
  if Status<>psRequestComplete then
    // on error, send back the English error message as text/html
    SendResponse(UnCamelCase(PTypeInfo(TypeInfo(TFCGIProtocolStatus))^.
      EnumBaseType^.GetEnumNameTrimed(Status)),rtStdOut);
  fillchar(EndBlock,sizeof(EndBlock),0);
  EndBlock.Head.Version := 1;
  EndBlock.Head.RecType := rtEndRequest;
  EndBlock.Head.ID := fRequestID;
  EndBlock.Head.Len := sizeof(EndBlock)-sizeof(EndBlock.Head);
  EndBlock.protocolStatus := Status;
  result := SendPacket(@EndBlock,sizeof(EndBlock));
end;

function TFastCGIServer.SendPacket(Buffer: pointer; BufferLen: integer): boolean;
{$ifdef MSWINDOWS}
var L: integer;
{$endif}
begin
  result := false;
  if (self=nil) or not ConnectionOpened or (BufferLen=0) then
    exit;
{$ifdef MSWINDOWS}
  case listenType of
    ltSocketSync:
      result := fSocket.TrySndLow(Buffer,BufferLen);
    ltPipeSync: begin
      repeat
        L := FileWrite(hListen,Buffer^,BufferLen);
        if L<0 then exit;
        dec(BufferLen,L);
        inc(PtrInt(Buffer),L);
      until BufferLen<=0;
      result := true;
    end;
    else exit;
  end;
{$endif}
end;

function TFastCGIServer.SendResponse(Content: RawUTF8; aRecType: TFCGIRecType): boolean;
var P: PUTF8Char;
    PLen: integer;
begin
  if (self=nil) or (fTempResponse='') then begin
    result := false;
    exit; // avoid GPF
  end;
  if aRecType=rtStdOut then begin // add headers to beginning of content
    if IdemPChar(pointer(fRequestMethod),'HEAD') then
      Content := ''; // force no content if only Header was asked for
    if (fResponseHeaders<>'') and
       (fResponseHeaders[length(fResponseHeaders)]<>#10) then
      fResponseHeaders := fResponseHeaders+#13#10; 
    if not ExistsIniNameValue(pointer(fResponseHeaders),'CONTENT-TYPE:') then
      fResponseHeaders := fResponseHeaders+'content-type: text/html'#13#10;
    Content := fResponseHeaders+#13#10+Content;
    fResponseHeaders := '';
  end;
  with PFCGIHeader(pointer(fTempResponse))^ do begin // use a temporary buffer
    PInteger(@Version)^ := 1; // will force ID := 0
    RecType := aRecType;
    if not (aRecType in [rtGetValuesResult, rtUnknown]) then
      ID := fRequestID;
    P := pointer(Content);
    PLen := length(Content);
    repeat // send in 64 KB max chunks
      Len := PLen-(P-pointer(Content));
      if Len>MAX_BUFFER-sizeof(TFCGIHeader) then
        Len := MAX_BUFFER-sizeof(TFCGIHeader);
      PadLen := 7 - ((Len + 7) and 7);
      move(P^,PByteArray(pointer(fTempResponse))^[sizeof(TFCGIHeader)],Len);
      result := SendPacket(pointer(fTempResponse),sizeof(TFCGIHeader)+Len+PadLen);
      inc(P,Len);
    until not Result or (P-pointer(Content)>=PLen);
  end; 
end;

{$endif}

end.
