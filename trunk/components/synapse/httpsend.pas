{==============================================================================|
| Project : Ararat Synapse                                       | 003.012.007 |
|==============================================================================|
| Content: HTTP client                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c) 1999-2012.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(HTTP protocol client)

Used RFC: RFC-1867, RFC-1947, RFC-2388, RFC-2616
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}
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

unit httpsend;

interface

uses
  SysUtils, Classes,
  blcksock, synautil, synaip, synacode, synsock;

const
  cHttpProtocol = '80';

type
  {:These encoding types are used internally by the THTTPSend object to identify
   the transfer data types.}
  TTransferEncoding = (TE_UNKNOWN, TE_IDENTITY, TE_CHUNKED);

  {:abstract(Implementation of HTTP protocol.)}
  THTTPSend = class(TSynaClient)
  protected
    FSock: TTCPBlockSocket;
    FTransferEncoding: TTransferEncoding;
    FAliveHost: string;
    FAlivePort: string;
    FHeaders: TStringList;
    FDocument: TMemoryStream;
    FMimeType: string;
    FProtocol: string;
    FKeepAlive: Boolean;
    FKeepAliveTimeout: integer;
    FStatus100: Boolean;
    FProxyHost: string;
    FProxyPort: string;
    FProxyUser: string;
    FProxyPass: string;
    FResultCode: Integer;
    FResultString: string;
    FUserAgent: string;
    FCookies: TStringList;
    FDownloadSize: integer;
    FUploadSize: integer;
    FRangeStart: integer;
    FRangeEnd: integer;
    FAddPortNumberToHost: Boolean;
    function ReadUnknown: Boolean; virtual;
    function ReadIdentity(Size: Integer): Boolean;
    function ReadChunked: Boolean;
    procedure ParseCookies;
    function PrepareHeaders: AnsiString;
    function InternalDoConnect(needssl: Boolean): Boolean;
    function InternalConnect(needssl: Boolean): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    {:Reset headers, document and Mimetype.}
    procedure Clear;

    {:Decode ResultCode and ResultString from Value.}
    procedure DecodeStatus(const Value: string);

    {:Connects to host defined in URL and accesses resource defined in URL by
     method. If Document is not empty, send it to the server as part of the HTTP
     request. Server response is in Document and headers. Connection may be
     authorised by username and password in URL. If you define proxy properties,
     connection is made by this proxy.
     If all OK, result is @true, else result is @false.

     If you use 'https:' instead of 'http:' in the URL, your request is made
     by SSL/TLS connection (if you do not specify port, then port 443 is used
     instead of standard port 80). If you use SSL/TLS request and you have
     defined HTTP proxy, then HTTP-tunnel mode is automatically used .}
    function HTTPMethod(const Method, URL: string): Boolean;

    {:You can call this method from OnStatus event to break current data
     transfer. (or from another thread.)}
    procedure Abort;
  published
    {:Before HTTP operation you may define any non-standard headers for HTTP
     request, except: 'Expect: 100-continue', 'Content-Length', 'Content-Type',
     'Connection', 'Authorization', 'Proxy-Authorization' and 'Host' headers.
     After HTTP operation, it contains full headers of the returned document.}
    property Headers: TStringList read FHeaders;

    {:Stringlist with name-value stringlist pairs. Each pair is one cookie.
     After the HTTP request is returned, cookies are parsed to this stringlist.
     You can leave these cookies untouched for next HTTP requests. You can also
     save this stringlist for later use.}
    property Cookies: TStringList read FCookies;

    {:Stream with document to send (before request), or with document received
     from HTTP server (after request).}
    property Document: TMemoryStream read FDocument;

    {:If you need to download only part of a requested document, specify here
     the position of subpart begin. If 0, the full document is requested.}
    property RangeStart: integer read FRangeStart Write FRangeStart;

    {:If you need to download only part of a requested document, specify here
     the position of subpart end. If 0, the document from rangeStart to end of
     document is requested.
     (Useful for resuming broken downloads, for example.)}
    property RangeEnd: integer read FRangeEnd Write FRangeEnd;

    {:Mime type of sending data. Default is: 'text/html'.}
    property MimeType: string read FMimeType Write FMimeType;

    {:Define protocol version. Possible values are: '1.1', '1.0' (default)
     and '0.9'.}
    property Protocol: string read FProtocol Write FProtocol;

    {:If @true (default value), keepalives in HTTP protocol 1.1 is enabled.}
    property KeepAlive: Boolean read FKeepAlive Write FKeepAlive;

    {:Define timeout for keepalives in seconds!}
    property KeepAliveTimeout: integer read FKeepAliveTimeout Write FKeepAliveTimeout;

    {:if @true, then the server is requested for 100status capability when
     uploading data. Default is @false (off).}
    property Status100: Boolean read FStatus100 Write FStatus100;

    {:Address of proxy server (IP address or domain name) where you want to
     connect in @link(HTTPMethod) method.}
    property ProxyHost: string read FProxyHost Write FProxyHost;

    {:Port number for proxy connection. Default value is 8080.}
    property ProxyPort: string read FProxyPort Write FProxyPort;

    {:Username for connection to proxy server used in HTTPMethod method.}
    property ProxyUser: string read FProxyUser Write FProxyUser;

    {:Password for connection to proxy server used in HTTPMethod method.}
    property ProxyPass: string read FProxyPass Write FProxyPass;

    {:Here you can specify custom User-Agent identification.
     Default: 'Mozilla/4.0 (compatible; Synapse)'}
    property UserAgent: string read FUserAgent Write FUserAgent;

    {:Operation result code after successful @link(HTTPMethod) method.}
    property ResultCode: Integer read FResultCode;

    {:Operation result string after successful @link(HTTPMethod) method.}
    property ResultString: string read FResultString;

    {:if this value is not 0, then data download is pending. In this case you
     have here the total size of downloaded data. Useful for drawing download
     progressbar from OnStatus event.}
    property DownloadSize: integer read FDownloadSize;

    {:if this value is not 0, then data upload is pending. In this case you have
     here the total size of uploaded data. Useful for drawing upload progressbar
     from OnStatus event.}
    property UploadSize: integer read FUploadSize;

    {:Socket object used for TCP/IP operation.
     Good for setting OnStatus hook, etc.}
    property Sock: TTCPBlockSocket read FSock;

    {:Allows to switch off port number in 'Host:' HTTP header. By default @TRUE.
     Some buggy servers do not like port informations in this header.}
    property AddPortNumberToHost: Boolean read FAddPortNumberToHost write FAddPortNumberToHost;
  end;

{:A very useful function, and example of use can be found in the THTTPSend
 object. It implements the GET method of the HTTP protocol. This function sends
 the GET method for URL document to an HTTP server. Returned document is in the
 "Response" stringlist (without any headers). Returns boolean TRUE if all went
 well.}
function HttpGetText(const URL: string; const Response: TStrings): Boolean;

{:A very useful function, and example of use can be found in the THTTPSend
 object. It implements the GET method of the HTTP protocol. This function sends
 the GET method for URL document to an HTTP server. Returned document is in the
 "Response" stream. Returns boolean TRUE if all went well.}
function HttpGetBinary(const URL: string; const Response: TStream): Boolean;

{:A very useful function, and example of use can be found in the THTTPSend
 object. It implements the POST method of the HTTP protocol. This function sends
 the SEND method for a URL document to an HTTP server. The document to be sent
 is located in the "Data" stream. The returned document is in the "Data" stream.
 Returns boolean TRUE if all went well.}
function HttpPostBinary(const URL: string; const Data: TStream): Boolean;

{:A very useful function, and example of use can be found in the THTTPSend
 object. It implements the POST method of the HTTP protocol. This function is
 good for POSTing form data. It sends the POST method for a URL document to
 an HTTP server. You must prepare the form data in the same manner as you would
 the URL data, and pass this prepared data to "URLdata". The following is
 a sample of how the data would appear: 'name=Lukas&field1=some%20data'.
 The information in the field must be encoded by the EncodeURLElement function.
 The returned document is in the "Data" stream. Returns boolean TRUE if all
 went well.}
function HttpPostURL(const URL, URLData: string; const Data: TStream): Boolean;

{:A very useful function, and example of use can be found in the THTTPSend
 object. It implements the POST method of the HTTP protocol. This function sends
 the POST method for a URL document to an HTTP server. This function simulates
 posting of file by HTML form using the 'multipart/form-data' method. The posted
 file is in the DATA stream. Its name is Filename string. Fieldname is for the
 name of the form field with the file. (simulates HTML INPUT FILE) The returned
 document is in the ResultData Stringlist. Returns boolean TRUE if all
 went well.}
function HttpPostFile(const URL, FieldName, FileName: string;
  const Data: TStream; const ResultData: TStrings): Boolean;

implementation

constructor THTTPSend.Create;
begin
  inherited Create;
  FHeaders := TStringList.Create;
  FCookies := TStringList.Create;
  FDocument := TMemoryStream.Create;
  FSock := TTCPBlockSocket.Create;
  FSock.Owner := self;
  FSock.ConvertLineEnd := True;
  FSock.SizeRecvBuffer := c64k;
  FSock.SizeSendBuffer := c64k;
  FTimeout := 90000;
  FTargetPort := cHttpProtocol;
  FProxyHost := '';
  FProxyPort := '8080';
  FProxyUser := '';
  FProxyPass := '';
  FAliveHost := '';
  FAlivePort := '';
  FProtocol := '1.0';
  FKeepAlive := True;
  FStatus100 := False;
  FUserAgent := 'Mozilla/4.0 (compatible; Synapse)';
  FDownloadSize := 0;
  FUploadSize := 0;
  FAddPortNumberToHost := true;
  FKeepAliveTimeout := 300;
  Clear;
end;

destructor THTTPSend.Destroy;
begin
  FSock.Free;
  FDocument.Free;
  FCookies.Free;
  FHeaders.Free;
  inherited Destroy;
end;

procedure THTTPSend.Clear;
begin
  FRangeStart := 0;
  FRangeEnd := 0;
  FDocument.Clear;
  FHeaders.Clear;
  FMimeType := 'text/html';
end;

procedure THTTPSend.DecodeStatus(const Value: string);
var
  s, su: string;
begin
  s := Trim(SeparateRight(Value, ' '));
  su := Trim(SeparateLeft(s, ' '));
  FResultCode := StrToIntDef(su, 0);
  FResultString := Trim(SeparateRight(s, ' '));
  if FResultString = s then
    FResultString := '';
end;

function THTTPSend.PrepareHeaders: AnsiString;
begin
  if FProtocol = '0.9' then
    Result := FHeaders[0] + CRLF
  else
{$IFNDEF MSWINDOWS}
    Result := {$IFDEF UNICODE}AnsiString{$ENDIF}(AdjustLineBreaks(FHeaders.Text, tlbsCRLF));
{$ELSE}
    Result := FHeaders.Text;
{$ENDIF}
end;

function THTTPSend.InternalDoConnect(needssl: Boolean): Boolean;
begin
  Result := False;
  FSock.CloseSocket;
  FSock.Bind(FIPInterface, cAnyPort);
  if FSock.LastError <> 0 then
    Exit;
  FSock.Connect(FTargetHost, FTargetPort);
  if FSock.LastError <> 0 then
    Exit;
  if needssl then
  begin
    if (FSock.SSL.SNIHost='') then
      FSock.SSL.SNIHost:=FTargetHost;
    FSock.SSLDoConnect;
    FSock.SSL.SNIHost:=''; //don't need it anymore and don't wan't to reuse it in next connection
    if FSock.LastError <> 0 then
      Exit;
  end;
  FAliveHost := FTargetHost;
  FAlivePort := FTargetPort;
  Result := True;
end;

function THTTPSend.InternalConnect(needssl: Boolean): Boolean;
begin
  if FSock.Socket = INVALID_SOCKET then
    Result := InternalDoConnect(needssl)
  else
    if (FAliveHost <> FTargetHost) or (FAlivePort <> FTargetPort)
      or FSock.CanRead(0) then
      Result := InternalDoConnect(needssl)
    else
      Result := True;
end;

function THTTPSend.HTTPMethod(const Method, URL: string): Boolean;
var
  Sending, Receiving: Boolean;
  status100: Boolean;
  status100error: string;
  ToClose: Boolean;
  Size: Integer;
  Prot, User, Pass, Host, Port, Path, Para, URI: string;
  s, su: AnsiString;
  HttpTunnel: Boolean;
  n: integer;
  pp: string;
  UsingProxy: boolean;
  l: TStringList;
  x: integer;
begin
  {initial values}
  Result := False;
  FResultCode := 500;
  FResultString := '';
  FDownloadSize := 0;
  FUploadSize := 0;

  URI := ParseURL(URL, Prot, User, Pass, Host, Port, Path, Para);
  User := DecodeURL(user);
  Pass := DecodeURL(pass);
  if User = '' then
  begin
    User := FUsername;
    Pass := FPassword;
  end;
  if UpperCase(Prot) = 'HTTPS' then
  begin
    HttpTunnel := FProxyHost <> '';
    FSock.HTTPTunnelIP := FProxyHost;
    FSock.HTTPTunnelPort := FProxyPort;
    FSock.HTTPTunnelUser := FProxyUser;
    FSock.HTTPTunnelPass := FProxyPass;
  end
  else
  begin
    HttpTunnel := False;
    FSock.HTTPTunnelIP := '';
    FSock.HTTPTunnelPort := '';
    FSock.HTTPTunnelUser := '';
    FSock.HTTPTunnelPass := '';
  end;
  UsingProxy := (FProxyHost <> '') and not(HttpTunnel);
  Sending := FDocument.Size > 0;
  {Headers for Sending data}
  status100 := FStatus100 and Sending and (FProtocol = '1.1');
  if status100 then
    FHeaders.Insert(0, 'Expect: 100-continue');
  if Sending then
  begin
    FHeaders.Insert(0, 'Content-Length: ' + IntToStr(FDocument.Size));
    if FMimeType <> '' then
      FHeaders.Insert(0, 'Content-Type: ' + FMimeType);
  end;
  { setting User-agent }
  if FUserAgent <> '' then
    FHeaders.Insert(0, 'User-Agent: ' + FUserAgent);
  { setting Ranges }
  if (FRangeStart > 0) or (FRangeEnd > 0) then
  begin
    if FRangeEnd >= FRangeStart then
      FHeaders.Insert(0, 'Range: bytes=' + IntToStr(FRangeStart) + '-' + IntToStr(FRangeEnd))
    else
      FHeaders.Insert(0, 'Range: bytes=' + IntToStr(FRangeStart) + '-');
  end;
  { setting Cookies }
  s := '';
  for n := 0 to FCookies.Count - 1 do
  begin
    if s <> '' then
      s := s + '; ';
    s := s + FCookies[n];
  end;
  if s <> '' then
    FHeaders.Insert(0, 'Cookie: ' + s);
  { setting KeepAlives }
  pp := '';
  if UsingProxy then
    pp := 'Proxy-';
  if FKeepAlive then
  begin
    FHeaders.Insert(0, pp + 'Connection: keep-alive');
    FHeaders.Insert(0, 'Keep-Alive: ' + IntToStr(FKeepAliveTimeout));
  end
  else
    FHeaders.Insert(0, pp + 'Connection: close');
  { set target servers/proxy, authorizations, etc... }
  if User <> '' then
    FHeaders.Insert(0, 'Authorization: Basic ' + EncodeBase64(User + ':' + Pass));
  if UsingProxy and (FProxyUser <> '') then
    FHeaders.Insert(0, 'Proxy-Authorization: Basic ' +
      EncodeBase64(FProxyUser + ':' + FProxyPass));
  if isIP6(Host) then
    s := '[' + Host + ']'
  else
    s := Host;
  if FAddPortNumberToHost and (Port <> '80') then
     FHeaders.Insert(0, 'Host: ' + s + ':' + Port)
  else
     FHeaders.Insert(0, 'Host: ' + s);
  if UsingProxy then
    URI := Prot + '://' + s + ':' + Port + URI;
  if URI = '/*' then
    URI := '*';
  if FProtocol = '0.9' then
    FHeaders.Insert(0, UpperCase(Method) + ' ' + URI)
  else
    FHeaders.Insert(0, UpperCase(Method) + ' ' + URI + ' HTTP/' + FProtocol);
  if UsingProxy then
  begin
    FTargetHost := FProxyHost;
    FTargetPort := FProxyPort;
  end
  else
  begin
    FTargetHost := Host;
    FTargetPort := Port;
  end;
  if FHeaders[FHeaders.Count - 1] <> '' then
    FHeaders.Add('');

  { connect }
  if not InternalConnect(UpperCase(Prot) = 'HTTPS') then
  begin
    FAliveHost := '';
    FAlivePort := '';
    Exit;
  end;

  { reading Status }
  FDocument.Position := 0;
  Status100Error := '';
  if status100 then
  begin
    { send Headers }
    FSock.SendString(PrepareHeaders);
    if FSock.LastError <> 0 then
      Exit;
    repeat
      s := FSock.RecvString(FTimeout);
      if s <> '' then
        Break;
    until FSock.LastError <> 0;
    DecodeStatus(s);
    Status100Error := s;
    repeat
      s := FSock.recvstring(FTimeout);
      if s = '' then
        Break;
    until FSock.LastError <> 0;
    if (FResultCode >= 100) and (FResultCode < 200) then
    begin
      { we can upload content }
      Status100Error := '';
      FUploadSize := FDocument.Size;
      FSock.SendBuffer(FDocument.Memory, FDocument.Size);
    end;
  end
  else
    { upload content }
    if sending then
    begin
      if FDocument.Size >= c64k then
      begin
        FSock.SendString(PrepareHeaders);
        FUploadSize := FDocument.Size;
        FSock.SendBuffer(FDocument.Memory, FDocument.Size);
      end
      else
      begin
        s := PrepareHeaders + ReadStrFromStream(FDocument, FDocument.Size);
        FUploadSize := Length(s);
        FSock.SendString(s);
      end;
    end
    else
    begin
      { we not need to upload document, send headers only }
      FSock.SendString(PrepareHeaders);
    end;

  if FSock.LastError <> 0 then
    Exit;

  Clear;
  Size := -1;
  FTransferEncoding := TE_UNKNOWN;

  { read status }
  if Status100Error = '' then
  begin
    repeat
      repeat
        s := FSock.RecvString(FTimeout);
        if s <> '' then
          Break;
      until FSock.LastError <> 0;
      if Pos('HTTP/', UpperCase(s)) = 1 then
      begin
        FHeaders.Add(s);
        DecodeStatus(s);
      end
      else
      begin
        { old HTTP 0.9 and some buggy servers not send result }
        s := s + CRLF;
        WriteStrToStream(FDocument, s);
        FResultCode := 0;
      end;
    until (FSock.LastError <> 0) or (FResultCode <> 100);
  end
  else
    FHeaders.Add(Status100Error);

  { if need receive headers, receive and parse it }
  ToClose := FProtocol <> '1.1';
  if FHeaders.Count > 0 then
  begin
    l := TStringList.Create;
    try
      repeat
        s := FSock.RecvString(FTimeout);
        l.Add(s);
        if s = '' then
          Break;
      until FSock.LastError <> 0;
      x := 0;
      while l.Count > x do
      begin
        s := NormalizeHeader(l, x);
        FHeaders.Add(s);
        su := UpperCase(s);
        if Pos('CONTENT-LENGTH:', su) = 1 then
        begin
          Size := StrToIntDef(Trim(SeparateRight(s, ':')), -1);
          if (Size <> -1) and (FTransferEncoding = TE_UNKNOWN) then
            FTransferEncoding := TE_IDENTITY;
        end;
        if Pos('CONTENT-TYPE:', su) = 1 then
          FMimeType := Trim(SeparateRight(s, ':'));
        if Pos('TRANSFER-ENCODING:', su) = 1 then
        begin
          s := Trim(SeparateRight(su, ':'));
          if Pos('CHUNKED', s) > 0 then
            FTransferEncoding := TE_CHUNKED;
        end;
        if UsingProxy then
        begin
          if Pos('PROXY-CONNECTION:', su) = 1 then
            if Pos('CLOSE', su) > 0 then
              ToClose := True;
        end
        else
        begin
          if Pos('CONNECTION:', su) = 1 then
            if Pos('CLOSE', su) > 0 then
              ToClose := True;
        end;
      end;
    finally
      l.free;
    end;
  end;

  Result := FSock.LastError = 0;
  if not Result then
    Exit;

  {if need receive response body, read it}
  Receiving := Method <> 'HEAD';
  Receiving := Receiving and (FResultCode <> 204);
  Receiving := Receiving and (FResultCode <> 304);
  if Receiving then
    case FTransferEncoding of
      TE_UNKNOWN:
        Result := ReadUnknown;
      TE_IDENTITY:
        Result := ReadIdentity(Size);
      TE_CHUNKED:
        Result := ReadChunked;
    end;

  FDocument.Seek(0, soFromBeginning);
  if ToClose then
  begin
    FSock.CloseSocket;
    FAliveHost := '';
    FAlivePort := '';
  end;
  ParseCookies;
end;

function THTTPSend.ReadUnknown: Boolean;
var
  s: ansistring;
begin
  Result := false;
  repeat
    s := FSock.RecvPacket(FTimeout);
    if FSock.LastError = 0 then
      WriteStrToStream(FDocument, s);
  until FSock.LastError <> 0;
  if FSock.LastError = WSAECONNRESET then
  begin
    Result := true;
    FSock.ResetLastError;
  end;
end;

function THTTPSend.ReadIdentity(Size: Integer): Boolean;
begin
  if Size > 0 then
  begin
    FDownloadSize := Size;
    FSock.RecvStreamSize(FDocument, FTimeout, Size);
    FDocument.Position := FDocument.Size;
    Result := FSock.LastError = 0;
  end
  else
    Result := true;
end;

function THTTPSend.ReadChunked: Boolean;
var
  s: ansistring;
  Size: Integer;
begin
  repeat
    repeat
      s := FSock.RecvString(FTimeout);
    until (s <> '') or (FSock.LastError <> 0);
    if FSock.LastError <> 0 then
      Break;
    s := Trim(SeparateLeft(s, ' '));
    s := Trim(SeparateLeft(s, ';'));
    Size := StrToIntDef('$' + s, 0);
    if Size = 0 then
      Break;
    if not ReadIdentity(Size) then
      break;
  until False;
  Result := FSock.LastError = 0;
end;

procedure THTTPSend.ParseCookies;
var
  n: integer;
  s: string;
  sn, sv: string;
begin
  for n := 0 to FHeaders.Count - 1 do
    if Pos('set-cookie:', lowercase(FHeaders[n])) = 1 then
    begin
      s := SeparateRight(FHeaders[n], ':');
      s := trim(SeparateLeft(s, ';'));
      sn := trim(SeparateLeft(s, '='));
      sv := trim(SeparateRight(s, '='));
      FCookies.Values[sn] := sv;
    end;
end;

procedure THTTPSend.Abort;
begin
  FSock.StopFlag := True;
end;

{==============================================================================}

function HttpGetText(const URL: string; const Response: TStrings): Boolean;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    Result := HTTP.HTTPMethod('GET', URL);
    if Result then
      Response.LoadFromStream(HTTP.Document);
  finally
    HTTP.Free;
  end;
end;

function HttpGetBinary(const URL: string; const Response: TStream): Boolean;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    Result := HTTP.HTTPMethod('GET', URL);
    if Result then
    begin
      Response.Seek(0, soFromBeginning);
      Response.CopyFrom(HTTP.Document, 0);
    end;
  finally
    HTTP.Free;
  end;
end;

function HttpPostBinary(const URL: string; const Data: TStream): Boolean;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    HTTP.Document.CopyFrom(Data, 0);
    HTTP.MimeType := 'Application/octet-stream';
    Result := HTTP.HTTPMethod('POST', URL);
    Data.Size := 0;
    if Result then
    begin
      Data.Seek(0, soFromBeginning);
      Data.CopyFrom(HTTP.Document, 0);
    end;
  finally
    HTTP.Free;
  end;
end;

function HttpPostURL(const URL, URLData: string; const Data: TStream): Boolean;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    WriteStrToStream(HTTP.Document, URLData);
    HTTP.MimeType := 'application/x-www-form-urlencoded';
    Result := HTTP.HTTPMethod('POST', URL);
    if Result then
      Data.CopyFrom(HTTP.Document, 0);
  finally
    HTTP.Free;
  end;
end;

function HttpPostFile(const URL, FieldName, FileName: string;
  const Data: TStream; const ResultData: TStrings): Boolean;
var
  HTTP: THTTPSend;
  Bound, s: string;
begin
  Bound := IntToHex(Random(MaxInt), 8) + '_Synapse_boundary';
  HTTP := THTTPSend.Create;
  try
    s := '--' + Bound + CRLF;
    s := s + 'content-disposition: form-data; name="' + FieldName + '";';
    s := s + ' filename="' + FileName +'"' + CRLF;
    s := s + 'Content-Type: Application/octet-string' + CRLF + CRLF;
    WriteStrToStream(HTTP.Document, s);
    HTTP.Document.CopyFrom(Data, 0);
    s := CRLF + '--' + Bound + '--' + CRLF;
    WriteStrToStream(HTTP.Document, s);
    HTTP.MimeType := 'multipart/form-data; boundary=' + Bound;
    Result := HTTP.HTTPMethod('POST', URL);
    if Result then
      ResultData.LoadFromStream(HTTP.Document);
  finally
    HTTP.Free;
  end;
end;

end.
