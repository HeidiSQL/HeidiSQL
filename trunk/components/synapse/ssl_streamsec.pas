{==============================================================================|
| Project : Ararat Synapse                                       | 001.000.006 |
|==============================================================================|
| Content: SSL support by StreamSecII                                          |
|==============================================================================|
| Copyright (c)1999-2005, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)2005.                     |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Henrick Hellström <henrick@streamsec.se>                                   |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(SSL plugin for StreamSecII or OpenStreamSecII)

StreamSecII is native pascal library, you not need any external libraries!

You can tune lot of StreamSecII properties by using your GlobalServer. If you not
using your GlobalServer, then this plugin create own TSimpleTLSInternalServer
instance for each TCP connection. Formore information about GlobalServer usage
refer StreamSecII documentation.

If you are not using key and certificate by GlobalServer, then you can use
properties of this plugin instead, but this have limited features and
@link(TCustomSSL.KeyPassword) not working properly yet!

For handling keys and certificates you can use this properties:
@link(TCustomSSL.CertCAFile), @link(TCustomSSL.CertCA),
@link(TCustomSSL.TrustCertificateFile), @link(TCustomSSL.TrustCertificate),
@link(TCustomSSL.PrivateKeyFile), @link(TCustomSSL.PrivateKey),
@link(TCustomSSL.CertificateFile), @link(TCustomSSL.Certificate),
@link(TCustomSSL.PFXFile). For usage of this properties and for possible formats
of keys and certificates refer to StreamSecII documentation.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

unit ssl_streamsec;

interface

uses
  SysUtils, Classes,
  blcksock, synsock, synautil, synacode,
  TlsInternalServer, TlsSynaSock, TlsConst, StreamSecII, Asn1, X509Base,
  SecUtils;

type
  {:@exclude}
  TMyTLSSynSockSlave = class(TTLSSynSockSlave)
  protected
    procedure SetMyTLSServer(const Value: TCustomTLSInternalServer);
    function GetMyTLSServer: TCustomTLSInternalServer;
  published
    property MyTLSServer: TCustomTLSInternalServer read GetMyTLSServer write SetMyTLSServer;
  end;

  {:@abstract(class implementing StreamSecII SSL plugin.)
   Instance of this class will be created for each @link(TTCPBlockSocket).
   You not need to create instance of this class, all is done by Synapse itself!}
  TSSLStreamSec = class(TCustomSSL)
  protected
    FSlave: TMyTLSSynSockSlave;
    FIsServer: Boolean;
    FTLSServer: TCustomTLSInternalServer;
    FServerCreated: Boolean;
    function SSLCheck: Boolean;
    function Init(server:Boolean): Boolean;
    function DeInit: Boolean;
    function Prepare(server:Boolean): Boolean;
    procedure NotTrustEvent(Sender: TObject; Cert: TASN1Struct; var ExplicitTrust: Boolean);
    function X500StrToStr(const Prefix: string; const Value: TX500String): string;
    function X501NameToStr(const Value: TX501Name): string;
    function GetCert: PASN1Struct;
  public
    constructor Create(const Value: TTCPBlockSocket); override;
    destructor Destroy; override;
    {:See @inherited}
    function LibVersion: String; override;
    {:See @inherited}
    function LibName: String; override;
    {:See @inherited and @link(ssl_streamsec) for more details.}
    function Connect: boolean; override;
    {:See @inherited and @link(ssl_streamsec) for more details.}
    function Accept: boolean; override;
    {:See @inherited}
    function Shutdown: boolean; override;
    {:See @inherited}
    function BiShutdown: boolean; override;
    {:See @inherited}
    function SendBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    {:See @inherited}
    function RecvBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    {:See @inherited}
    function WaitingData: Integer; override;
    {:See @inherited}
    function GetSSLVersion: string; override;
    {:See @inherited}
    function GetPeerSubject: string; override;
    {:See @inherited}
    function GetPeerIssuer: string; override;
    {:See @inherited}
    function GetPeerName: string; override;
    {:See @inherited}
    function GetPeerFingerprint: string; override;
    {:See @inherited}
    function GetCertInfo: string; override;
  published
    {:TLS server for tuning of StreamSecII.}
    property TLSServer: TCustomTLSInternalServer read FTLSServer write FTLSServer;
  end;

implementation

{==============================================================================}
procedure TMyTLSSynSockSlave.SetMyTLSServer(const Value: TCustomTLSInternalServer);
begin
  TLSServer := Value;
end;

function TMyTLSSynSockSlave.GetMyTLSServer: TCustomTLSInternalServer;
begin
  Result := TLSServer;
end;

{==============================================================================}

constructor TSSLStreamSec.Create(const Value: TTCPBlockSocket);
begin
  inherited Create(Value);
  FSlave := nil;
  FIsServer := False;
  FTLSServer := nil;
end;

destructor TSSLStreamSec.Destroy;
begin
  DeInit;
  inherited Destroy;
end;

function TSSLStreamSec.LibVersion: String;
begin
  Result := 'StreamSecII';
end;

function TSSLStreamSec.LibName: String;
begin
  Result := 'ssl_streamsec';
end;

function TSSLStreamSec.SSLCheck: Boolean;
begin
  Result := true;
  FLastErrorDesc := '';
  if not Assigned(FSlave) then
    Exit;
  FLastError := FSlave.ErrorCode;
  if FLastError <> 0 then
  begin
    FLastErrorDesc := TlsConst.AlertMsg(FLastError);
  end;
end;

procedure TSSLStreamSec.NotTrustEvent(Sender: TObject; Cert: TASN1Struct; var ExplicitTrust: Boolean);
begin
  ExplicitTrust := true;
end;

function TSSLStreamSec.Init(server:Boolean): Boolean;
var
  st: TMemoryStream;
  pass: ISecretKey;
  ws: WideString;
begin
  Result := False;
  ws := FKeyPassword;
  pass := TSecretKey.CreateBmpStr(PWideChar(ws), length(ws));
  try
    FIsServer := Server;
    FSlave := TMyTLSSynSockSlave.CreateSocket(FSocket.Socket);
    if Assigned(FTLSServer) then
      FSlave.MyTLSServer := FTLSServer
    else
      if Assigned(TLSInternalServer.GlobalServer) then
        FSlave.MyTLSServer := TLSInternalServer.GlobalServer
      else begin
        FSlave.MyTLSServer := TSimpleTLSInternalServer.Create(nil);
        FServerCreated := True;
      end;
    if server then
      FSlave.MyTLSServer.ClientOrServer := cosServerSide
    else
      FSlave.MyTLSServer.ClientOrServer := cosClientSide;
    if not FVerifyCert then
    begin
      FSlave.MyTLSServer.OnCertNotTrusted := NotTrustEvent;
    end;
    FSlave.MyTLSServer.Options.VerifyServerName := [];
    FSlave.MyTLSServer.Options.Export40Bit := prAllowed;
    FSlave.MyTLSServer.Options.Export56Bit := prAllowed;
    FSlave.MyTLSServer.Options.RequestClientCertificate := False;
    FSlave.MyTLSServer.Options.RequireClientCertificate := False;
    if server and FVerifyCert then
    begin
      FSlave.MyTLSServer.Options.RequestClientCertificate := True;
      FSlave.MyTLSServer.Options.RequireClientCertificate := True;
    end;
    if FCertCAFile <> '' then
      FSlave.MyTLSServer.LoadRootCertsFromFile(CertCAFile);
    if FCertCA <> '' then
    begin
      st := TMemoryStream.Create;
      try
        WriteStrToStream(st, FCertCA);
        st.Seek(0, soFromBeginning);
        FSlave.MyTLSServer.LoadRootCertsFromStream(st);
      finally
        st.free;
      end;
    end;
    if FTrustCertificateFile <> '' then
      FSlave.MyTLSServer.LoadTrustedCertsFromFile(FTrustCertificateFile);
    if FTrustCertificate <> '' then
    begin
      st := TMemoryStream.Create;
      try
        WriteStrToStream(st, FTrustCertificate);
        st.Seek(0, soFromBeginning);
        FSlave.MyTLSServer.LoadTrustedCertsFromStream(st);
      finally
        st.free;
      end;
    end;
    if FPrivateKeyFile <> '' then
      FSlave.MyTLSServer.LoadPrivateKeyRingFromFile(FPrivateKeyFile, pass);
//      FSlave.MyTLSServer.PrivateKeyRing.LoadPrivateKeyFromFile(FPrivateKeyFile, pass);
    if FPrivateKey <> '' then
    begin
      st := TMemoryStream.Create;
      try
        WriteStrToStream(st, FPrivateKey);
        st.Seek(0, soFromBeginning);
        FSlave.MyTLSServer.LoadPrivateKeyRingFromStream(st, pass);
      finally
        st.free;
      end;
    end;
    if FCertificateFile <> '' then
      FSlave.MyTLSServer.LoadMyCertsFromFile(FCertificateFile);
    if FCertificate <> '' then
    begin
      st := TMemoryStream.Create;
      try
        WriteStrToStream(st, FCertificate);
        st.Seek(0, soFromBeginning);
        FSlave.MyTLSServer.LoadMyCertsFromStream(st);
      finally
        st.free;
      end;
    end;
    if FPFXfile <> '' then
      FSlave.MyTLSServer.ImportFromPFX(FPFXfile, pass);
    if server and FServerCreated then
    begin
      FSlave.MyTLSServer.Options.BulkCipherAES128 := prPrefer;
      FSlave.MyTLSServer.Options.BulkCipherAES256 := prAllowed;
      FSlave.MyTLSServer.Options.EphemeralECDHKeySize := ecs256;
      FSlave.MyTLSServer.Options.SignatureRSA := prPrefer;
      FSlave.MyTLSServer.Options.KeyAgreementRSA := prAllowed;
      FSlave.MyTLSServer.Options.KeyAgreementECDHE := prAllowed;
      FSlave.MyTLSServer.Options.KeyAgreementDHE := prPrefer;
      FSlave.MyTLSServer.TLSSetupServer;
    end;
    Result := true;
  finally
    pass := nil;
  end;
end;

function TSSLStreamSec.DeInit: Boolean;
var
  obj: TObject;
begin
  Result := True;
  if assigned(FSlave) then
  begin
    FSlave.Close;
    if FServerCreated then
      obj := FSlave.TLSServer
    else
      obj := nil;
    FSlave.Free;
    obj.Free;
    FSlave := nil;
  end;
  FSSLEnabled := false;
end;

function TSSLStreamSec.Prepare(server:Boolean): Boolean;
begin
  Result := false;
  DeInit;
  if Init(server) then
    Result := true
  else
    DeInit;
end;

function TSSLStreamSec.Connect: boolean;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if Prepare(false) then
  begin
    FSlave.Open;
    SSLCheck;
    if FLastError <> 0 then
      Exit;
    FSSLEnabled := True;
    Result := True;
  end;
end;

function TSSLStreamSec.Accept: boolean;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if Prepare(true) then
  begin
    FSlave.DoConnect;
    SSLCheck;
    if FLastError <> 0 then
      Exit;
    FSSLEnabled := True;
    Result := True;
  end;
end;

function TSSLStreamSec.Shutdown: boolean;
begin
  Result := BiShutdown;
end;

function TSSLStreamSec.BiShutdown: boolean;
begin
  DeInit;
  Result := True;
end;

function TSSLStreamSec.SendBuffer(Buffer: TMemory; Len: Integer): Integer;
var
  l: integer;
begin
  l := len;
  FSlave.SendBuf(Buffer^, l, true);
  Result := l;
  SSLCheck;
end;

function TSSLStreamSec.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
var
  l: integer;
begin
  l := Len;
  Result := FSlave.ReceiveBuf(Buffer^, l);
  SSLCheck;
end;

function TSSLStreamSec.WaitingData: Integer;
begin
  Result := 0;
  while FSlave.Connected do begin
    Result := FSlave.ReceiveLength;
    if Result > 0 then
      Break;
    Sleep(1);
  end;
end;

function TSSLStreamSec.GetSSLVersion: string;
begin
  Result := 'SSLv3 or TLSv1';
end;

function TSSLStreamSec.GetCert: PASN1Struct;
begin
  if FIsServer then
    Result := FSlave.GetClientCert
  else
    Result := FSlave.GetServerCert;
end;

function TSSLStreamSec.GetPeerSubject: string;
var
  XName: TX501Name;
  Cert: PASN1Struct;
begin
  Result := '';
  Cert := GetCert;
  if Assigned(cert) then
  begin
    ExtractSubject(Cert^,XName, false);
    Result := X501NameToStr(XName);
  end;
end;

function TSSLStreamSec.GetPeerName: string;
var
  XName: TX501Name;
  Cert: PASN1Struct;
begin
  Result := '';
  Cert := GetCert;
  if Assigned(cert) then
  begin
    ExtractSubject(Cert^,XName, false);
    Result := XName.commonName.Str;
  end;
end;

function TSSLStreamSec.GetPeerIssuer: string;
var
  XName: TX501Name;
  Cert: PASN1Struct;
begin
  Result := '';
  Cert := GetCert;
  if Assigned(cert) then
  begin
    ExtractIssuer(Cert^, XName, false);
    Result := X501NameToStr(XName);
  end;
end;

function TSSLStreamSec.GetPeerFingerprint: string;
var
  Cert: PASN1Struct;
begin
  Result := '';
  Cert := GetCert;
  if Assigned(cert) then
    Result := MD5(Cert.ContentAsOctetString);
end;

function TSSLStreamSec.GetCertInfo: string;
var
  Cert: PASN1Struct;
  l: Tstringlist;
begin
  Result := '';
  Cert := GetCert;
  if Assigned(cert) then
  begin
    l := TStringList.Create;
    try
      Asn1.RenderAsText(cert^, l, true, true, true, 2);
      Result := l.Text;
    finally
      l.free;
    end;
  end;
end;

function TSSLStreamSec.X500StrToStr(const Prefix: string;
  const Value: TX500String): string;
begin
  if Value.Str = '' then
    Result := ''
  else
    Result := '/' + Prefix + '=' + Value.Str;
end;

function TSSLStreamSec.X501NameToStr(const Value: TX501Name): string;
begin
  Result := X500StrToStr('CN',Value.commonName) +
           X500StrToStr('C',Value.countryName) +
           X500StrToStr('L',Value.localityName) +
           X500StrToStr('ST',Value.stateOrProvinceName) +
           X500StrToStr('O',Value.organizationName) +
           X500StrToStr('OU',Value.organizationalUnitName) +
           X500StrToStr('T',Value.title) +
           X500StrToStr('N',Value.name) +
           X500StrToStr('G',Value.givenName) +
           X500StrToStr('I',Value.initials) +
           X500StrToStr('SN',Value.surname) +
           X500StrToStr('GQ',Value.generationQualifier) +
           X500StrToStr('DNQ',Value.dnQualifier) +
           X500StrToStr('E',Value.emailAddress);
end;


{==============================================================================}

initialization
  SSLImplementation := TSSLStreamSec;

finalization

end.


