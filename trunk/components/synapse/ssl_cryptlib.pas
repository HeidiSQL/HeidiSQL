{==============================================================================|
| Project : Ararat Synapse                                       | 001.001.000 |
|==============================================================================|
| Content: SSL/SSH support by Peter Gutmann's CryptLib                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2005-2012.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(SSL/SSH plugin for CryptLib)

This plugin requires cl32.dll at least version 3.2.0! It can be used on Win32
and Linux. This library is staticly linked - when you compile your application
with this plugin, you MUST distribute it with Cryptib library, otherwise you
cannot run your application!

It can work with keys and certificates stored as PKCS#15 only! It must be stored
as disk file only, you cannot load them from memory! Each file can hold multiple
keys and certificates. You must identify it by 'label' stored in
@link(TSSLCryptLib.PrivateKeyLabel).

If you need to use secure connection and authorize self by certificate
(each SSL/TLS server or client with client authorization), then use
@link(TCustomSSL.PrivateKeyFile), @link(TSSLCryptLib.PrivateKeyLabel) and
@link(TCustomSSL.KeyPassword) properties.

If you need to use server what verifying client certificates, then use
@link(TCustomSSL.CertCAFile) as PKCS#15 file with public keyas of allowed clients. Clients
with non-matching certificates will be rejected by cryptLib.

This plugin is capable to create Ad-Hoc certificates. When you start SSL/TLS
server without explicitly assigned key and certificate, then this plugin create
Ad-Hoc key and certificate for each incomming connection by self. It slowdown
accepting of new connections!

You can use this plugin for SSHv2 connections too! You must explicitly set
@link(TCustomSSL.SSLType) to value LT_SSHv2 and set @link(TCustomSSL.username)
and @link(TCustomSSL.password). You can use special SSH channels too, see
@link(TCustomSSL).
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

unit ssl_cryptlib;

interface

uses
  Windows,
  SysUtils,
  blcksock, synsock, synautil, synacode,
  cryptlib;

type
  {:@abstract(class implementing CryptLib SSL/SSH plugin.)
   Instance of this class will be created for each @link(TTCPBlockSocket).
   You not need to create instance of this class, all is done by Synapse itself!}
  TSSLCryptLib = class(TCustomSSL)
  protected
    FCryptSession: CRYPT_SESSION;
    FPrivateKeyLabel: string;
    FDelCert: Boolean;
    FReadBuffer: string;
    FTrustedCAs: array of integer;
    function SSLCheck(Value: integer): Boolean;
    function Init(server:Boolean): Boolean;
    function DeInit: Boolean;
    function Prepare(server:Boolean): Boolean;
    function GetString(const cryptHandle: CRYPT_HANDLE; const attributeType: CRYPT_ATTRIBUTE_TYPE): string;
    function CreateSelfSignedCert(Host: string): Boolean; override;
    function PopAll: string;
  public
    {:See @inherited}
    constructor Create(const Value: TTCPBlockSocket); override;
    destructor Destroy; override;
    {:Load trusted CA's in PEM format}
    procedure SetCertCAFile(const Value: string); override;
    {:See @inherited}
    function LibVersion: String; override;
    {:See @inherited}
    function LibName: String; override;
    {:See @inherited}
    procedure Assign(const Value: TCustomSSL); override;
    {:See @inherited and @link(ssl_cryptlib) for more details.}
    function Connect: boolean; override;
    {:See @inherited and @link(ssl_cryptlib) for more details.}
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
    function GetVerifyCert: integer; override;
  published
    {:name of certificate/key within PKCS#15 file. It can hold more then one
     certificate/key and each certificate/key must have unique label within one file.}
    property PrivateKeyLabel: string read FPrivateKeyLabel Write FPrivateKeyLabel;
  end;

implementation

{==============================================================================}

constructor TSSLCryptLib.Create(const Value: TTCPBlockSocket);
begin
  inherited Create(Value);
  FcryptSession := CRYPT_SESSION(CRYPT_SESSION_NONE);
  FPrivateKeyLabel := 'synapse';
  FDelCert := false;
  FTrustedCAs := nil;
end;

destructor TSSLCryptLib.Destroy;
begin
  SetCertCAFile('');  // destroy certificates
  DeInit;
  inherited Destroy;
end;

procedure TSSLCryptLib.Assign(const Value: TCustomSSL);
begin
  inherited Assign(Value);
  if Value is TSSLCryptLib then
  begin
    FPrivateKeyLabel := TSSLCryptLib(Value).privatekeyLabel;
  end;
end;

function TSSLCryptLib.GetString(const cryptHandle: CRYPT_HANDLE; const attributeType: CRYPT_ATTRIBUTE_TYPE): string;
var
  l: integer;
begin
  l := 0;
  cryptGetAttributeString(cryptHandle, attributeType, nil, l);
  setlength(Result, l);
  cryptGetAttributeString(cryptHandle, attributeType, pointer(Result), l);
  setlength(Result, l);
end;

function TSSLCryptLib.LibVersion: String;
var
  x: integer;
begin
  Result := GetString(CRYPT_UNUSED, CRYPT_OPTION_INFO_DESCRIPTION);
  cryptGetAttribute(CRYPT_UNUSED, CRYPT_OPTION_INFO_MAJORVERSION, x);
  Result := Result + ' v' + IntToStr(x);
  cryptGetAttribute(CRYPT_UNUSED, CRYPT_OPTION_INFO_MINORVERSION, x);
  Result := Result + '.' + IntToStr(x);
  cryptGetAttribute(CRYPT_UNUSED, CRYPT_OPTION_INFO_STEPPING, x);
  Result := Result + '.' + IntToStr(x);
end;

function TSSLCryptLib.LibName: String;
begin
  Result := 'ssl_cryptlib';
end;

function TSSLCryptLib.SSLCheck(Value: integer): Boolean;
begin
  Result := true;
  FLastErrorDesc := '';
  if Value = CRYPT_ERROR_COMPLETE then
    Value := 0;
  FLastError := Value;
  if FLastError <> 0 then
  begin
    Result := False;
{$IF CRYPTLIB_VERSION >= 3400}
    FLastErrorDesc := GetString(FCryptSession, CRYPT_ATTRIBUTE_ERRORMESSAGE);
{$ELSE}
    FLastErrorDesc := GetString(FCryptSession, CRYPT_ATTRIBUTE_INT_ERRORMESSAGE);
{$IFEND}
  end;
end;

function TSSLCryptLib.CreateSelfSignedCert(Host: string): Boolean;
var
  privateKey: CRYPT_CONTEXT;
  keyset: CRYPT_KEYSET;
  cert: CRYPT_CERTIFICATE;
  publicKey: CRYPT_CONTEXT;
begin
  if FPrivatekeyFile = '' then
    FPrivatekeyFile := GetTempFile('', 'key');
  cryptCreateContext(privateKey, CRYPT_UNUSED, CRYPT_ALGO_RSA);
  cryptSetAttributeString(privateKey, CRYPT_CTXINFO_LABEL, Pointer(FPrivatekeyLabel),
    Length(FPrivatekeyLabel));
  cryptSetAttribute(privateKey, CRYPT_CTXINFO_KEYSIZE, 1024);
  cryptGenerateKey(privateKey);
  cryptKeysetOpen(keyset, CRYPT_UNUSED, CRYPT_KEYSET_FILE, PChar(FPrivatekeyFile), CRYPT_KEYOPT_CREATE);
  FDelCert := True;
  cryptAddPrivateKey(keyset, privateKey, PChar(FKeyPassword));
  cryptCreateCert(cert, CRYPT_UNUSED, CRYPT_CERTTYPE_CERTIFICATE);
  cryptSetAttribute(cert, CRYPT_CERTINFO_XYZZY, 1);
  cryptGetPublicKey(keyset, publicKey, CRYPT_KEYID_NAME, PChar(FPrivatekeyLabel));
  cryptSetAttribute(cert, CRYPT_CERTINFO_SUBJECTPUBLICKEYINFO, publicKey);
  cryptSetAttributeString(cert, CRYPT_CERTINFO_COMMONNAME, Pointer(host), Length(host));
  cryptSignCert(cert, privateKey);
  cryptAddPublicKey(keyset, cert);
  cryptKeysetClose(keyset);
  cryptDestroyCert(cert);
  cryptDestroyContext(privateKey);
  cryptDestroyContext(publicKey);
  Result := True;
end;

function TSSLCryptLib.PopAll: string;
const
  BufferMaxSize = 32768;
var
  Outbuffer: string;
  WriteLen: integer;
begin
  Result := '';
  repeat
    setlength(outbuffer, BufferMaxSize);
    Writelen := 0;
    SSLCheck(CryptPopData(FCryptSession, @OutBuffer[1], BufferMaxSize, Writelen));
    if FLastError <> 0 then
      Break;
    if WriteLen > 0 then
    begin
      setlength(outbuffer, WriteLen);
      Result := Result + outbuffer;
    end;
  until WriteLen = 0;
end;

function TSSLCryptLib.Init(server:Boolean): Boolean;
var
  st: CRYPT_SESSION_TYPE;
  keysetobj: CRYPT_KEYSET;
  cryptContext: CRYPT_CONTEXT;
  x: integer;
begin
  Result := False;
  FLastErrorDesc := '';
  FLastError := 0;
  FDelCert := false;
  FcryptSession := CRYPT_SESSION(CRYPT_SESSION_NONE);
  if server then
    case FSSLType of
      LT_all, LT_SSLv3, LT_TLSv1, LT_TLSv1_1:
        st := CRYPT_SESSION_SSL_SERVER;
      LT_SSHv2:
        st := CRYPT_SESSION_SSH_SERVER;
    else
      Exit;
    end
  else
    case FSSLType of
      LT_all, LT_SSLv3, LT_TLSv1, LT_TLSv1_1:
        st := CRYPT_SESSION_SSL;
      LT_SSHv2:
        st := CRYPT_SESSION_SSH;
    else
      Exit;
    end;
  if not SSLCheck(cryptCreateSession(FcryptSession, CRYPT_UNUSED, st)) then
    Exit;
  x := -1;
  case FSSLType of
    LT_SSLv3:
      x := 0;
    LT_TLSv1:
      x := 1;
    LT_TLSv1_1:
      x := 2;
  end;
  if x >= 0 then
    if not SSLCheck(cryptSetAttribute(FCryptSession, CRYPT_SESSINFO_VERSION, x)) then
      Exit;

  if (FCertComplianceLevel <> -1) then
    if not SSLCheck(cryptSetAttribute (CRYPT_UNUSED, CRYPT_OPTION_CERT_COMPLIANCELEVEL,
      FCertComplianceLevel)) then
      Exit;

  if FUsername <> '' then
  begin
    cryptSetAttributeString(FcryptSession, CRYPT_SESSINFO_USERNAME,
      Pointer(FUsername), Length(FUsername));
    cryptSetAttributeString(FcryptSession, CRYPT_SESSINFO_PASSWORD,
      Pointer(FPassword), Length(FPassword));
  end;
  if FSSLType = LT_SSHv2 then
    if FSSHChannelType <> '' then
    begin
      cryptSetAttribute(FCryptSession, CRYPT_SESSINFO_SSH_CHANNEL, CRYPT_UNUSED);
      cryptSetAttributeString(FCryptSession, CRYPT_SESSINFO_SSH_CHANNEL_TYPE,
        Pointer(FSSHChannelType), Length(FSSHChannelType));
      if FSSHChannelArg1 <> '' then
        cryptSetAttributeString(FCryptSession, CRYPT_SESSINFO_SSH_CHANNEL_ARG1,
          Pointer(FSSHChannelArg1), Length(FSSHChannelArg1));
      if FSSHChannelArg2 <> '' then
        cryptSetAttributeString(FCryptSession, CRYPT_SESSINFO_SSH_CHANNEL_ARG2,
          Pointer(FSSHChannelArg2), Length(FSSHChannelArg2));
    end;


  if server and (FPrivatekeyFile = '') then
  begin
    if FPrivatekeyLabel = '' then
      FPrivatekeyLabel := 'synapse';
    if FkeyPassword = '' then
      FkeyPassword := 'synapse';
    CreateSelfSignedcert(FSocket.ResolveIPToName(FSocket.GetRemoteSinIP));
  end;

  if (FPrivatekeyLabel <> '') and (FPrivatekeyFile <> '') then
  begin
    if not SSLCheck(cryptKeysetOpen(KeySetObj, CRYPT_UNUSED, CRYPT_KEYSET_FILE,
      PChar(FPrivatekeyFile), CRYPT_KEYOPT_READONLY)) then
      Exit;
    try
    if not SSLCheck(cryptGetPrivateKey(KeySetObj, cryptcontext, CRYPT_KEYID_NAME,
      PChar(FPrivatekeyLabel), PChar(FKeyPassword))) then
      Exit;
    if not SSLCheck(cryptSetAttribute(FcryptSession, CRYPT_SESSINFO_PRIVATEKEY,
      cryptcontext)) then
      Exit;
    finally
      cryptKeysetClose(keySetObj);
      cryptDestroyContext(cryptcontext);
    end;
  end;
  if server and FVerifyCert then
  begin
    if not SSLCheck(cryptKeysetOpen(KeySetObj, CRYPT_UNUSED, CRYPT_KEYSET_FILE,
      PChar(FCertCAFile), CRYPT_KEYOPT_READONLY)) then
      Exit;
    try
    if not SSLCheck(cryptSetAttribute(FcryptSession, CRYPT_SESSINFO_KEYSET,
      keySetObj)) then
      Exit;
    finally
      cryptKeysetClose(keySetObj);
    end;
  end;
  Result := true;
end;

function TSSLCryptLib.DeInit: Boolean;
begin
  Result := True;
  if FcryptSession <> CRYPT_SESSION(CRYPT_SESSION_NONE) then
    CryptDestroySession(FcryptSession);
  FcryptSession := CRYPT_SESSION(CRYPT_SESSION_NONE);
  FSSLEnabled := False;
  if FDelCert then
    SysUtils.DeleteFile(FPrivatekeyFile);
end;

function TSSLCryptLib.Prepare(server:Boolean): Boolean;
begin
  Result := false;
  DeInit;
  if Init(server) then
    Result := true
  else
    DeInit;
end;

function TSSLCryptLib.Connect: boolean;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if Prepare(false) then
  begin
    if not SSLCheck(cryptSetAttribute(FCryptSession, CRYPT_SESSINFO_NETWORKSOCKET, FSocket.Socket)) then
      Exit;
    if not SSLCheck(cryptSetAttribute(FCryptSession, CRYPT_SESSINFO_ACTIVE, 1)) then
      Exit;
    if FverifyCert then
      if (GetVerifyCert <> 0) or (not DoVerifyCert) then
        Exit;
    FSSLEnabled := True;
    Result := True;
    FReadBuffer := '';
  end;
end;

function TSSLCryptLib.Accept: boolean;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if Prepare(true) then
  begin
    if not SSLCheck(cryptSetAttribute(FCryptSession, CRYPT_SESSINFO_NETWORKSOCKET, FSocket.Socket)) then
      Exit;
    if not SSLCheck(cryptSetAttribute(FCryptSession, CRYPT_SESSINFO_ACTIVE, 1)) then
      Exit;
    FSSLEnabled := True;
    Result := True;
    FReadBuffer := '';
  end;
end;

function TSSLCryptLib.Shutdown: boolean;
begin
  Result := BiShutdown;
end;

function TSSLCryptLib.BiShutdown: boolean;
begin
  if FcryptSession <> CRYPT_SESSION(CRYPT_SESSION_NONE) then
    cryptSetAttribute(FCryptSession, CRYPT_SESSINFO_ACTIVE, 0);
  DeInit;
  FReadBuffer := '';
  Result := True;
end;

function TSSLCryptLib.SendBuffer(Buffer: TMemory; Len: Integer): Integer;
var
  l: integer;
begin
  FLastError := 0;
  FLastErrorDesc := '';
  SSLCheck(cryptPushData(FCryptSession, Buffer, Len, L));
  cryptFlushData(FcryptSession);
  Result := l;
end;

function TSSLCryptLib.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
begin
  FLastError := 0;
  FLastErrorDesc := '';
  if Length(FReadBuffer) = 0 then
    FReadBuffer := PopAll;
  if Len > Length(FReadBuffer) then
    Len := Length(FReadBuffer);
  Move(Pointer(FReadBuffer)^, buffer^, Len);
  Delete(FReadBuffer, 1, Len);
  Result := Len;
end;

function TSSLCryptLib.WaitingData: Integer;
begin
  Result := Length(FReadBuffer);
end;

function TSSLCryptLib.GetSSLVersion: string;
var
  x: integer;
begin
  Result := '';
  if FcryptSession = CRYPT_SESSION(CRYPT_SESSION_NONE) then
    Exit;
  cryptGetAttribute(FCryptSession, CRYPT_SESSINFO_VERSION, x);
  if FSSLType in [LT_SSLv3, LT_TLSv1, LT_TLSv1_1, LT_all] then
    case x of
      0:
        Result := 'SSLv3';
      1:
        Result := 'TLSv1';
      2:
        Result := 'TLSv1.1';
    end;
  if FSSLType in [LT_SSHv2] then
    case x of
      0:
        Result := 'SSHv1';
      1:
        Result := 'SSHv2';
    end;
end;

function TSSLCryptLib.GetPeerSubject: string;
var
  cert: CRYPT_CERTIFICATE;
begin
  Result := '';
  if FcryptSession = CRYPT_SESSION(CRYPT_SESSION_NONE) then
    Exit;
  cryptGetAttribute(FCryptSession, CRYPT_SESSINFO_RESPONSE, cert);
  cryptSetAttribute(cert, CRYPT_ATTRIBUTE_CURRENT, CRYPT_CERTINFO_SUBJECTNAME);
  Result := GetString(cert, CRYPT_CERTINFO_DN);
  cryptDestroyCert(cert);
end;

function TSSLCryptLib.GetPeerName: string;
var
  cert: CRYPT_CERTIFICATE;
begin
  Result := '';
  if FcryptSession = CRYPT_SESSION(CRYPT_SESSION_NONE) then
    Exit;
  cryptGetAttribute(FCryptSession, CRYPT_SESSINFO_RESPONSE, cert);
  cryptSetAttribute(cert, CRYPT_ATTRIBUTE_CURRENT, CRYPT_CERTINFO_SUBJECTNAME);
  Result := GetString(cert, CRYPT_CERTINFO_COMMONNAME);
  cryptDestroyCert(cert);
end;

function TSSLCryptLib.GetPeerIssuer: string;
var
  cert: CRYPT_CERTIFICATE;
begin
  Result := '';
  if FcryptSession = CRYPT_SESSION(CRYPT_SESSION_NONE) then
    Exit;
  cryptGetAttribute(FCryptSession, CRYPT_SESSINFO_RESPONSE, cert);
  cryptSetAttribute(cert, CRYPT_ATTRIBUTE_CURRENT, CRYPT_CERTINFO_ISSUERNAME);
  Result := GetString(cert, CRYPT_CERTINFO_COMMONNAME);
  cryptDestroyCert(cert);
end;

function TSSLCryptLib.GetPeerFingerprint: string;
var
  cert: CRYPT_CERTIFICATE;
begin
  Result := '';
  if FcryptSession = CRYPT_SESSION(CRYPT_SESSION_NONE) then
    Exit;
  cryptGetAttribute(FCryptSession, CRYPT_SESSINFO_RESPONSE, cert);
  Result := GetString(cert, CRYPT_CERTINFO_FINGERPRINT);
  cryptDestroyCert(cert);
end;


procedure TSSLCryptLib.SetCertCAFile(const Value: string);

var F:textfile;
  bInCert:boolean;
  s,sCert:string;
  cert: CRYPT_CERTIFICATE;
  idx:integer;

begin
if assigned(FTrustedCAs) then
  begin
  for idx := 0 to High(FTrustedCAs) do
    cryptDestroyCert(FTrustedCAs[idx]);
  FTrustedCAs:=nil;
  end;
if Value<>'' then
  begin
  AssignFile(F,Value);
  reset(F);
  bInCert:=false;
  idx:=0;
  while not eof(F) do
    begin
    readln(F,s);
    if pos('-----END CERTIFICATE-----',s)>0 then
      begin
      bInCert:=false;
      cert:=0;
      if (cryptImportCert(PAnsiChar(sCert),length(sCert)-2,CRYPT_UNUSED,cert)=CRYPT_OK) then
        begin
        cryptSetAttribute( cert, CRYPT_CERTINFO_TRUSTED_IMPLICIT, 1 );
        SetLength(FTrustedCAs,idx+1);
        FTrustedCAs[idx]:=cert;
        idx:=idx+1;
        end;
      end;
    if bInCert then
      sCert:=sCert+s+#13#10;
    if pos('-----BEGIN CERTIFICATE-----',s)>0 then
      begin
      bInCert:=true;
      sCert:='';
      end;
    end;
  CloseFile(F);
  end;
end;

function TSSLCryptLib.GetVerifyCert: integer;
var
  cert: CRYPT_CERTIFICATE;
  itype,ilocus:integer;
begin
  Result := -1;
  if FcryptSession = CRYPT_SESSION(CRYPT_SESSION_NONE) then
    Exit;
  cryptGetAttribute(FCryptSession, CRYPT_SESSINFO_RESPONSE, cert);
  result:=cryptCheckCert(cert,CRYPT_UNUSED);
  if result<>CRYPT_OK then
    begin
    //get extended error info if available
    cryptGetAttribute(cert,CRYPT_ATTRIBUTE_ERRORtype,itype);
    cryptGetAttribute(cert,CRYPT_ATTRIBUTE_ERRORLOCUS,ilocus);
    cryptSetAttribute(cert, CRYPT_ATTRIBUTE_CURRENT, CRYPT_CERTINFO_SUBJECTNAME);
    FLastError := Result;
    FLastErrorDesc := format('SSL/TLS certificate verification failed for "%s"'#13#10'Status: %d. ERRORTYPE: %d. ERRORLOCUS: %d.',
      [GetString(cert, CRYPT_CERTINFO_COMMONNAME),result,itype,ilocus]);
    end;
  cryptDestroyCert(cert);
end;

{==============================================================================}

var imajor,iminor,iver:integer;
//    e: ESynapseError;

initialization
  if cryptInit = CRYPT_OK then
    SSLImplementation := TSSLCryptLib;
  cryptAddRandom(nil, CRYPT_RANDOM_SLOWPOLL);
  cryptGetAttribute (CRYPT_UNUSED, CRYPT_OPTION_INFO_MAJORVERSION,imajor);
  cryptGetAttribute (CRYPT_UNUSED, CRYPT_OPTION_INFO_MINORVERSION,iminor);
// according to the documentation CRYPTLIB version has 3 digits. recent versions use 4 digits
  if CRYPTLIB_VERSION >1000 then
    iver:=CRYPTLIB_VERSION div 100
  else
    iver:=CRYPTLIB_VERSION div 10;
  if (iver <> imajor*10+iminor) then
  begin
    SSLImplementation :=TSSLNone;
//    e := ESynapseError.Create(format('Error wrong cryptlib version (is %d.%d expected %d.%d). ',
//       [imajor,iminor,iver div 10, iver mod 10]));
//    e.ErrorCode := 0;
//    e.ErrorMessage := format('Error wrong cryptlib version (%d.%d expected %d.%d)',
//       [imajor,iminor,iver div 10, iver mod 10]);
//    raise e;
  end;
finalization
  cryptEnd;
end.


