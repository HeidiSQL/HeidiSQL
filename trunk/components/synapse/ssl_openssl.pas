{==============================================================================|
| Project : Ararat Synapse                                       | 001.002.000 |
|==============================================================================|
| Content: SSL support by OpenSSL                                              |
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
| Portions created by Petr Fejfar are Copyright (c)2011-2012.                  |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

//requires OpenSSL libraries!

{:@abstract(SSL plugin for OpenSSL)

You need OpenSSL libraries version 0.9.7. It can work with 0.9.6 too, but
application mysteriously crashing when you are using freePascal on Linux.
Use Kylix on Linux is OK! If you have version 0.9.7 on Linux, then I not see
any problems with FreePascal.

OpenSSL libraries are loaded dynamicly - you not need OpenSSl librares even you
compile your application with this unit. SSL just not working when you not have
OpenSSL libraries.

This plugin have limited support for .NET too! Because is not possible to use
callbacks with CDECL calling convention under .NET, is not supported
key/certificate passwords and multithread locking. :-(

For handling keys and certificates you can use this properties:

@link(TCustomSSL.CertificateFile) for PEM or ASN1 DER (cer) format. @br
@link(TCustomSSL.Certificate) for ASN1 DER format only. @br
@link(TCustomSSL.PrivateKeyFile) for PEM or ASN1 DER (key) format. @br
@link(TCustomSSL.PrivateKey) for ASN1 DER format only. @br
@link(TCustomSSL.CertCAFile) for PEM CA certificate bundle. @br
@link(TCustomSSL.PFXFile) for PFX format. @br
@link(TCustomSSL.PFX) for PFX format from binary string. @br

This plugin is capable to create Ad-Hoc certificates. When you start SSL/TLS
server without explicitly assigned key and certificate, then this plugin create
Ad-Hoc key and certificate for each incomming connection by self. It slowdown
accepting of new connections!
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit ssl_openssl;

interface

uses
  SysUtils, Classes,
  blcksock, synsock, synautil,
{$IFDEF CIL}
  System.Text,
{$ENDIF}
  ssl_openssl_lib;

type
  {:@abstract(class implementing OpenSSL SSL plugin.)
   Instance of this class will be created for each @link(TTCPBlockSocket).
   You not need to create instance of this class, all is done by Synapse itself!}
  TSSLOpenSSL = class(TCustomSSL)
  protected
    FSsl: PSSL;
    Fctx: PSSL_CTX;
    function SSLCheck: Boolean;
    function SetSslKeys: boolean;
    function Init(server:Boolean): Boolean;
    function DeInit: Boolean;
    function Prepare(server:Boolean): Boolean;
    function LoadPFX(pfxdata: ansistring): Boolean;
    function CreateSelfSignedCert(Host: string): Boolean; override;
  public
    {:See @inherited}
    constructor Create(const Value: TTCPBlockSocket); override;
    destructor Destroy; override;
    {:See @inherited}
    function LibVersion: String; override;
    {:See @inherited}
    function LibName: String; override;
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
    function GetPeerSerialNo: integer; override; {pf}
    {:See @inherited}
    function GetPeerIssuer: string; override;
    {:See @inherited}
    function GetPeerName: string; override;
    {:See @inherited}
    function GetPeerNameHash: cardinal; override; {pf}
    {:See @inherited}
    function GetPeerFingerprint: string; override;
    {:See @inherited}
    function GetCertInfo: string; override;
    {:See @inherited}
    function GetCipherName: string; override;
    {:See @inherited}
    function GetCipherBits: integer; override;
    {:See @inherited}
    function GetCipherAlgBits: integer; override;
    {:See @inherited}
    function GetVerifyCert: integer; override;
  end;

implementation

{==============================================================================}

{$IFNDEF CIL}
function PasswordCallback(buf:PAnsiChar; size:Integer; rwflag:Integer; userdata: Pointer):Integer; cdecl;
var
  Password: AnsiString;
begin
  Password := '';
  if TCustomSSL(userdata) is TCustomSSL then
    Password := TCustomSSL(userdata).KeyPassword;
  if Length(Password) > (Size - 1) then
    SetLength(Password, Size - 1);
  Result := Length(Password);
  StrLCopy(buf, PAnsiChar(Password + #0), Result + 1);
end;
{$ENDIF}

{==============================================================================}

constructor TSSLOpenSSL.Create(const Value: TTCPBlockSocket);
begin
  inherited Create(Value);
  FCiphers := 'DEFAULT';
  FSsl := nil;
  Fctx := nil;
end;

destructor TSSLOpenSSL.Destroy;
begin
  DeInit;
  inherited Destroy;
end;

function TSSLOpenSSL.LibVersion: String;
begin
  Result := SSLeayversion(0);
end;

function TSSLOpenSSL.LibName: String;
begin
  Result := 'ssl_openssl';
end;

function TSSLOpenSSL.SSLCheck: Boolean;
var
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
  s : AnsiString;
begin
  Result := true;
  FLastErrorDesc := '';
  FLastError := ErrGetError;
  ErrClearError;
  if FLastError <> 0 then
  begin
    Result := False;
{$IFDEF CIL}
    sb := StringBuilder.Create(256);
    ErrErrorString(FLastError, sb, 256);
    FLastErrorDesc := Trim(sb.ToString);
{$ELSE}
    s := StringOfChar(#0, 256);
    ErrErrorString(FLastError, s, Length(s));
    FLastErrorDesc := s;
{$ENDIF}
  end;
end;

function TSSLOpenSSL.CreateSelfSignedCert(Host: string): Boolean;
var
  pk: EVP_PKEY;
  x: PX509;
  rsa: PRSA;
  t: PASN1_UTCTIME;
  name: PX509_NAME;
  b: PBIO;
  xn, y: integer;
  s: AnsiString;
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
begin
  Result := True;
  pk := EvpPkeynew;
  x := X509New;
  try
    rsa := RsaGenerateKey(1024, $10001, nil, nil);
    EvpPkeyAssign(pk, EVP_PKEY_RSA, rsa);
    X509SetVersion(x, 2);
    Asn1IntegerSet(X509getSerialNumber(x), 0);
    t := Asn1UtctimeNew;
    try
      X509GmtimeAdj(t, -60 * 60 *24);
      X509SetNotBefore(x, t);
      X509GmtimeAdj(t, 60 * 60 * 60 *24);
      X509SetNotAfter(x, t);
    finally
      Asn1UtctimeFree(t);
    end;
    X509SetPubkey(x, pk);
    Name := X509GetSubjectName(x);
    X509NameAddEntryByTxt(Name, 'C', $1001, 'CZ', -1, -1, 0);
    X509NameAddEntryByTxt(Name, 'CN', $1001, host, -1, -1, 0);
    x509SetIssuerName(x, Name);
    x509Sign(x, pk, EvpGetDigestByName('SHA1'));
    b := BioNew(BioSMem);
    try
      i2dX509Bio(b, x);
      xn := bioctrlpending(b);
{$IFDEF CIL}
      sb := StringBuilder.Create(xn);
      y := bioread(b, sb, xn);
      if y > 0 then
      begin
        sb.Length := y;
        s := sb.ToString;
      end;
{$ELSE}
      setlength(s, xn);
      y := bioread(b, s, xn);
      if y > 0 then
        setlength(s, y);
{$ENDIF}
    finally
      BioFreeAll(b);
    end;
    FCertificate := s;
    b := BioNew(BioSMem);
    try
      i2dPrivatekeyBio(b, pk);
      xn := bioctrlpending(b);
{$IFDEF CIL}
      sb := StringBuilder.Create(xn);
      y := bioread(b, sb, xn);
      if y > 0 then
      begin
        sb.Length := y;
        s := sb.ToString;
      end;
{$ELSE}
      setlength(s, xn);
      y := bioread(b, s, xn);
      if y > 0 then
        setlength(s, y);
{$ENDIF}
    finally
      BioFreeAll(b);
    end;
    FPrivatekey := s;
  finally
    X509free(x);
    EvpPkeyFree(pk);
  end;
end;

function TSSLOpenSSL.LoadPFX(pfxdata: Ansistring): Boolean;
var
  cert, pkey, ca: SslPtr;
  b: PBIO;
  p12: SslPtr;
begin
  Result := False;
  b := BioNew(BioSMem);
  try
    BioWrite(b, pfxdata, Length(PfxData));
    p12 := d2iPKCS12bio(b, nil);
    if not Assigned(p12) then
      Exit;
    try
      cert := nil;
      pkey := nil;
      ca := nil;
      try {pf}
        if PKCS12parse(p12, FKeyPassword, pkey, cert, ca) > 0 then
          if SSLCTXusecertificate(Fctx, cert) > 0 then
            if SSLCTXusePrivateKey(Fctx, pkey) > 0 then
              Result := True;
      {pf}
      finally
        EvpPkeyFree(pkey);
        X509free(cert);
        SkX509PopFree(ca,_X509Free); // for ca=nil a new STACK was allocated...
      end;
      {/pf}
    finally
      PKCS12free(p12);
    end;
  finally
    BioFreeAll(b);
  end;
end;

function TSSLOpenSSL.SetSslKeys: boolean;
var
  st: TFileStream;
  s: string;
begin
  Result := False;
  if not assigned(FCtx) then
    Exit;
  try
    if FCertificateFile <> '' then
      if SslCtxUseCertificateChainFile(FCtx, FCertificateFile) <> 1 then
        if SslCtxUseCertificateFile(FCtx, FCertificateFile, SSL_FILETYPE_PEM) <> 1 then
          if SslCtxUseCertificateFile(FCtx, FCertificateFile, SSL_FILETYPE_ASN1) <> 1 then
            Exit;
    if FCertificate <> '' then
      if SslCtxUseCertificateASN1(FCtx, length(FCertificate), FCertificate) <> 1 then
        Exit;
    SSLCheck;
    if FPrivateKeyFile <> '' then
      if SslCtxUsePrivateKeyFile(FCtx, FPrivateKeyFile, SSL_FILETYPE_PEM) <> 1 then
        if SslCtxUsePrivateKeyFile(FCtx, FPrivateKeyFile, SSL_FILETYPE_ASN1) <> 1 then
          Exit;
    if FPrivateKey <> '' then
      if SslCtxUsePrivateKeyASN1(EVP_PKEY_RSA, FCtx, FPrivateKey, length(FPrivateKey)) <> 1 then
        Exit;
    SSLCheck;
    if FCertCAFile <> '' then
      if SslCtxLoadVerifyLocations(FCtx, FCertCAFile, '') <> 1 then
        Exit;
    if FPFXfile <> '' then
    begin
      try
        st := TFileStream.Create(FPFXfile, fmOpenRead	 or fmShareDenyNone);
        try
          s := ReadStrFromStream(st, st.Size);
        finally
          st.Free;
        end;
        if not LoadPFX(s) then
          Exit;
      except
        on Exception do
          Exit;
      end;
    end;
    if FPFX <> '' then
      if not LoadPFX(FPfx) then
        Exit;
    SSLCheck;
    Result := True;
  finally
    SSLCheck;
  end;
end;

function TSSLOpenSSL.Init(server:Boolean): Boolean;
var
  s: AnsiString;
begin
  Result := False;
  FLastErrorDesc := '';
  FLastError := 0;
  Fctx := nil;
  case FSSLType of
    LT_SSLv2:
      Fctx := SslCtxNew(SslMethodV2);
    LT_SSLv3:
      Fctx := SslCtxNew(SslMethodV3);
    LT_TLSv1:
      Fctx := SslCtxNew(SslMethodTLSV1);
    LT_all:
      Fctx := SslCtxNew(SslMethodV23);
  else
    Exit;
  end;
  if Fctx = nil then
  begin
    SSLCheck;
    Exit;
  end
  else
  begin
    s := FCiphers;
    SslCtxSetCipherList(Fctx, s);
    if FVerifyCert then
      SslCtxSetVerify(FCtx, SSL_VERIFY_PEER, nil)
    else
      SslCtxSetVerify(FCtx, SSL_VERIFY_NONE, nil);
{$IFNDEF CIL}
    SslCtxSetDefaultPasswdCb(FCtx, @PasswordCallback);
    SslCtxSetDefaultPasswdCbUserdata(FCtx, self);
{$ENDIF}

    if server and (FCertificateFile = '') and (FCertificate = '')
      and (FPFXfile = '') and (FPFX = '') then
    begin
      CreateSelfSignedcert(FSocket.ResolveIPToName(FSocket.GetRemoteSinIP));
    end;

    if not SetSSLKeys then
      Exit
    else
    begin
      Fssl := nil;
      Fssl := SslNew(Fctx);
      if Fssl = nil then
      begin
        SSLCheck;
        exit;
      end;
    end;
  end;
  Result := true;
end;

function TSSLOpenSSL.DeInit: Boolean;
begin
  Result := True;
  if assigned (Fssl) then
    sslfree(Fssl);
  Fssl := nil;
  if assigned (Fctx) then
  begin
    SslCtxFree(Fctx);
    Fctx := nil;
    ErrRemoveState(0);
  end;
  FSSLEnabled := False;
end;

function TSSLOpenSSL.Prepare(server:Boolean): Boolean;
begin
  Result := false;
  DeInit;
  if Init(server) then
    Result := true
  else
    DeInit;
end;

function TSSLOpenSSL.Connect: boolean;
var
  x: integer;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if Prepare(False) then
  begin
{$IFDEF CIL}
    if sslsetfd(FSsl, FSocket.Socket.Handle.ToInt32) < 1 then
{$ELSE}
    if sslsetfd(FSsl, FSocket.Socket) < 1 then
{$ENDIF}
    begin
      SSLCheck;
      Exit;
    end;
    if SNIHost<>'' then
      SSLCtrl(Fssl, SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name, PAnsiChar(AnsiString(SNIHost)));
    x := sslconnect(FSsl);
    if x < 1 then
    begin
      SSLcheck;
      Exit;
    end;
  if FverifyCert then
    if (GetVerifyCert <> 0) or (not DoVerifyCert) then
      Exit;
    FSSLEnabled := True;
    Result := True;
  end;
end;

function TSSLOpenSSL.Accept: boolean;
var
  x: integer;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if Prepare(True) then
  begin
{$IFDEF CIL}
    if sslsetfd(FSsl, FSocket.Socket.Handle.ToInt32) < 1 then
{$ELSE}
    if sslsetfd(FSsl, FSocket.Socket) < 1 then
{$ENDIF}
    begin
      SSLCheck;
      Exit;
    end;
    x := sslAccept(FSsl);
    if x < 1 then
    begin
      SSLcheck;
      Exit;
    end;
    FSSLEnabled := True;
    Result := True;
  end;
end;

function TSSLOpenSSL.Shutdown: boolean;
begin
  if assigned(FSsl) then
    sslshutdown(FSsl);
  DeInit;
  Result := True;
end;

function TSSLOpenSSL.BiShutdown: boolean;
var
  x: integer;
begin
  if assigned(FSsl) then
  begin
    x := sslshutdown(FSsl);
    if x = 0 then
    begin
      Synsock.Shutdown(FSocket.Socket, 1);
      sslshutdown(FSsl);
    end;
  end;
  DeInit;
  Result := True;
end;

function TSSLOpenSSL.SendBuffer(Buffer: TMemory; Len: Integer): Integer;
var
  err: integer;
{$IFDEF CIL}
  s: ansistring;
{$ENDIF}
begin
  FLastError := 0;
  FLastErrorDesc := '';
  repeat
{$IFDEF CIL}
    s := StringOf(Buffer);
    Result := SslWrite(FSsl, s, Len);
{$ELSE}
    Result := SslWrite(FSsl, Buffer , Len);
{$ENDIF}
    err := SslGetError(FSsl, Result);
  until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
  if err = SSL_ERROR_ZERO_RETURN then
    Result := 0
  else
    if (err <> 0) then
      FLastError := err;
end;

function TSSLOpenSSL.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
var
  err: integer;
{$IFDEF CIL}
  sb: stringbuilder;
  s: ansistring;
{$ENDIF}
begin
  FLastError := 0;
  FLastErrorDesc := '';
  repeat
{$IFDEF CIL}
    sb := StringBuilder.Create(Len);
    Result := SslRead(FSsl, sb, Len);
    if Result > 0 then
    begin
      sb.Length := Result;
      s := sb.ToString;
      System.Array.Copy(BytesOf(s), Buffer, length(s));
    end;
{$ELSE}
    Result := SslRead(FSsl, Buffer , Len);
{$ENDIF}
    err := SslGetError(FSsl, Result);
  until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
  if err = SSL_ERROR_ZERO_RETURN then
    Result := 0
  {pf}// Verze 1.1.0 byla s else tak jak to ted mam,
      // ve verzi 1.1.1 bylo ELSE zruseno, ale pak je SSL_ERROR_ZERO_RETURN
      // propagovano jako Chyba.
  {pf} else {/pf} if (err <> 0) then   
    FLastError := err;
end;

function TSSLOpenSSL.WaitingData: Integer;
begin
  Result := sslpending(Fssl);
end;

function TSSLOpenSSL.GetSSLVersion: string;
begin
  if not assigned(FSsl) then
    Result := ''
  else
    Result := SSlGetVersion(FSsl);
end;

function TSSLOpenSSL.GetPeerSubject: string;
var
  cert: PX509;
  s: ansistring;
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  if not assigned(cert) then
  begin
    Result := '';
    Exit;
  end;
{$IFDEF CIL}
  sb := StringBuilder.Create(4096);
  Result := X509NameOneline(X509GetSubjectName(cert), sb, 4096);
{$ELSE}
  setlength(s, 4096);
  Result := X509NameOneline(X509GetSubjectName(cert), s, Length(s));
{$ENDIF}
  X509Free(cert);
end;


function TSSLOpenSSL.GetPeerSerialNo: integer; {pf}
var
  cert: PX509;
  SN:   PASN1_INTEGER;
begin
  if not assigned(FSsl) then
  begin
    Result := -1;
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  try
    if not assigned(cert) then
    begin
      Result := -1;
      Exit;
    end;
    SN := X509GetSerialNumber(cert);
    Result := Asn1IntegerGet(SN);
  finally
    X509Free(cert);
  end;
end;

function TSSLOpenSSL.GetPeerName: string;
var
  s: ansistring;
begin
  s := GetPeerSubject;
  s := SeparateRight(s, '/CN=');
  Result := Trim(SeparateLeft(s, '/'));
end;

function TSSLOpenSSL.GetPeerNameHash: cardinal; {pf}
var
  cert: PX509;
begin
  if not assigned(FSsl) then
  begin
    Result := 0;
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  try
    if not assigned(cert) then
    begin
      Result := 0;
      Exit;
    end;
    Result := X509NameHash(X509GetSubjectName(cert));
  finally
    X509Free(cert);
  end;
end;

function TSSLOpenSSL.GetPeerIssuer: string;
var
  cert: PX509;
  s: ansistring;
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  if not assigned(cert) then
  begin
    Result := '';
    Exit;
  end;
{$IFDEF CIL}
  sb := StringBuilder.Create(4096);
  Result := X509NameOneline(X509GetIssuerName(cert), sb, 4096);
{$ELSE}
  setlength(s, 4096);
  Result := X509NameOneline(X509GetIssuerName(cert), s, Length(s));
{$ENDIF}
  X509Free(cert);
end;

function TSSLOpenSSL.GetPeerFingerprint: string;
var
  cert: PX509;
  x: integer;
{$IFDEF CIL}
  sb: StringBuilder;
{$ENDIF}
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  if not assigned(cert) then
  begin
    Result := '';
    Exit;
  end;
{$IFDEF CIL}
  sb := StringBuilder.Create(EVP_MAX_MD_SIZE);
  X509Digest(cert, EvpGetDigestByName('MD5'), sb, x);
  sb.Length := x;
  Result := sb.ToString;
{$ELSE}
  setlength(Result, EVP_MAX_MD_SIZE);
  X509Digest(cert, EvpGetDigestByName('MD5'), Result, x);
  SetLength(Result, x);
{$ENDIF}
  X509Free(cert);
end;

function TSSLOpenSSL.GetCertInfo: string;
var
  cert: PX509;
  x, y: integer;
  b: PBIO;
  s: AnsiString;
{$IFDEF CIL}
  sb: stringbuilder;
{$ENDIF}
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  if not assigned(cert) then
  begin
    Result := '';
    Exit;
  end;
  try {pf}
    b := BioNew(BioSMem);
    try
      X509Print(b, cert);
      x := bioctrlpending(b);
  {$IFDEF CIL}
      sb := StringBuilder.Create(x);
      y := bioread(b, sb, x);
      if y > 0 then
      begin
        sb.Length := y;
        s := sb.ToString;
      end;
  {$ELSE}
      setlength(s,x);
      y := bioread(b,s,x);
      if y > 0 then
        setlength(s, y);
  {$ENDIF}
      Result := ReplaceString(s, LF, CRLF);
    finally
      BioFreeAll(b);
    end;
  {pf}
  finally
    X509Free(cert);
  end;
  {/pf}
end;

function TSSLOpenSSL.GetCipherName: string;
begin
  if not assigned(FSsl) then
    Result := ''
  else
    Result := SslCipherGetName(SslGetCurrentCipher(FSsl));
end;

function TSSLOpenSSL.GetCipherBits: integer;
var
  x: integer;
begin
  if not assigned(FSsl) then
    Result := 0
  else
    Result := SSLCipherGetBits(SslGetCurrentCipher(FSsl), x);
end;

function TSSLOpenSSL.GetCipherAlgBits: integer;
begin
  if not assigned(FSsl) then
    Result := 0
  else
    SSLCipherGetBits(SslGetCurrentCipher(FSsl), Result);
end;

function TSSLOpenSSL.GetVerifyCert: integer;
begin
  if not assigned(FSsl) then
    Result := 1
  else
    Result := SslGetVerifyResult(FSsl);
end;

{==============================================================================}

initialization
  if InitSSLInterface then
    SSLImplementation := TSSLOpenSSL;

end.
