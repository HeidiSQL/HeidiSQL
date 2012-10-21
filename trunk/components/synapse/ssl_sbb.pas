{==============================================================================|
| Project : Ararat Synapse                                       | 001.000.003 |
|==============================================================================|
| Content: SSL support for SecureBlackBox                                     |
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
|   Allen Drennan (adrennan@wiredred.com)                                      |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(SSL plugin for Eldos SecureBlackBox)

For handling keys and certificates you can use this properties:
@link(TCustomSSL.CertCAFile), @link(TCustomSSL.CertCA),
@link(TCustomSSL.TrustCertificateFile), @link(TCustomSSL.TrustCertificate),
@link(TCustomSSL.PrivateKeyFile), @link(TCustomSSL.PrivateKey),
@link(TCustomSSL.CertificateFile), @link(TCustomSSL.Certificate),
@link(TCustomSSL.PFXFile). For usage of this properties and for possible formats
of keys and certificates refer to SecureBlackBox documentation.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

unit ssl_sbb;

interface

uses
  SysUtils, Classes, Windows, blcksock, synsock, synautil, synacode,
  SBClient, SBServer, SBX509, SBWinCertStorage, SBCustomCertStorage,
  SBUtils, SBConstants, SBSessionPool;

const
  DEFAULT_RECV_BUFFER=32768;

type
  {:@abstract(class implementing SecureBlackbox SSL plugin.)
   Instance of this class will be created for each @link(TTCPBlockSocket).
   You not need to create instance of this class, all is done by Synapse itself!}
  TSSLSBB=class(TCustomSSL)
  protected
    FServer: Boolean;
    FElSecureClient:TElSecureClient;
    FElSecureServer:TElSecureServer;
    FElCertStorage:TElMemoryCertStorage;
    FElX509Certificate:TElX509Certificate;
    FElX509CACertificate:TElX509Certificate;
    FCipherSuites:TBits;
  private
    FRecvBuffer:String;
    FRecvBuffers:String;
    FRecvBuffersLock:TRTLCriticalSection;
    FRecvDecodedBuffers:String;
    function GetCipherSuite:Integer;
    procedure Reset;
    function Prepare(Server:Boolean):Boolean;
    procedure OnError(Sender:TObject; ErrorCode:Integer; Fatal:Boolean; Remote:Boolean);
    procedure OnSend(Sender:TObject;Buffer:Pointer;Size:LongInt);
    procedure OnReceive(Sender:TObject;Buffer:Pointer;MaxSize:LongInt;var Written:LongInt);
    procedure OnData(Sender:TObject;Buffer:Pointer;Size:LongInt);
  public
    constructor Create(const Value: TTCPBlockSocket); override;
    destructor Destroy; override;
    {:See @inherited}
    function LibVersion: String; override;
    {:See @inherited}
    function LibName: String; override;
    {:See @inherited and @link(ssl_sbb) for more details.}
    function Connect: boolean; override;
    {:See @inherited and @link(ssl_sbb) for more details.}
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
    property ElSecureClient:TElSecureClient read FElSecureClient write FElSecureClient;
    property ElSecureServer:TElSecureServer read FElSecureServer write FElSecureServer;
    property CipherSuites:TBits read FCipherSuites write FCipherSuites;
    property CipherSuite:Integer read GetCipherSuite;
  end;

implementation

var
  FAcceptThread:THandle=0;

// on error
procedure TSSLSBB.OnError(Sender:TObject; ErrorCode:Integer; Fatal:Boolean; Remote:Boolean);

begin
  FLastErrorDesc:='';
  FLastError:=ErrorCode;
end;

// on send
procedure TSSLSBB.OnSend(Sender:TObject;Buffer:Pointer;Size:LongInt);

var
  lResult:Integer;

begin
  if FSocket.Socket=INVALID_SOCKET then
    Exit;
  lResult:=Send(FSocket.Socket,Buffer,Size,0);
  if lResult=SOCKET_ERROR then
    begin
      FLastErrorDesc:='';
      FLastError:=WSAGetLastError;
    end;
end;

// on receive
procedure TSSLSBB.OnReceive(Sender:TObject;Buffer:Pointer;MaxSize:LongInt;var Written:LongInt);

begin
  if GetCurrentThreadId<>FAcceptThread then EnterCriticalSection(FRecvBuffersLock);
  try
    if Length(FRecvBuffers)<=MaxSize then
      begin
        Written:=Length(FRecvBuffers);
        Move(FRecvBuffers[1],Buffer^,Written);
        FRecvBuffers:='';
      end
    else
      begin
        Written:=MaxSize;
        Move(FRecvBuffers[1],Buffer^,Written);
        Delete(FRecvBuffers,1,Written);
      end;
  finally
    if GetCurrentThreadId<>FAcceptThread then LeaveCriticalSection(FRecvBuffersLock);
  end;
end;

// on data
procedure TSSLSBB.OnData(Sender:TObject;Buffer:Pointer;Size:LongInt);

var
  lString:String;

begin
  SetLength(lString,Size);
  Move(Buffer^,lString[1],Size);
  FRecvDecodedBuffers:=FRecvDecodedBuffers+lString;
end;

{ inherited }

constructor TSSLSBB.Create(const Value: TTCPBlockSocket);

var
  loop1:Integer;

begin
  inherited Create(Value);
  FServer:=FALSE;
  FElSecureClient:=NIL;
  FElSecureServer:=NIL;
  FElCertStorage:=NIL;
  FElX509Certificate:=NIL;
  FElX509CACertificate:=NIL;
  SetLength(FRecvBuffer,DEFAULT_RECV_BUFFER);
  FRecvBuffers:='';
  InitializeCriticalSection(FRecvBuffersLock);
  FRecvDecodedBuffers:='';
  FCipherSuites:=TBits.Create;
  if FCipherSuites<>NIL then
    begin
      FCipherSuites.Size:=SB_SUITE_LAST+1;
      for loop1:=SB_SUITE_FIRST to SB_SUITE_LAST do
        FCipherSuites[loop1]:=TRUE;
    end;
end;

destructor TSSLSBB.Destroy;

begin
  Reset;
  inherited Destroy;
  if FCipherSuites<>NIL then
    FreeAndNIL(FCipherSuites);
  DeleteCriticalSection(FRecvBuffersLock);
end;

function TSSLSBB.LibVersion: String;

begin
  Result:='SecureBlackBox';
end;

function TSSLSBB.LibName: String;

begin
  Result:='ssl_sbb';
end;

function FileToString(lFile:String):String;

var
  lStream:TMemoryStream;

begin
  Result:='';
  lStream:=TMemoryStream.Create;
  if lStream<>NIL then
    begin
      lStream.LoadFromFile(lFile);
      if lStream.Size>0 then
        begin
          lStream.Position:=0;
          SetLength(Result,lStream.Size);
          Move(lStream.Memory^,Result[1],lStream.Size);
        end;
      lStream.Free;
    end;
end;

function TSSLSBB.GetCipherSuite:Integer;

begin
  if FServer then
    Result:=FElSecureServer.CipherSuite
  else
    Result:=FElSecureClient.CipherSuite;
end;

procedure TSSLSBB.Reset;

begin
  if FElSecureServer<>NIL then
    FreeAndNIL(FElSecureServer);
  if FElSecureClient<>NIL then
    FreeAndNIL(FElSecureClient);
  if FElX509Certificate<>NIL then
    FreeAndNIL(FElX509Certificate);
  if FElX509CACertificate<>NIL then
    FreeAndNIL(FElX509CACertificate);
  if FElCertStorage<>NIL then
    FreeAndNIL(FElCertStorage);
  FSSLEnabled:=FALSE;
end;

function TSSLSBB.Prepare(Server:Boolean): Boolean;

var
  loop1:Integer;
  lStream:TMemoryStream;
  lCertificate,lPrivateKey,lCertCA:String;

begin
  Result:=FALSE;
  FServer:=Server;

  // reset, if necessary
  Reset;

  // init, certificate
  if FCertificateFile<>'' then
    lCertificate:=FileToString(FCertificateFile)
  else
    lCertificate:=FCertificate;
  if FPrivateKeyFile<>'' then
    lPrivateKey:=FileToString(FPrivateKeyFile)
  else
    lPrivateKey:=FPrivateKey;
  if FCertCAFile<>'' then
    lCertCA:=FileToString(FCertCAFile)
  else
    lCertCA:=FCertCA;
  if (lCertificate<>'') and (lPrivateKey<>'') then
    begin
      FElCertStorage:=TElMemoryCertStorage.Create(NIL);
      if FElCertStorage<>NIL then
        FElCertStorage.Clear;

      // apply ca certificate
      if lCertCA<>'' then
        begin
          FElX509CACertificate:=TElX509Certificate.Create(NIL);
          if FElX509CACertificate<>NIL then
            begin
              with FElX509CACertificate do
                begin
                  lStream:=TMemoryStream.Create;
                  try
                    WriteStrToStream(lStream,lCertCA);
                    lStream.Seek(0,soFromBeginning);
                    LoadFromStream(lStream);
                  finally
                    lStream.Free;
                  end;
                end;
              if FElCertStorage<>NIL then
                FElCertStorage.Add(FElX509CACertificate);
            end;
        end;

      // apply certificate
      FElX509Certificate:=TElX509Certificate.Create(NIL);
      if FElX509Certificate<>NIL then
        begin
          with FElX509Certificate do
            begin
              lStream:=TMemoryStream.Create;
              try
                WriteStrToStream(lStream,lCertificate);
                lStream.Seek(0,soFromBeginning);
                LoadFromStream(lStream);
              finally
                lStream.Free;
              end;
              lStream:=TMemoryStream.Create;
              try
                WriteStrToStream(lStream,lPrivateKey);
                lStream.Seek(0,soFromBeginning);
                LoadKeyFromStream(lStream);
              finally
                lStream.Free;
              end;
              if FElCertStorage<>NIL then
                FElCertStorage.Add(FElX509Certificate);
            end;
        end;
    end;

  // init, as server
  if FServer then
    begin
      FElSecureServer:=TElSecureServer.Create(NIL);
      if FElSecureServer<>NIL then
        begin
          // init, ciphers
          for loop1:=SB_SUITE_FIRST to SB_SUITE_LAST do
            FElSecureServer.CipherSuites[loop1]:=FCipherSuites[loop1];
          FElSecureServer.Versions:=[sbSSL2,sbSSL3,sbTLS1];
          FElSecureServer.ClientAuthentication:=FALSE;
          FElSecureServer.OnError:=OnError;
          FElSecureServer.OnSend:=OnSend;
          FElSecureServer.OnReceive:=OnReceive;
          FElSecureServer.OnData:=OnData;
          FElSecureServer.CertStorage:=FElCertStorage;
          Result:=TRUE;
        end;
    end
  else
    // init, as client
    begin
      FElSecureClient:=TElSecureClient.Create(NIL);
      if FElSecureClient<>NIL then
        begin
          // init, ciphers
          for loop1:=SB_SUITE_FIRST to SB_SUITE_LAST do
            FElSecureClient.CipherSuites[loop1]:=FCipherSuites[loop1];
          FElSecureClient.Versions:=[sbSSL3,sbTLS1];
          FElSecureClient.OnError:=OnError;
          FElSecureClient.OnSend:=OnSend;
          FElSecureClient.OnReceive:=OnReceive;
          FElSecureClient.OnData:=OnData;
          FElSecureClient.CertStorage:=FElCertStorage;
          Result:=TRUE;
        end;
    end;
end;

function TSSLSBB.Connect:Boolean;

var
  lResult:Integer;

begin
  Result:=FALSE;
  if FSocket.Socket=INVALID_SOCKET then
    Exit;
  if Prepare(FALSE) then
    begin
      FElSecureClient.Open;

      // reset
      FRecvBuffers:='';
      FRecvDecodedBuffers:='';

      // wait for open or error
      while (not FElSecureClient.Active) and
        (FLastError=0) do
        begin
          // data available?
          if FRecvBuffers<>'' then
            FElSecureClient.DataAvailable
          else
            begin
              // socket recv
              lResult:=Recv(FSocket.Socket,@FRecvBuffer[1],Length(FRecvBuffer),0);
              if lResult=SOCKET_ERROR then
                begin
                  FLastErrorDesc:='';
                  FLastError:=WSAGetLastError;
                end
              else
                begin
                  if lResult>0 then
                    FRecvBuffers:=FRecvBuffers+Copy(FRecvBuffer,1,lResult)
                  else
                    Break;
                end;
            end;
        end;
      if FLastError<>0 then
        Exit;
      FSSLEnabled:=FElSecureClient.Active;
      Result:=FSSLEnabled;
    end;
end;

function TSSLSBB.Accept:Boolean;

var
  lResult:Integer;

begin
  Result:=FALSE;
  if FSocket.Socket=INVALID_SOCKET then
    Exit;
  if Prepare(TRUE) then
    begin
      FAcceptThread:=GetCurrentThreadId;
      FElSecureServer.Open;

      // reset
      FRecvBuffers:='';
      FRecvDecodedBuffers:='';

      // wait for open or error
      while (not FElSecureServer.Active) and
        (FLastError=0) do
        begin
          // data available?
          if FRecvBuffers<>'' then
            FElSecureServer.DataAvailable
          else
            begin
              // socket recv
              lResult:=Recv(FSocket.Socket,@FRecvBuffer[1],Length(FRecvBuffer),0);
              if lResult=SOCKET_ERROR then
                begin
                  FLastErrorDesc:='';
                  FLastError:=WSAGetLastError;
                end
              else
                begin
                  if lResult>0 then
                    FRecvBuffers:=FRecvBuffers+Copy(FRecvBuffer,1,lResult)
                  else
                    Break;
                end;
            end;
        end;
      if FLastError<>0 then
        Exit;
      FSSLEnabled:=FElSecureServer.Active;
      Result:=FSSLEnabled;
    end;
end;

function TSSLSBB.Shutdown:Boolean;

begin
  Result:=BiShutdown;
end;

function TSSLSBB.BiShutdown: boolean;

begin
  Reset;
  Result:=TRUE;
end;

function TSSLSBB.SendBuffer(Buffer: TMemory; Len: Integer): Integer;

begin
  if FServer then
    FElSecureServer.SendData(Buffer,Len)
  else
    FElSecureClient.SendData(Buffer,Len);
  Result:=Len;
end;

function TSSLSBB.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;

begin
  Result:=0;
  try
    // recv waiting, if necessary
    if FRecvDecodedBuffers='' then
      WaitingData;

    // received
    if Length(FRecvDecodedBuffers)<Len then
      begin
        Result:=Length(FRecvDecodedBuffers);
        Move(FRecvDecodedBuffers[1],Buffer^,Result);
        FRecvDecodedBuffers:='';
      end
    else
      begin
        Result:=Len;
        Move(FRecvDecodedBuffers[1],Buffer^,Result);
        Delete(FRecvDecodedBuffers,1,Result);
      end;
  except
    // ignore
  end;
end;

function TSSLSBB.WaitingData: Integer;

var
  lResult:Integer;
  lRecvBuffers:Boolean;

begin
  Result:=0;
  if FSocket.Socket=INVALID_SOCKET then
    Exit;
  // data available?
  if GetCurrentThreadId<>FAcceptThread then EnterCriticalSection(FRecvBuffersLock);
  try
    lRecvBuffers:=FRecvBuffers<>'';
  finally
    if GetCurrentThreadId<>FAcceptThread then LeaveCriticalSection(FRecvBuffersLock);
  end;
  if lRecvBuffers then
    begin
      if FServer then
        FElSecureServer.DataAvailable
      else
        FElSecureClient.DataAvailable;
    end
  else
    begin
      // socket recv
      lResult:=Recv(FSocket.Socket,@FRecvBuffer[1],Length(FRecvBuffer),0);
      if lResult=SOCKET_ERROR then
        begin
          FLastErrorDesc:='';
          FLastError:=WSAGetLastError;
        end
      else
        begin
          if GetCurrentThreadId<>FAcceptThread then EnterCriticalSection(FRecvBuffersLock);
          try
            FRecvBuffers:=FRecvBuffers+Copy(FRecvBuffer,1,lResult);
          finally
            if GetCurrentThreadId<>FAcceptThread then LeaveCriticalSection(FRecvBuffersLock);
          end;

          // data available?
          if GetCurrentThreadId<>FAcceptThread then EnterCriticalSection(FRecvBuffersLock);
          try
            lRecvBuffers:=FRecvBuffers<>'';
          finally
            if GetCurrentThreadId<>FAcceptThread then LeaveCriticalSection(FRecvBuffersLock);
          end;
          if lRecvBuffers then
            begin
              if FServer then
                FElSecureServer.DataAvailable
              else
                FElSecureClient.DataAvailable;
            end;
        end;
    end;

  // decoded buffers result
  Result:=Length(FRecvDecodedBuffers);
end;

function TSSLSBB.GetSSLVersion: string;

begin
  Result:='SSLv3 or TLSv1';
end;

function TSSLSBB.GetPeerSubject: string;

begin
  Result := '';
//  if FServer then
    // must return subject of the client certificate
//  else
    // must return subject of the server certificate
end;

function TSSLSBB.GetPeerName: string;

begin
  Result := '';
//  if FServer then
    // must return commonname of the client certificate
//  else
    // must return commonname of the server certificate
end;

function TSSLSBB.GetPeerIssuer: string;

begin
  Result := '';
//  if FServer then
    // must return issuer of the client certificate
//  else
    // must return issuer of the server certificate
end;

function TSSLSBB.GetPeerFingerprint: string;

begin
  Result := '';
//  if FServer then
    // must return a unique hash string of the client certificate
//  else
    // must return a unique hash string of the server certificate
end;

function TSSLSBB.GetCertInfo: string;

begin
  Result := '';
//  if FServer then
    // must return a text representation of the ASN of the client certificate
//  else
    // must return a text representation of the ASN of the server certificate
end;

{==============================================================================}

initialization
  SSLImplementation := TSSLSBB;

finalization

end.
