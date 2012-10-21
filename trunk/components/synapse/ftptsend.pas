{==============================================================================|
| Project : Ararat Synapse                                       | 001.001.001 |
|==============================================================================|
| Content: Trivial FTP (TFTP) client and server                                |
|==============================================================================|
| Copyright (c)1999-2010, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)2003-2010.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{: @abstract(TFTP client and server protocol)

Used RFC: RFC-1350
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit ftptsend;

interface

uses
  SysUtils, Classes,
  blcksock, synautil;

const
  cTFTPProtocol = '69';

  cTFTP_RRQ = word(1);
  cTFTP_WRQ = word(2);
  cTFTP_DTA = word(3);
  cTFTP_ACK = word(4);
  cTFTP_ERR = word(5);

type
  {:@abstract(Implementation of TFTP client and server)
   Note: Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}
  TTFTPSend = class(TSynaClient)
  private
    FSock: TUDPBlockSocket;
    FErrorCode: integer;
    FErrorString: string;
    FData: TMemoryStream;
    FRequestIP: string;
    FRequestPort: string;
    function SendPacket(Cmd: word; Serial: word; const Value: string): Boolean;
    function RecvPacket(Serial: word; var Value: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    {:Upload @link(data) as file to TFTP server.}
    function SendFile(const Filename: string): Boolean;

    {:Download file from TFTP server to @link(data).}
    function RecvFile(const Filename: string): Boolean;

    {:Acts as TFTP server and wait for client request. When some request
     incoming within Timeout, result is @true and parametres is filled with
     information from request. You must handle this request, validate it, and
     call @link(ReplyError), @link(ReplyRecv) or @link(ReplySend) for send reply
     to TFTP Client.}
    function WaitForRequest(var Req: word; var filename: string): Boolean;

    {:send error to TFTP client, when you acts as TFTP server.}
    procedure ReplyError(Error: word; Description: string);

    {:Accept uploaded file from TFTP client to @link(data), when you acts as
     TFTP server.}
    function ReplyRecv: Boolean;

    {:Accept download request file from TFTP client and send content of
     @link(data), when you acts as TFTP server.}
    function ReplySend: Boolean;
  published
    {:Code of TFTP error.}
    property ErrorCode: integer read FErrorCode;

    {:Human readable decription of TFTP error. (if is sended by remote side)}
    property ErrorString: string read FErrorString;

    {:MemoryStream with datas for sending or receiving}
    property Data: TMemoryStream read FData;

    {:Address of TFTP remote side.}
    property RequestIP: string read FRequestIP write FRequestIP;

    {:Port of TFTP remote side.}
    property RequestPort: string read FRequestPort write FRequestPort;
  end;

implementation

constructor TTFTPSend.Create;
begin
  inherited Create;
  FSock := TUDPBlockSocket.Create;
  FSock.Owner := self;
  FTargetPort := cTFTPProtocol;
  FData := TMemoryStream.Create;
  FErrorCode := 0;
  FErrorString := '';
end;

destructor TTFTPSend.Destroy;
begin
  FSock.Free;
  FData.Free;
  inherited Destroy;
end;

function TTFTPSend.SendPacket(Cmd: word; Serial: word; const Value: string): Boolean;
var
  s, sh: string;
begin
  FErrorCode := 0;
  FErrorString := '';
  Result := false;
  if Cmd <> 2 then
    s := CodeInt(Cmd) + CodeInt(Serial) + Value
  else
    s := CodeInt(Cmd) + Value;
  FSock.SendString(s);
  s := FSock.RecvPacket(FTimeout);
  if FSock.LastError = 0 then
    if length(s) >= 4 then
    begin
      sh := CodeInt(4) + CodeInt(Serial);
      if Pos(sh, s) = 1 then
        Result := True
      else
        if s[1] = #5 then
        begin
          FErrorCode := DecodeInt(s, 3);
          Delete(s, 1, 4);
          FErrorString := SeparateLeft(s, #0);
        end;
    end;
end;

function TTFTPSend.RecvPacket(Serial: word; var Value: string): Boolean;
var
  s: string;
  ser: word;
begin
  FErrorCode := 0;
  FErrorString := '';
  Result := False;
  Value := '';
  s := FSock.RecvPacket(FTimeout);
  if FSock.LastError = 0 then
    if length(s) >= 4 then
      if DecodeInt(s, 1) = 3 then
      begin
        ser := DecodeInt(s, 3);
        if ser = Serial then
        begin
          Delete(s, 1, 4);
          Value := s;
          S := CodeInt(4) + CodeInt(ser);
          FSock.SendString(s);
          Result := FSock.LastError = 0;
        end
        else
        begin
          S := CodeInt(5) + CodeInt(5) + 'Unexcepted serial#' + #0;
          FSock.SendString(s);
        end;
      end;
      if DecodeInt(s, 1) = 5 then
      begin
        FErrorCode := DecodeInt(s, 3);
        Delete(s, 1, 4);
        FErrorString := SeparateLeft(s, #0);
      end;
end;

function TTFTPSend.SendFile(const Filename: string): Boolean;
var
  s: string;
  ser: word;
  n, n1, n2: integer;
begin
  Result := False;
  FErrorCode := 0;
  FErrorString := '';
  FSock.CloseSocket;
  FSock.Connect(FTargetHost, FTargetPort);
  try
    if FSock.LastError = 0 then
    begin
      s := Filename + #0 + 'octet' + #0;
      if not Sendpacket(2, 0, s) then
        Exit;
      ser := 1;
      FData.Position := 0;
      n1 := FData.Size div 512;
      n2 := FData.Size mod 512;
      for n := 1 to n1 do
      begin
        s := ReadStrFromStream(FData, 512);
//        SetLength(s, 512);
//        FData.Read(pointer(s)^, 512);
        if not Sendpacket(3, ser, s) then
          Exit;
        inc(ser);
      end;
      s := ReadStrFromStream(FData, n2);
//      SetLength(s, n2);
//      FData.Read(pointer(s)^, n2);
      if not Sendpacket(3, ser, s) then
        Exit;
      Result := True;
    end;
  finally
    FSock.CloseSocket;
  end;
end;

function TTFTPSend.RecvFile(const Filename: string): Boolean;
var
  s: string;
  ser: word;
begin
  Result := False;
  FErrorCode := 0;
  FErrorString := '';
  FSock.CloseSocket;
  FSock.Connect(FTargetHost, FTargetPort);
  try
    if FSock.LastError = 0 then
    begin
      s := CodeInt(1) + Filename + #0 + 'octet' + #0;
      FSock.SendString(s);
      if FSock.LastError <> 0 then
        Exit;
      FData.Clear;
      ser := 1;
      repeat
        if not RecvPacket(ser, s) then
          Exit;
        inc(ser);
        WriteStrToStream(FData, s);
//        FData.Write(pointer(s)^, length(s));
      until length(s) <> 512;
      FData.Position := 0;
      Result := true;
    end;
  finally
    FSock.CloseSocket;
  end;
end;

function TTFTPSend.WaitForRequest(var Req: word; var filename: string): Boolean;
var
  s: string;
begin
  Result := False;
  FErrorCode := 0;
  FErrorString := '';
  FSock.CloseSocket;
  FSock.Bind('0.0.0.0', FTargetPort);
  if FSock.LastError = 0 then
  begin
    s := FSock.RecvPacket(FTimeout);
    if FSock.LastError = 0 then
      if Length(s) >= 4 then
      begin
        FRequestIP := FSock.GetRemoteSinIP;
        FRequestPort := IntToStr(FSock.GetRemoteSinPort);
        Req := DecodeInt(s, 1);
        delete(s, 1, 2);
        filename := Trim(SeparateLeft(s, #0));
        s := SeparateRight(s, #0);
        s := SeparateLeft(s, #0);
        Result := lowercase(trim(s)) = 'octet';
      end;
  end;
end;

procedure TTFTPSend.ReplyError(Error: word; Description: string);
var
  s: string;
begin
  FSock.CloseSocket;
  FSock.Connect(FRequestIP, FRequestPort);
  s := CodeInt(5) + CodeInt(Error) + Description + #0;
  FSock.SendString(s);
  FSock.CloseSocket;
end;

function TTFTPSend.ReplyRecv: Boolean;
var
  s: string;
  ser: integer;
begin
  Result := False;
  FErrorCode := 0;
  FErrorString := '';
  FSock.CloseSocket;
  FSock.Connect(FRequestIP, FRequestPort);
  try
    s := CodeInt(4) + CodeInt(0);
    FSock.SendString(s);
    FData.Clear;
    ser := 1;
    repeat
      if not RecvPacket(ser, s) then
        Exit;
      inc(ser);
      WriteStrToStream(FData, s);
//      FData.Write(pointer(s)^, length(s));
    until length(s) <> 512;
    FData.Position := 0;
    Result := true;
  finally
    FSock.CloseSocket;
  end;
end;

function TTFTPSend.ReplySend: Boolean;
var
  s: string;
  ser: word;
  n, n1, n2: integer;
begin
  Result := False;
  FErrorCode := 0;
  FErrorString := '';
  FSock.CloseSocket;
  FSock.Connect(FRequestIP, FRequestPort);
  try
    ser := 1;
    FData.Position := 0;
    n1 := FData.Size div 512;
    n2 := FData.Size mod 512;
    for n := 1 to n1 do
    begin
      s := ReadStrFromStream(FData, 512);
//      SetLength(s, 512);
//      FData.Read(pointer(s)^, 512);
      if not Sendpacket(3, ser, s) then
        Exit;
      inc(ser);
    end;
    s := ReadStrFromStream(FData, n2);
//    SetLength(s, n2);
//    FData.Read(pointer(s)^, n2);
    if not Sendpacket(3, ser, s) then
      Exit;
    Result := True;
  finally
    FSock.CloseSocket;
  end;
end;

{==============================================================================}

end.
