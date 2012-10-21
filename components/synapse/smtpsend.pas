{==============================================================================|
| Project : Ararat Synapse                                       | 003.005.001 |
|==============================================================================|
| Content: SMTP client                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c) 1999-2010.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(SMTP client)

Used RFC: RFC-1869, RFC-1870, RFC-1893, RFC-2034, RFC-2104, RFC-2195, RFC-2487,
 RFC-2554, RFC-2821
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit smtpsend;

interface

uses
  SysUtils, Classes,
  blcksock, synautil, synacode;

const
  cSmtpProtocol = '25';

type
  {:@abstract(Implementation of SMTP and ESMTP procotol),
   include some ESMTP extensions, include SSL/TLS too.

   Note: Are you missing properties for setting Username and Password for ESMTP?
   Look to parent @link(TSynaClient) object!

   Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}
  TSMTPSend = class(TSynaClient)
  private
    FSock: TTCPBlockSocket;
    FResultCode: Integer;
    FResultString: string;
    FFullResult: TStringList;
    FESMTPcap: TStringList;
    FESMTP: Boolean;
    FAuthDone: Boolean;
    FESMTPSize: Boolean;
    FMaxSize: Integer;
    FEnhCode1: Integer;
    FEnhCode2: Integer;
    FEnhCode3: Integer;
    FSystemName: string;
    FAutoTLS: Boolean;
    FFullSSL: Boolean;
    procedure EnhancedCode(const Value: string);
    function ReadResult: Integer;
    function AuthLogin: Boolean;
    function AuthCram: Boolean;
    function AuthPlain: Boolean;
    function Helo: Boolean;
    function Ehlo: Boolean;
    function Connect: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    {:Connects to SMTP server (defined in @link(TSynaClient.TargetHost)) and
     begin SMTP session. (First try ESMTP EHLO, next old HELO handshake). Parses
     ESMTP capabilites and if you specified Username and password and remote
     server can handle AUTH command, try login by AUTH command. Preffered login
     method is CRAM-MD5 (if safer!). If all OK, result is @true, else result is
     @false.}
    function Login: Boolean;

    {:Close SMTP session (QUIT command) and disconnect from SMTP server.}
    function Logout: Boolean;

    {:Send RSET SMTP command for reset SMTP session. If all OK, result is @true,
     else result is @false.}
    function Reset: Boolean;

    {:Send NOOP SMTP command for keep SMTP session. If all OK, result is @true,
     else result is @false.}
    function NoOp: Boolean;

    {:Send MAIL FROM SMTP command for set sender e-mail address. If sender's
     e-mail address is empty string, transmited message is error message.

     If size not 0 and remote server can handle SIZE parameter, append SIZE
     parameter to request. If all OK, result is @true, else result is @false.}
    function MailFrom(const Value: string; Size: Integer): Boolean;

    {:Send RCPT TO SMTP command for set receiver e-mail address. It cannot be an
     empty string. If all OK, result is @true, else result is @false.}
    function MailTo(const Value: string): Boolean;

    {:Send DATA SMTP command and transmit message data. If all OK, result is
     @true, else result is @false.}
    function MailData(const Value: Tstrings): Boolean;

    {:Send ETRN SMTP command for start sending of remote queue for domain in
     Value. If all OK, result is @true, else result is @false.}
    function Etrn(const Value: string): Boolean;

    {:Send VRFY SMTP command for check receiver e-mail address. It cannot be
     an empty string. If all OK, result is @true, else result is @false.}
    function Verify(const Value: string): Boolean;

    {:Call STARTTLS command for upgrade connection to SSL/TLS mode.}
    function StartTLS: Boolean;

    {:Return string descriptive text for enhanced result codes stored in
     @link(EnhCode1), @link(EnhCode2) and @link(EnhCode3).}
    function EnhCodeString: string;

    {:Try to find specified capability in ESMTP response.}
    function FindCap(const Value: string): string;
  published
    {:result code of last SMTP command.}
    property ResultCode: Integer read FResultCode;

    {:result string of last SMTP command (begin with string representation of
     result code).}
    property ResultString: string read FResultString;

    {:All result strings of last SMTP command (result is maybe multiline!).}
    property FullResult: TStringList read FFullResult;

    {:List of ESMTP capabilites of remote ESMTP server. (If you connect to ESMTP
     server only!).}
    property ESMTPcap: TStringList read FESMTPcap;

    {:@TRUE if you successfuly logged to ESMTP server.}
    property ESMTP: Boolean read FESMTP;

    {:@TRUE if you successfuly pass authorisation to remote server.}
    property AuthDone: Boolean read FAuthDone;

    {:@TRUE if remote server can handle SIZE parameter.}
    property ESMTPSize: Boolean read FESMTPSize;

    {:When @link(ESMTPsize) is @TRUE, contains max length of message that remote
     server can handle.}
    property MaxSize: Integer read FMaxSize;

    {:First digit of Enhanced result code. If last operation does not have
     enhanced result code, values is 0.}
    property EnhCode1: Integer read FEnhCode1;

    {:Second digit of Enhanced result code. If last operation does not have
     enhanced result code, values is 0.}
    property EnhCode2: Integer read FEnhCode2;

    {:Third digit of Enhanced result code. If last operation does not have
     enhanced result code, values is 0.}
    property EnhCode3: Integer read FEnhCode3;

    {:name of our system used in HELO and EHLO command. Implicit value is
     internet address of your machine.}
    property SystemName: string read FSystemName Write FSystemName;

    {:If is set to true, then upgrade to SSL/TLS mode if remote server support it.}
    property AutoTLS: Boolean read FAutoTLS Write FAutoTLS;

    {:SSL/TLS mode is used from first contact to server. Servers with full
     SSL/TLS mode usualy using non-standard TCP port!}
    property FullSSL: Boolean read FFullSSL Write FFullSSL;

    {:Socket object used for TCP/IP operation. Good for seting OnStatus hook, etc.}
    property Sock: TTCPBlockSocket read FSock;
  end;

{:A very useful function and example of its use would be found in the TSMTPsend
 object. Send maildata (text of e-mail with all SMTP headers! For example when
 text of message is created by @link(TMimemess) object) from "MailFrom" e-mail
 address to "MailTo" e-mail address (If you need more then one receiver, then
 separate their addresses by comma).

 Function sends e-mail to a SMTP server defined in "SMTPhost" parameter.
 Username and password are used for authorization to the "SMTPhost". If you
 don't want authorization, set "Username" and "Password" to empty strings. If
 e-mail message is successfully sent, the result returns @true.

 If you need use different port number then standard, then add this port number
 to SMTPhost after colon. (i.e. '127.0.0.1:1025')}
function SendToRaw(const MailFrom, MailTo, SMTPHost: string;
  const MailData: TStrings; const Username, Password: string): Boolean;

{:A very useful function and example of its use would be found in the TSMTPsend
 object. Send "Maildata" (text of e-mail without any SMTP headers!) from
 "MailFrom" e-mail address to "MailTo" e-mail address with "Subject".  (If you
 need more then one receiver, then separate their addresses by comma).

 This function constructs all needed SMTP headers (with DATE header) and sends
 the e-mail to the SMTP server defined in the "SMTPhost" parameter. If the
 e-mail message is successfully sent, the result will be @TRUE.

 If you need use different port number then standard, then add this port number
 to SMTPhost after colon. (i.e. '127.0.0.1:1025')}
function SendTo(const MailFrom, MailTo, Subject, SMTPHost: string;
  const MailData: TStrings): Boolean;

{:A very useful function and example of its use would be found in the TSMTPsend
 object. Sends "MailData" (text of e-mail without any SMTP headers!) from
 "MailFrom" e-mail address to "MailTo" e-mail address (If you need more then one
 receiver, then separate their addresses by comma).

 This function sends the e-mail to the SMTP server defined in the "SMTPhost"
 parameter. Username and password are used for authorization to the "SMTPhost".
 If you dont want authorization, set "Username" and "Password" to empty Strings.
 If the e-mail message is successfully sent, the result will be @TRUE.

 If you need use different port number then standard, then add this port number
 to SMTPhost after colon. (i.e. '127.0.0.1:1025')}
function SendToEx(const MailFrom, MailTo, Subject, SMTPHost: string;
  const MailData: TStrings; const Username, Password: string): Boolean;

implementation

constructor TSMTPSend.Create;
begin
  inherited Create;
  FFullResult := TStringList.Create;
  FESMTPcap := TStringList.Create;
  FSock := TTCPBlockSocket.Create;
  FSock.Owner := self;
  FSock.ConvertLineEnd := true;
  FTimeout := 60000;
  FTargetPort := cSmtpProtocol;
  FSystemName := FSock.LocalName;
  FAutoTLS := False;
  FFullSSL := False;
end;

destructor TSMTPSend.Destroy;
begin
  FSock.Free;
  FESMTPcap.Free;
  FFullResult.Free;
  inherited Destroy;
end;

procedure TSMTPSend.EnhancedCode(const Value: string);
var
  s, t: string;
  e1, e2, e3: Integer;
begin
  FEnhCode1 := 0;
  FEnhCode2 := 0;
  FEnhCode3 := 0;
  s := Copy(Value, 5, Length(Value) - 4);
  t := Trim(SeparateLeft(s, '.'));
  s := Trim(SeparateRight(s, '.'));
  if t = '' then
    Exit;
  if Length(t) > 1 then
    Exit;
  e1 := StrToIntDef(t, 0);
  if e1 = 0 then
    Exit;
  t := Trim(SeparateLeft(s, '.'));
  s := Trim(SeparateRight(s, '.'));
  if t = '' then
    Exit;
  if Length(t) > 3 then
    Exit;
  e2 := StrToIntDef(t, 0);
  t := Trim(SeparateLeft(s, ' '));
  if t = '' then
    Exit;
  if Length(t) > 3 then
    Exit;
  e3 := StrToIntDef(t, 0);
  FEnhCode1 := e1;
  FEnhCode2 := e2;
  FEnhCode3 := e3;
end;

function TSMTPSend.ReadResult: Integer;
var
  s: String;
begin
  Result := 0;
  FFullResult.Clear;
  repeat
    s := FSock.RecvString(FTimeout);
    FResultString := s;
    FFullResult.Add(s);
    if FSock.LastError <> 0 then
      Break;
  until Pos('-', s) <> 4;
  s := FFullResult[0];
  if Length(s) >= 3 then
    Result := StrToIntDef(Copy(s, 1, 3), 0);
  FResultCode := Result;
  EnhancedCode(s);
end;

function TSMTPSend.AuthLogin: Boolean;
begin
  Result := False;
  FSock.SendString('AUTH LOGIN' + CRLF);
  if ReadResult <> 334 then
    Exit;
  FSock.SendString(EncodeBase64(FUsername) + CRLF);
  if ReadResult <> 334 then
    Exit;
  FSock.SendString(EncodeBase64(FPassword) + CRLF);
  Result := ReadResult = 235;
end;

function TSMTPSend.AuthCram: Boolean;
var
  s: ansistring;
begin
  Result := False;
  FSock.SendString('AUTH CRAM-MD5' + CRLF);
  if ReadResult <> 334 then
    Exit;
  s := Copy(FResultString, 5, Length(FResultString) - 4);
  s := DecodeBase64(s);
  s := HMAC_MD5(s, FPassword);
  s := FUsername + ' ' + StrToHex(s);
  FSock.SendString(EncodeBase64(s) + CRLF);
  Result := ReadResult = 235;
end;

function TSMTPSend.AuthPlain: Boolean;
var
  s: ansistring;
begin
  s := ansichar(0) + FUsername + ansichar(0) + FPassword;
  FSock.SendString('AUTH PLAIN ' + EncodeBase64(s) + CRLF);
  Result := ReadResult = 235;
end;

function TSMTPSend.Connect: Boolean;
begin
  FSock.CloseSocket;
  FSock.Bind(FIPInterface, cAnyPort);
  if FSock.LastError = 0 then
    FSock.Connect(FTargetHost, FTargetPort);
  if FSock.LastError = 0 then
    if FFullSSL then
      FSock.SSLDoConnect;
  Result := FSock.LastError = 0;
end;

function TSMTPSend.Helo: Boolean;
var
  x: Integer;
begin
  FSock.SendString('HELO ' + FSystemName + CRLF);
  x := ReadResult;
  Result := (x >= 250) and (x <= 259);
end;

function TSMTPSend.Ehlo: Boolean;
var
  x: Integer;
begin
  FSock.SendString('EHLO ' + FSystemName + CRLF);
  x := ReadResult;
  Result := (x >= 250) and (x <= 259);
end;

function TSMTPSend.Login: Boolean;
var
  n: Integer;
  auths: string;
  s: string;
begin
  Result := False;
  FESMTP := True;
  FAuthDone := False;
  FESMTPcap.clear;
  FESMTPSize := False;
  FMaxSize := 0;
  if not Connect then
    Exit;
  if ReadResult <> 220 then
    Exit;
  if not Ehlo then
  begin
    FESMTP := False;
    if not Helo then
      Exit;
  end;
  Result := True;
  if FESMTP then
  begin
    for n := 1 to FFullResult.Count - 1 do
      FESMTPcap.Add(Copy(FFullResult[n], 5, Length(FFullResult[n]) - 4));
    if (not FullSSL) and FAutoTLS and (FindCap('STARTTLS') <> '') then
      if StartTLS then
      begin
        Ehlo;
        FESMTPcap.Clear;
        for n := 1 to FFullResult.Count - 1 do
          FESMTPcap.Add(Copy(FFullResult[n], 5, Length(FFullResult[n]) - 4));
      end
      else
      begin
        Result := False;
        Exit;
      end;
    if not ((FUsername = '') and (FPassword = '')) then
    begin
      s := FindCap('AUTH ');
      if s = '' then
        s := FindCap('AUTH=');
      auths := UpperCase(s);
      if s <> '' then
      begin
        if Pos('CRAM-MD5', auths) > 0 then
          FAuthDone := AuthCram;
        if (not FauthDone) and (Pos('PLAIN', auths) > 0) then
          FAuthDone := AuthPlain;
        if (not FauthDone) and (Pos('LOGIN', auths) > 0) then
          FAuthDone := AuthLogin;
      end;
    end;
    s := FindCap('SIZE');
    if s <> '' then
    begin
      FESMTPsize := True;
      FMaxSize := StrToIntDef(Copy(s, 6, Length(s) - 5), 0);
    end;
  end;
end;

function TSMTPSend.Logout: Boolean;
begin
  FSock.SendString('QUIT' + CRLF);
  Result := ReadResult = 221;
  FSock.CloseSocket;
end;

function TSMTPSend.Reset: Boolean;
begin
  FSock.SendString('RSET' + CRLF);
  Result := ReadResult div 100 = 2;
end;

function TSMTPSend.NoOp: Boolean;
begin
  FSock.SendString('NOOP' + CRLF);
  Result := ReadResult div 100 = 2;
end;

function TSMTPSend.MailFrom(const Value: string; Size: Integer): Boolean;
var
  s: string;
begin
  s := 'MAIL FROM:<' + Value + '>';
  if FESMTPsize and (Size > 0) then
    s := s + ' SIZE=' + IntToStr(Size);
  FSock.SendString(s + CRLF);
  Result := ReadResult div 100 = 2;
end;

function TSMTPSend.MailTo(const Value: string): Boolean;
begin
  FSock.SendString('RCPT TO:<' + Value + '>' + CRLF);
  Result := ReadResult div 100 = 2;
end;

function TSMTPSend.MailData(const Value: TStrings): Boolean;
var
  n: Integer;
  s: string;
  t: string;
  x: integer;
begin
  Result := False;
  FSock.SendString('DATA' + CRLF);
  if ReadResult <> 354 then
    Exit;
  t := '';
  x := 1500;
  for n := 0 to Value.Count - 1 do
  begin
    s := Value[n];
    if Length(s) >= 1 then
      if s[1] = '.' then
        s := '.' + s;
    if Length(t) + Length(s) >= x then
    begin
      FSock.SendString(t);
      t := '';
    end;
    t := t + s + CRLF;
  end;
  if t <> '' then
    FSock.SendString(t);
  FSock.SendString('.' + CRLF);
  Result := ReadResult div 100 = 2;
end;

function TSMTPSend.Etrn(const Value: string): Boolean;
var
  x: Integer;
begin
  FSock.SendString('ETRN ' + Value + CRLF);
  x := ReadResult;
  Result := (x >= 250) and (x <= 259);
end;

function TSMTPSend.Verify(const Value: string): Boolean;
var
  x: Integer;
begin
  FSock.SendString('VRFY ' + Value + CRLF);
  x := ReadResult;
  Result := (x >= 250) and (x <= 259);
end;

function TSMTPSend.StartTLS: Boolean;
begin
  Result := False;
  if FindCap('STARTTLS') <> '' then
  begin
    FSock.SendString('STARTTLS' + CRLF);
    if (ReadResult = 220) and (FSock.LastError = 0) then
    begin
      Fsock.SSLDoConnect;
      Result := FSock.LastError = 0;
    end;
  end;
end;

function TSMTPSend.EnhCodeString: string;
var
  s, t: string;
begin
  s := IntToStr(FEnhCode2) + '.' + IntToStr(FEnhCode3);
  t := '';
  if s = '0.0' then t := 'Other undefined Status';
  if s = '1.0' then t := 'Other address status';
  if s = '1.1' then t := 'Bad destination mailbox address';
  if s = '1.2' then t := 'Bad destination system address';
  if s = '1.3' then t := 'Bad destination mailbox address syntax';
  if s = '1.4' then t := 'Destination mailbox address ambiguous';
  if s = '1.5' then t := 'Destination mailbox address valid';
  if s = '1.6' then t := 'Mailbox has moved';
  if s = '1.7' then t := 'Bad sender''s mailbox address syntax';
  if s = '1.8' then t := 'Bad sender''s system address';
  if s = '2.0' then t := 'Other or undefined mailbox status';
  if s = '2.1' then t := 'Mailbox disabled, not accepting messages';
  if s = '2.2' then t := 'Mailbox full';
  if s = '2.3' then t := 'Message Length exceeds administrative limit';
  if s = '2.4' then t := 'Mailing list expansion problem';
  if s = '3.0' then t := 'Other or undefined mail system status';
  if s = '3.1' then t := 'Mail system full';
  if s = '3.2' then t := 'System not accepting network messages';
  if s = '3.3' then t := 'System not capable of selected features';
  if s = '3.4' then t := 'Message too big for system';
  if s = '3.5' then t := 'System incorrectly configured';
  if s = '4.0' then t := 'Other or undefined network or routing status';
  if s = '4.1' then t := 'No answer from host';
  if s = '4.2' then t := 'Bad connection';
  if s = '4.3' then t := 'Routing server failure';
  if s = '4.4' then t := 'Unable to route';
  if s = '4.5' then t := 'Network congestion';
  if s = '4.6' then t := 'Routing loop detected';
  if s = '4.7' then t := 'Delivery time expired';
  if s = '5.0' then t := 'Other or undefined protocol status';
  if s = '5.1' then t := 'Invalid command';
  if s = '5.2' then t := 'Syntax error';
  if s = '5.3' then t := 'Too many recipients';
  if s = '5.4' then t := 'Invalid command arguments';
  if s = '5.5' then t := 'Wrong protocol version';
  if s = '6.0' then t := 'Other or undefined media error';
  if s = '6.1' then t := 'Media not supported';
  if s = '6.2' then t := 'Conversion required and prohibited';
  if s = '6.3' then t := 'Conversion required but not supported';
  if s = '6.4' then t := 'Conversion with loss performed';
  if s = '6.5' then t := 'Conversion failed';
  if s = '7.0' then t := 'Other or undefined security status';
  if s = '7.1' then t := 'Delivery not authorized, message refused';
  if s = '7.2' then t := 'Mailing list expansion prohibited';
  if s = '7.3' then t := 'Security conversion required but not possible';
  if s = '7.4' then t := 'Security features not supported';
  if s = '7.5' then t := 'Cryptographic failure';
  if s = '7.6' then t := 'Cryptographic algorithm not supported';
  if s = '7.7' then t := 'Message integrity failure';
  s := '???-';
  if FEnhCode1 = 2 then s := 'Success-';
  if FEnhCode1 = 4 then s := 'Persistent Transient Failure-';
  if FEnhCode1 = 5 then s := 'Permanent Failure-';
  Result := s + t;
end;

function TSMTPSend.FindCap(const Value: string): string;
var
  n: Integer;
  s: string;
begin
  s := UpperCase(Value);
  Result := '';
  for n := 0 to FESMTPcap.Count - 1 do
    if Pos(s, UpperCase(FESMTPcap[n])) = 1 then
    begin
      Result := FESMTPcap[n];
      Break;
    end;
end;

{==============================================================================}

function SendToRaw(const MailFrom, MailTo, SMTPHost: string;
  const MailData: TStrings; const Username, Password: string): Boolean;
var
  SMTP: TSMTPSend;
  s, t: string;
begin
  Result := False;
  SMTP := TSMTPSend.Create;
  try
// if you need SOCKS5 support, uncomment next lines:
    // SMTP.Sock.SocksIP := '127.0.0.1';
    // SMTP.Sock.SocksPort := '1080';
// if you need support for upgrade session to TSL/SSL, uncomment next lines:
    // SMTP.AutoTLS := True;
// if you need support for TSL/SSL tunnel, uncomment next lines:
    // SMTP.FullSSL := True;
    SMTP.TargetHost := Trim(SeparateLeft(SMTPHost, ':'));
    s := Trim(SeparateRight(SMTPHost, ':'));
    if (s <> '') and (s <> SMTPHost) then
      SMTP.TargetPort := s;
    SMTP.Username := Username;
    SMTP.Password := Password;
    if SMTP.Login then
    begin
      if SMTP.MailFrom(GetEmailAddr(MailFrom), Length(MailData.Text)) then
      begin
        s := MailTo;
        repeat
          t := GetEmailAddr(Trim(FetchEx(s, ',', '"')));
          if t <> '' then
            Result := SMTP.MailTo(t);
          if not Result then
            Break;
        until s = '';
        if Result then
          Result := SMTP.MailData(MailData);
      end;
      SMTP.Logout;
    end;
  finally
    SMTP.Free;
  end;
end;

function SendToEx(const MailFrom, MailTo, Subject, SMTPHost: string;
  const MailData: TStrings; const Username, Password: string): Boolean;
var
  t: TStrings;
begin
  t := TStringList.Create;
  try
    t.Assign(MailData);
    t.Insert(0, '');
    t.Insert(0, 'X-mailer: Synapse - Delphi & Kylix TCP/IP library by Lukas Gebauer');
    t.Insert(0, 'Subject: ' + Subject);
    t.Insert(0, 'Date: ' + Rfc822DateTime(now));
    t.Insert(0, 'To: ' + MailTo);
    t.Insert(0, 'From: ' + MailFrom);
    Result := SendToRaw(MailFrom, MailTo, SMTPHost, t, Username, Password);
  finally
    t.Free;
  end;
end;

function SendTo(const MailFrom, MailTo, Subject, SMTPHost: string;
  const MailData: TStrings): Boolean;
begin
  Result := SendToEx(MailFrom, MailTo, Subject, SMTPHost, MailData, '', '');
end;

end.
