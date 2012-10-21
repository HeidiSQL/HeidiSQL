{==============================================================================|
| Project : Ararat Synapse                                       | 001.005.003 |
|==============================================================================|
| Content: NNTP client                                                         |
|==============================================================================|
| Copyright (c)1999-2011, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c) 1999-2011.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(NNTP client)
NNTP (network news transfer protocol)

Used RFC: RFC-977, RFC-2980
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
  {$WARN SUSPICIOUS_TYPECAST OFF}
{$ENDIF}

unit nntpsend;

interface

uses
  SysUtils, Classes,
  blcksock, synautil;

const
  cNNTPProtocol = '119';

type

  {:abstract(Implementation of Network News Transfer Protocol.

   Note: Are you missing properties for setting Username and Password? Look to
   parent @link(TSynaClient) object!

   Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}
  TNNTPSend = class(TSynaClient)
  private
    FSock: TTCPBlockSocket;
    FResultCode: Integer;
    FResultString: string;
    FData: TStringList;
    FDataToSend: TStringList;
    FAutoTLS: Boolean;
    FFullSSL: Boolean;
    FNNTPcap: TStringList;
    function ReadResult: Integer;
    function ReadData: boolean;
    function SendData: boolean;
    function Connect: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    {:Connects to NNTP server and begin session.}
    function Login: Boolean;

    {:Logout from NNTP server and terminate session.}
    function Logout: Boolean;

    {:By this you can call any NNTP command.}
    function DoCommand(const Command: string): boolean;

    {:by this you can call any NNTP command. This variant is used for commands
     for download information from server.}
    function DoCommandRead(const Command: string): boolean;

    {:by this you can call any NNTP command. This variant is used for commands
     for upload information to server.}
    function DoCommandWrite(const Command: string): boolean;

    {:Download full message to @link(data) property. Value can be number of
     message or message-id (in brackets).}
    function GetArticle(const Value: string): Boolean;

    {:Download only body of message to @link(data) property. Value can be number
     of message or message-id (in brackets).}
    function GetBody(const Value: string): Boolean;

    {:Download only headers of message to @link(data) property. Value can be
     number of message or message-id (in brackets).}
    function GetHead(const Value: string): Boolean;

    {:Get message status. Value can be number of message or message-id
     (in brackets).}
    function GetStat(const Value: string): Boolean;

    {:Select given group.}
    function SelectGroup(const Value: string): Boolean;

    {:Tell to server 'I have mesage with given message-ID.' If server need this
     message, message is uploaded to server.}
    function IHave(const MessID: string): Boolean;

    {:Move message pointer to last item in group.}
    function GotoLast: Boolean;

    {:Move message pointer to next item in group.}
    function GotoNext: Boolean;

    {:Download to @link(data) property list of all groups on NNTP server.}
    function ListGroups: Boolean;

    {:Download to @link(data) property list of all groups created after given time.}
    function ListNewGroups(Since: TDateTime): Boolean;

    {:Download to @link(data) property list of message-ids in given group since
     given time.}
    function NewArticles(const Group: string; Since: TDateTime): Boolean;

    {:Upload new article to server. (for new messages by you)}
    function PostArticle: Boolean;

    {:Tells to remote NNTP server 'I am not NNTP client, but I am another NNTP
     server'.}
    function SwitchToSlave: Boolean;

    {:Call NNTP XOVER command.}
    function Xover(xoStart, xoEnd: string): boolean;

    {:Call STARTTLS command for upgrade connection to SSL/TLS mode.}
    function StartTLS: Boolean;

    {:Try to find given capability in extension list. This list is getted after
     successful login to NNTP server. If extension capability is not found,
     then return is empty string.}
    function FindCap(const Value: string): string;

    {:Try get list of server extensions. List is returned in @link(data) property.}
    function ListExtensions: Boolean;
  published
    {:Result code number of last operation.}
    property ResultCode: Integer read FResultCode;

    {:String description of last result code from NNTP server.}
    property ResultString: string read FResultString;

    {:Readed data. (message, etc.)}
    property Data: TStringList read FData;

    {:If is set to @true, then upgrade to SSL/TLS mode after login if remote
     server support it.}
    property AutoTLS: Boolean read FAutoTLS Write FAutoTLS;

    {:SSL/TLS mode is used from first contact to server. Servers with full
     SSL/TLS mode usualy using non-standard TCP port!}
    property FullSSL: Boolean read FFullSSL Write FFullSSL;

    {:Socket object used for TCP/IP operation. Good for seting OnStatus hook, etc.}
    property Sock: TTCPBlockSocket read FSock;
  end;

implementation

constructor TNNTPSend.Create;
begin
  inherited Create;
  FSock := TTCPBlockSocket.Create;
  FSock.Owner := self;
  FData := TStringList.Create;
  FDataToSend := TStringList.Create;
  FNNTPcap := TStringList.Create;
  FSock.ConvertLineEnd := True;
  FTimeout := 60000;
  FTargetPort := cNNTPProtocol;
  FAutoTLS := False;
  FFullSSL := False;
end;

destructor TNNTPSend.Destroy;
begin
  FSock.Free;
  FDataToSend.Free;
  FData.Free;
  FNNTPcap.Free;
  inherited Destroy;
end;

function TNNTPSend.ReadResult: Integer;
var
  s: string;
begin
  Result := 0;
  FData.Clear;
  s := FSock.RecvString(FTimeout);
  FResultString := Copy(s, 5, Length(s) - 4);
  if FSock.LastError <> 0 then
    Exit;
  if Length(s) >= 3 then
    Result := StrToIntDef(Copy(s, 1, 3), 0);
  FResultCode := Result;
end;

function TNNTPSend.ReadData: boolean;
var
  s: string;
begin
  repeat
    s := FSock.RecvString(FTimeout);
    if s = '.' then
      break;
    if (s <> '') and (s[1] = '.') then
      s := Copy(s, 2, Length(s) - 1);
    FData.Add(s);
  until FSock.LastError <> 0;
  Result := FSock.LastError = 0;
end;

function TNNTPSend.SendData: boolean;
var
  s: string;
  n: integer;
begin
  for n := 0 to FDataToSend.Count - 1 do
  begin
    s := FDataToSend[n];
    if (s <> '') and (s[1] = '.') then
      s := s + '.';
    FSock.SendString(s + CRLF);
    if FSock.LastError <> 0 then
      break;
  end;
  if FDataToSend.Count = 0 then
    FSock.SendString(CRLF);
  if FSock.LastError = 0 then
    FSock.SendString('.' + CRLF);
  FDataToSend.Clear;
  Result := FSock.LastError = 0;
end;

function TNNTPSend.Connect: Boolean;
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

function TNNTPSend.Login: Boolean;
begin
  Result := False;
  FNNTPcap.Clear;
  if not Connect then
    Exit;
  Result := (ReadResult div 100) = 2;
  if Result then
  begin
    ListExtensions;
    FNNTPcap.Assign(Fdata);
    if (not FullSSL) and FAutoTLS and (FindCap('STARTTLS') <> '') then
      Result := StartTLS;
  end;
  if (FUsername <> '') and Result then
  begin
    FSock.SendString('AUTHINFO USER ' + FUsername + CRLF);
    if (ReadResult div 100) = 3 then
    begin
      FSock.SendString('AUTHINFO PASS ' + FPassword + CRLF);
      Result := (ReadResult div 100) = 2;
    end;
  end;
end;

function TNNTPSend.Logout: Boolean;
begin
  FSock.SendString('QUIT' + CRLF);
  Result := (ReadResult div 100) = 2;
  FSock.CloseSocket;
end;

function TNNTPSend.DoCommand(const Command: string): Boolean;
begin
  FSock.SendString(Command + CRLF);
  Result := (ReadResult div 100) = 2;
  Result := Result and (FSock.LastError = 0);
end;

function TNNTPSend.DoCommandRead(const Command: string): Boolean;
begin
  Result := DoCommand(Command);
  if Result then
  begin
    Result := ReadData;
    Result := Result and (FSock.LastError = 0);
  end;
end;

function TNNTPSend.DoCommandWrite(const Command: string): Boolean;
var
  x: integer;
begin
  FDataToSend.Assign(FData);
  FSock.SendString(Command + CRLF);
  x := (ReadResult div 100);
  if x = 3 then
  begin
    SendData;
    x := (ReadResult div 100);
  end;
  Result := x = 2;
  Result := Result and (FSock.LastError = 0);
end;

function TNNTPSend.GetArticle(const Value: string): Boolean;
var
  s: string;
begin
  s := 'ARTICLE';
  if Value <> '' then
    s := s + ' ' + Value;
  Result := DoCommandRead(s);
end;

function TNNTPSend.GetBody(const Value: string): Boolean;
var
  s: string;
begin
  s := 'BODY';
  if Value <> '' then
    s := s + ' ' + Value;
  Result := DoCommandRead(s);
end;

function TNNTPSend.GetHead(const Value: string): Boolean;
var
  s: string;
begin
  s := 'HEAD';
  if Value <> '' then
    s := s + ' ' + Value;
  Result := DoCommandRead(s);
end;

function TNNTPSend.GetStat(const Value: string): Boolean;
var
  s: string;
begin
  s := 'STAT';
  if Value <> '' then
    s := s + ' ' + Value;
  Result := DoCommand(s);
end;

function TNNTPSend.SelectGroup(const Value: string): Boolean;
begin
  Result := DoCommand('GROUP ' + Value);
end;

function TNNTPSend.IHave(const MessID: string): Boolean;
begin
  Result := DoCommandWrite('IHAVE ' + MessID);
end;

function TNNTPSend.GotoLast: Boolean;
begin
  Result := DoCommand('LAST');
end;

function TNNTPSend.GotoNext: Boolean;
begin
  Result := DoCommand('NEXT');
end;

function TNNTPSend.ListGroups: Boolean;
begin
  Result := DoCommandRead('LIST');
end;

function TNNTPSend.ListNewGroups(Since: TDateTime): Boolean;
begin
  Result := DoCommandRead('NEWGROUPS ' + SimpleDateTime(Since) + ' GMT');
end;

function TNNTPSend.NewArticles(const Group: string; Since: TDateTime): Boolean;
begin
  Result := DoCommandRead('NEWNEWS ' + Group + ' ' + SimpleDateTime(Since) + ' GMT');
end;

function TNNTPSend.PostArticle: Boolean;
begin
  Result := DoCommandWrite('POST');
end;

function TNNTPSend.SwitchToSlave: Boolean;
begin
  Result := DoCommand('SLAVE');
end;

function TNNTPSend.Xover(xoStart, xoEnd: string): Boolean;
var
  s: string;
begin
  s := 'XOVER ' + xoStart;
  if xoEnd <> xoStart then
    s := s + '-' + xoEnd;
  Result := DoCommandRead(s);
end;

function TNNTPSend.StartTLS: Boolean;
begin
  Result := False;
  if FindCap('STARTTLS') <> '' then
  begin
    if DoCommand('STARTTLS') then
    begin
      Fsock.SSLDoConnect;
      Result := FSock.LastError = 0;
    end;
  end;
end;

function TNNTPSend.ListExtensions: Boolean;
begin
  Result := DoCommandRead('LIST EXTENSIONS');
end;

function TNNTPSend.FindCap(const Value: string): string;
var
  n: Integer;
  s: string;
begin
  s := UpperCase(Value);
  Result := '';
  for n := 0 to FNNTPcap.Count - 1 do
    if Pos(s, UpperCase(FNNTPcap[n])) = 1 then
    begin
      Result := FNNTPcap[n];
      Break;
    end;
end;

{==============================================================================}

end.
