{==============================================================================|
| Project : Ararat Synapse                                       | 001.003.001 |
|==============================================================================|
| Content: TELNET and SSH2 client                                              |
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
| Portions created by Lukas Gebauer are Copyright (c)2002-2010.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(Telnet script client)

Used RFC: RFC-854
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit tlntsend;

interface

uses
  SysUtils, Classes,
  blcksock, synautil;

const
  cTelnetProtocol = '23';
  cSSHProtocol = '22';

  TLNT_EOR                = #239;
  TLNT_SE                 = #240;
  TLNT_NOP                = #241;
  TLNT_DATA_MARK          = #242;
  TLNT_BREAK              = #243;
  TLNT_IP                 = #244;
  TLNT_AO                 = #245;
  TLNT_AYT                = #246;
  TLNT_EC                 = #247;
  TLNT_EL                 = #248;
  TLNT_GA                 = #249;
  TLNT_SB                 = #250;
  TLNT_WILL               = #251;
  TLNT_WONT               = #252;
  TLNT_DO                 = #253;
  TLNT_DONT               = #254;
  TLNT_IAC                = #255;

type
  {:@abstract(State of telnet protocol). Used internaly by TTelnetSend.}
  TTelnetState =(tsDATA, tsIAC, tsIAC_SB, tsIAC_WILL, tsIAC_DO, tsIAC_WONT,
     tsIAC_DONT, tsIAC_SBIAC, tsIAC_SBDATA, tsSBDATA_IAC);

  {:@abstract(Class with implementation of Telnet/SSH script client.)

   Note: Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}
  TTelnetSend = class(TSynaClient)
  private
    FSock: TTCPBlockSocket;
    FBuffer: Ansistring;
    FState: TTelnetState;
    FSessionLog: Ansistring;
    FSubNeg: Ansistring;
    FSubType: Ansichar;
    FTermType: Ansistring;
    function Connect: Boolean;
    function Negotiate(const Buf: Ansistring): Ansistring;
    procedure FilterHook(Sender: TObject; var Value: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;

    {:Connects to Telnet server.}
    function Login: Boolean;

    {:Connects to SSH2 server and login by Username and Password properties.

     You must use some of SSL plugins with SSH support. For exammple CryptLib.}
    function SSHLogin: Boolean;

    {:Logout from telnet server.}
    procedure Logout;

    {:Send this data to telnet server.}
    procedure Send(const Value: string);

    {:Reading data from telnet server until Value is readed. If it is not readed
     until timeout, result is @false. Otherwise result is @true.}
    function WaitFor(const Value: string): Boolean;

    {:Read data terminated by terminator from telnet server.}
    function RecvTerminated(const Terminator: string): string;

    {:Read string from telnet server.}
    function RecvString: string;
  published
    {:Socket object used for TCP/IP operation. Good for seting OnStatus hook, etc.}
    property Sock: TTCPBlockSocket read FSock;

    {:all readed datas in this session (from connect) is stored in this large
     string.}
    property SessionLog: Ansistring read FSessionLog write FSessionLog;

    {:Terminal type indentification. By default is 'SYNAPSE'.}
    property TermType: Ansistring read FTermType write FTermType;
  end;

implementation

constructor TTelnetSend.Create;
begin
  inherited Create;
  FSock := TTCPBlockSocket.Create;
  FSock.Owner := self;
  FSock.OnReadFilter := FilterHook;
  FTimeout := 60000;
  FTargetPort := cTelnetProtocol;
  FSubNeg := '';
  FSubType := #0;
  FTermType := 'SYNAPSE';
end;

destructor TTelnetSend.Destroy;
begin
  FSock.Free;
  inherited Destroy;
end;

function TTelnetSend.Connect: Boolean;
begin
  // Do not call this function! It is calling by LOGIN method!
  FBuffer := '';
  FSessionLog := '';
  FState := tsDATA;
  FSock.CloseSocket;
  FSock.LineBuffer := '';
  FSock.Bind(FIPInterface, cAnyPort);
  FSock.Connect(FTargetHost, FTargetPort);
  Result := FSock.LastError = 0;
end;

function TTelnetSend.RecvTerminated(const Terminator: string): string;
begin
  Result := FSock.RecvTerminated(FTimeout, Terminator);
end;

function TTelnetSend.RecvString: string;
begin
  Result := FSock.RecvTerminated(FTimeout, CRLF);
end;

function TTelnetSend.WaitFor(const Value: string): Boolean;
begin
  Result := FSock.RecvTerminated(FTimeout, Value) <> '';
end;

procedure TTelnetSend.FilterHook(Sender: TObject; var Value: AnsiString);
begin
  Value := Negotiate(Value);
  FSessionLog := FSessionLog + Value;
end;

function TTelnetSend.Negotiate(const Buf: Ansistring): Ansistring;
var
  n: integer;
  c: Ansichar;
  Reply: Ansistring;
  SubReply: Ansistring;
begin
  Result := '';
  for n := 1 to Length(Buf) do
  begin
    c := Buf[n];
    Reply := '';
    case FState of
      tsData:
        if c = TLNT_IAC then
          FState := tsIAC
        else
          Result := Result + c;

      tsIAC:
        case c of
          TLNT_IAC:
            begin
              FState := tsData;
              Result := Result + TLNT_IAC;
            end;
          TLNT_WILL:
            FState := tsIAC_WILL;
          TLNT_WONT:
            FState := tsIAC_WONT;
          TLNT_DONT:
            FState := tsIAC_DONT;
          TLNT_DO:
            FState := tsIAC_DO;
          TLNT_EOR:
            FState := tsDATA;
          TLNT_SB:
            begin
              FState := tsIAC_SB;
              FSubType := #0;
              FSubNeg := '';
            end;
        else
          FState := tsData;
        end;

      tsIAC_WILL:
        begin
        case c of
          #3:  //suppress GA
            Reply := TLNT_DO;
        else
          Reply := TLNT_DONT;
        end;
          FState := tsData;
        end;

      tsIAC_WONT:
        begin
          Reply := TLNT_DONT;
          FState := tsData;
        end;

      tsIAC_DO:
      begin
        case c of
          #24:  //termtype
            Reply := TLNT_WILL;
        else
          Reply := TLNT_WONT;
        end;
        FState := tsData;
      end;

      tsIAC_DONT:
      begin
        Reply := TLNT_WONT;
        FState := tsData;
      end;

      tsIAC_SB:
        begin
          FSubType := c;
          FState := tsIAC_SBDATA;
        end;

      tsIAC_SBDATA:
        begin
          if c = TLNT_IAC then
            FState := tsSBDATA_IAC
          else
            FSubNeg := FSubNeg + c;
        end;

      tsSBDATA_IAC:
        case c of
          TLNT_IAC:
            begin
              FState := tsIAC_SBDATA;
              FSubNeg := FSubNeg + c;
            end;
          TLNT_SE:
            begin
              SubReply := '';
              case FSubType of
                #24:  //termtype
                  begin
                    if (FSubNeg <> '') and (FSubNeg[1] = #1) then
                      SubReply := #0 + FTermType;
                  end;
              end;
              Sock.SendString(TLNT_IAC + TLNT_SB + FSubType + SubReply + TLNT_IAC + TLNT_SE);
              FState := tsDATA;
            end;
         else
           FState := tsDATA;
         end;

      else
        FState := tsData;
    end;
    if Reply <> '' then
      Sock.SendString(TLNT_IAC + Reply + c);
  end;

end;

procedure TTelnetSend.Send(const Value: string);
begin
  Sock.SendString(ReplaceString(Value, TLNT_IAC, TLNT_IAC + TLNT_IAC));
end;

function TTelnetSend.Login: Boolean;
begin
  Result := False;
  if not Connect then
    Exit;
  Result := True;
end;

function TTelnetSend.SSHLogin: Boolean;
begin
  Result := False;
  if Connect then
  begin
    FSock.SSL.SSLType := LT_SSHv2;
    FSock.SSL.Username := FUsername;
    FSock.SSL.Password := FPassword;
    FSock.SSLDoConnect;
    Result := FSock.LastError = 0;
  end;
end;

procedure TTelnetSend.Logout;
begin
  FSock.CloseSocket;
end;


end.
