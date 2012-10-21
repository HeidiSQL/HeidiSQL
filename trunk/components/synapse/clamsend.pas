{==============================================================================|
| Project : Ararat Synapse                                       | 001.001.001 |
|==============================================================================|
| Content: ClamAV-daemon client                                                |
|==============================================================================|
| Copyright (c)2005-2010, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)2005-2010.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract( ClamAV-daemon client)

This unit is capable to do antivirus scan of your data by TCP channel to ClamD
daemon from ClamAV. See more about ClamAV on @LINK(http://www.clamav.net)
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

unit clamsend;

interface

uses
  SysUtils, Classes,
  synsock, blcksock, synautil;

const
  cClamProtocol = '3310';

type

  {:@abstract(Implementation of ClamAV-daemon client protocol)
   By this class you can scan any your data by ClamAV opensource antivirus.

   This class can connect to ClamD by TCP channel, send your data to ClamD
   and read result.}
  TClamSend = class(TSynaClient)
  private
    FSock: TTCPBlockSocket;
    FDSock: TTCPBlockSocket;
    FSession: boolean;
    function Login: boolean; virtual;
    function Logout: Boolean; virtual;
    function OpenStream: Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    {:Call any command to ClamD. Used internally by other methods.}
    function DoCommand(const Value: AnsiString): AnsiString; virtual;

    {:Return ClamAV version and version of loaded databases.}
    function GetVersion: AnsiString; virtual;

    {:Scan content of TStrings.}
    function ScanStrings(const Value: TStrings): AnsiString; virtual;

    {:Scan content of TStream.}
    function ScanStream(const Value: TStream): AnsiString; virtual;

    {:Scan content of TStrings by new 0.95 API.}
    function ScanStrings2(const Value: TStrings): AnsiString; virtual;

    {:Scan content of TStream by new 0.95 API.}
    function ScanStream2(const Value: TStream): AnsiString; virtual;
  published
    {:Socket object used for TCP/IP operation. Good for seting OnStatus hook, etc.}
    property Sock: TTCPBlockSocket read FSock;

    {:Socket object used for TCP data transfer operation. Good for seting OnStatus hook, etc.}
    property DSock: TTCPBlockSocket read FDSock;

    {:Can turn-on session mode of communication with ClamD. Default is @false,
     because ClamAV developers design their TCP code very badly and session mode
     is broken now (CVS-20051031). Maybe ClamAV developers fix their bugs
     and this mode will be possible in future.}
    property Session: boolean read FSession write FSession;
  end;

implementation

constructor TClamSend.Create;
begin
  inherited Create;
  FSock := TTCPBlockSocket.Create;
  FSock.Owner := self;
  FDSock := TTCPBlockSocket.Create;
  FDSock.Owner := self;
  FTimeout := 60000;
  FTargetPort := cClamProtocol;
  FSession := false;
end;

destructor TClamSend.Destroy;
begin
  Logout;
  FDSock.Free;
  FSock.Free;
  inherited Destroy;
end;

function TClamSend.DoCommand(const Value: AnsiString): AnsiString;
begin
  Result := '';
  if not FSession then
    FSock.CloseSocket
  else
    FSock.SendString(Value + LF);
  if not FSession or (FSock.LastError <> 0) then
  begin
    if Login then
      FSock.SendString(Value + LF)
    else
      Exit;
  end;
  Result := FSock.RecvTerminated(FTimeout, LF);
end;

function TClamSend.Login: boolean;
begin
  Result := False;
  Sock.CloseSocket;
  FSock.Bind(FIPInterface, cAnyPort);
  if FSock.LastError <> 0 then
    Exit;
  FSock.Connect(FTargetHost, FTargetPort);
  if FSock.LastError <> 0 then
    Exit;
  if FSession then
    FSock.SendString('SESSION' + LF);
  Result := FSock.LastError = 0;
end;

function TClamSend.Logout: Boolean;
begin
  FSock.SendString('END' + LF);
  Result := FSock.LastError = 0;
  FSock.CloseSocket;
end;

function TClamSend.GetVersion: AnsiString;
begin
  Result := DoCommand('nVERSION');
end;

function TClamSend.OpenStream: Boolean;
var
  S: AnsiString;
begin
  Result := False;
  s := DoCommand('nSTREAM');
  if (s <> '') and (Copy(s, 1, 4) = 'PORT') then
  begin
    s := SeparateRight(s, ' ');
    FDSock.CloseSocket;
    FDSock.Bind(FIPInterface, cAnyPort);
    if FDSock.LastError <> 0 then
      Exit;
    FDSock.Connect(FTargetHost, s);
    if FDSock.LastError <> 0 then
      Exit;
    Result := True;
  end;
end;

function TClamSend.ScanStrings(const Value: TStrings): AnsiString;
begin
  Result := '';
  if OpenStream then
  begin
    DSock.SendString(Value.Text);
    DSock.CloseSocket;
    Result := FSock.RecvTerminated(FTimeout, LF);
  end;
end;

function TClamSend.ScanStream(const Value: TStream): AnsiString;
begin
  Result := '';
  if OpenStream then
  begin
    DSock.SendStreamRaw(Value);
    DSock.CloseSocket;
    Result := FSock.RecvTerminated(FTimeout, LF);
  end;
end;

function TClamSend.ScanStrings2(const Value: TStrings): AnsiString;
var
  i: integer;
  s: AnsiString;
begin
  Result := '';
  if not FSession then
    FSock.CloseSocket
  else
    FSock.sendstring('nINSTREAM' + LF);
  if not FSession or (FSock.LastError <> 0) then
  begin
    if Login then
      FSock.sendstring('nINSTREAM' + LF)
    else
      Exit;
  end;
  s := Value.text;
  i := length(s);
  FSock.SendString(CodeLongint(i) + s + #0#0#0#0);
  Result := FSock.RecvTerminated(FTimeout, LF);
end;

function TClamSend.ScanStream2(const Value: TStream): AnsiString;
var
  i: integer;
begin
  Result := '';
  if not FSession then
    FSock.CloseSocket
  else
    FSock.sendstring('nINSTREAM' + LF);
  if not FSession or (FSock.LastError <> 0) then
  begin
    if Login then
      FSock.sendstring('nINSTREAM' + LF)
    else
      Exit;
  end;
  i := value.Size;
  FSock.SendString(CodeLongint(i));
  FSock.SendStreamRaw(Value);
  FSock.SendString(#0#0#0#0);
  Result := FSock.RecvTerminated(FTimeout, LF);
end;

end.
