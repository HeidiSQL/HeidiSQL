{==============================================================================|
| Project : Ararat Synapse                                       | 003.000.003 |
|==============================================================================|
| Content: SNTP client                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2000-2010.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Patrick Chevalley                                                          |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract( NTP and SNTP client)

Used RFC: RFC-1305, RFC-2030
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$H+}

unit sntpsend;

interface

uses
  SysUtils,
  synsock, blcksock, synautil;

const
  cNtpProtocol = '123';

type

  {:@abstract(Record containing the NTP packet.)}
  TNtp = packed record
    mode: Byte;
    stratum: Byte;
    poll: Byte;
    Precision: Byte;
    RootDelay: Longint;
    RootDisperson: Longint;
    RefID: Longint;
    Ref1: Longint;
    Ref2: Longint;
    Org1: Longint;
    Org2: Longint;
    Rcv1: Longint;
    Rcv2: Longint;
    Xmit1: Longint;
    Xmit2: Longint;
  end;

  {:@abstract(Implementation of NTP and SNTP client protocol),
   include time synchronisation. It can send NTP or SNTP time queries, or it
   can receive NTP broadcasts too.
   
   Note: Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}
  TSNTPSend = class(TSynaClient)
  private
    FNTPReply: TNtp;
    FNTPTime: TDateTime;
    FNTPOffset: double;
    FNTPDelay: double;
    FMaxSyncDiff: double;
    FSyncTime: Boolean;
    FSock: TUDPBlockSocket;
    FBuffer: AnsiString;
    FLi, FVn, Fmode : byte;
    function StrToNTP(const Value: AnsiString): TNtp;
    function NTPtoStr(const Value: Tntp): AnsiString;
    procedure ClearNTP(var Value: Tntp);
  public
    constructor Create;
    destructor Destroy; override;

    {:Decode 128 bit timestamp used in NTP packet to TDateTime type.}
    function DecodeTs(Nsec, Nfrac: Longint): TDateTime;

    {:Decode TDateTime type to 128 bit timestamp used in NTP packet.}
    procedure EncodeTs(dt: TDateTime; var Nsec, Nfrac: Longint);

    {:Send request to @link(TSynaClient.TargetHost) and wait for reply. If all
     is OK, then result is @true and @link(NTPReply) and @link(NTPTime) are
     valid.}
    function GetSNTP: Boolean;

    {:Send request to @link(TSynaClient.TargetHost) and wait for reply. If all
     is OK, then result is @true and @link(NTPReply) and @link(NTPTime) are
     valid. Result time is after all needed corrections.}
    function GetNTP: Boolean;

    {:Wait for broadcast NTP packet. If all OK, result is @true and
     @link(NTPReply) and @link(NTPTime) are valid.}
    function GetBroadcastNTP: Boolean;

    {:Holds last received NTP packet.}
    property NTPReply: TNtp read FNTPReply;
  published
    {:Date and time of remote NTP or SNTP server. (UTC time!!!)}
    property NTPTime: TDateTime read FNTPTime;

    {:Offset between your computer and remote NTP or SNTP server.}
    property NTPOffset: Double read FNTPOffset;

    {:Delay between your computer and remote NTP or SNTP server.}
    property NTPDelay: Double read FNTPDelay;

    {:Define allowed maximum difference between your time and remote time for
     synchronising time. If difference is bigger, your system time is not
     changed!}
    property MaxSyncDiff: double read FMaxSyncDiff write FMaxSyncDiff;

    {:If @true, after successfull getting time is local computer clock
     synchronised to given time.
     For synchronising time you must have proper rights! (Usually Administrator)}
    property SyncTime: Boolean read FSyncTime write FSyncTime;

    {:Socket object used for TCP/IP operation. Good for seting OnStatus hook, etc.}
    property Sock: TUDPBlockSocket read FSock;
  end;

implementation

constructor TSNTPSend.Create;
begin
  inherited Create;
  FSock := TUDPBlockSocket.Create;
  FSock.Owner := self;
  FTimeout := 5000;
  FTargetPort := cNtpProtocol;
  FMaxSyncDiff := 3600;
  FSyncTime := False;
end;

destructor TSNTPSend.Destroy;
begin
  FSock.Free;
  inherited Destroy;
end;

function TSNTPSend.StrToNTP(const Value: AnsiString): TNtp;
begin
  if length(FBuffer) >= SizeOf(Result) then
  begin
    Result.mode := ord(Value[1]);
    Result.stratum := ord(Value[2]);
    Result.poll := ord(Value[3]);
    Result.Precision := ord(Value[4]);
    Result.RootDelay := DecodeLongInt(value, 5);
    Result.RootDisperson := DecodeLongInt(value, 9);
    Result.RefID := DecodeLongInt(value, 13);
    Result.Ref1 := DecodeLongInt(value, 17);
    Result.Ref2 := DecodeLongInt(value, 21);
    Result.Org1 := DecodeLongInt(value, 25);
    Result.Org2 := DecodeLongInt(value, 29);
    Result.Rcv1 := DecodeLongInt(value, 33);
    Result.Rcv2 := DecodeLongInt(value, 37);
    Result.Xmit1 := DecodeLongInt(value, 41);
    Result.Xmit2 := DecodeLongInt(value, 45);
  end;

end;

function TSNTPSend.NTPtoStr(const Value: Tntp): AnsiString;
begin
  SetLength(Result, 4);
  Result[1] := AnsiChar(Value.mode);
  Result[2] := AnsiChar(Value.stratum);
  Result[3] := AnsiChar(Value.poll);
  Result[4] := AnsiChar(Value.precision);
  Result := Result + CodeLongInt(Value.RootDelay);
  Result := Result + CodeLongInt(Value.RootDisperson);
  Result := Result + CodeLongInt(Value.RefID);
  Result := Result + CodeLongInt(Value.Ref1);
  Result := Result + CodeLongInt(Value.Ref2);
  Result := Result + CodeLongInt(Value.Org1);
  Result := Result + CodeLongInt(Value.Org2);
  Result := Result + CodeLongInt(Value.Rcv1);
  Result := Result + CodeLongInt(Value.Rcv2);
  Result := Result + CodeLongInt(Value.Xmit1);
  Result := Result + CodeLongInt(Value.Xmit2);
end;

procedure TSNTPSend.ClearNTP(var Value: Tntp);
begin
  Value.mode := 0;
  Value.stratum := 0;
  Value.poll := 0;
  Value.Precision := 0;
  Value.RootDelay := 0;
  Value.RootDisperson := 0;
  Value.RefID := 0;
  Value.Ref1 := 0;
  Value.Ref2 := 0;
  Value.Org1 := 0;
  Value.Org2 := 0;
  Value.Rcv1 := 0;
  Value.Rcv2 := 0;
  Value.Xmit1 := 0;
  Value.Xmit2 := 0;
end;

function TSNTPSend.DecodeTs(Nsec, Nfrac: Longint): TDateTime;
const
  maxi = 4294967295.0;
var
  d, d1: Double;
begin
  d := Nsec;
  if d < 0 then
    d := maxi + d + 1;
  d1 := Nfrac;
  if d1 < 0 then
    d1 := maxi + d1 + 1;
  d1 := d1 / maxi;
  d1 := Trunc(d1 * 10000) / 10000;
  Result := (d + d1) / 86400;
  Result := Result + 2;
end;

procedure TSNTPSend.EncodeTs(dt: TDateTime; var Nsec, Nfrac: Longint);
const
  maxi = 4294967295.0;
  maxilongint = 2147483647;
var
  d, d1: Double;
begin
  d  := (dt - 2) * 86400;
  d1 := frac(d);
  if d > maxilongint then
     d := d - maxi - 1;
  d  := trunc(d);
  d1 := Trunc(d1 * 10000) / 10000;
  d1 := d1 * maxi;
  if d1 > maxilongint then
     d1 := d1 - maxi - 1;
  Nsec:=trunc(d);
  Nfrac:=trunc(d1);
end;

function TSNTPSend.GetBroadcastNTP: Boolean;
var
  x: Integer;
begin
  Result := False;
  FSock.Bind(FIPInterface, FTargetPort);
  FBuffer := FSock.RecvPacket(FTimeout);
  if FSock.LastError = 0 then
  begin
    x := Length(FBuffer);
    if (FTargetHost = '0.0.0.0') or (FSock.GetRemoteSinIP = FSock.ResolveName(FTargetHost)) then
      if x >= SizeOf(NTPReply) then
      begin
        FNTPReply := StrToNTP(FBuffer);
        FNTPTime := DecodeTs(NTPReply.Xmit1, NTPReply.Xmit2);
        if FSyncTime and ((abs(FNTPTime - GetUTTime) * 86400) <= FMaxSyncDiff) then
          SetUTTime(FNTPTime);
        Result := True;
      end;
  end;
end;

function TSNTPSend.GetSNTP: Boolean;
var
  q: TNtp;
  x: Integer;
begin
  Result := False;
  FSock.CloseSocket;
  FSock.Bind(FIPInterface, cAnyPort);
  FSock.Connect(FTargetHost, FTargetPort);
  ClearNtp(q);
  q.mode := $1B;
  FBuffer := NTPtoStr(q);
  FSock.SendString(FBuffer);
  FBuffer := FSock.RecvPacket(FTimeout);
  if FSock.LastError = 0 then
  begin
    x := Length(FBuffer);
    if x >= SizeOf(NTPReply) then
    begin
      FNTPReply := StrToNTP(FBuffer);
      FNTPTime := DecodeTs(NTPReply.Xmit1, NTPReply.Xmit2);
      if FSyncTime and ((abs(FNTPTime - GetUTTime) * 86400) <= FMaxSyncDiff) then
        SetUTTime(FNTPTime);
      Result := True;
    end;
  end;
end;

function TSNTPSend.GetNTP: Boolean;
var
  q: TNtp;
  x: Integer;
  t1, t2, t3, t4 : TDateTime;
begin
  Result := False;
  FSock.CloseSocket;
  FSock.Bind(FIPInterface, cAnyPort);
  FSock.Connect(FTargetHost, FTargetPort);
  ClearNtp(q);
  q.mode := $1B;
  t1 := GetUTTime;
  EncodeTs(t1, q.org1, q.org2);
  FBuffer := NTPtoStr(q);
  FSock.SendString(FBuffer);
  FBuffer := FSock.RecvPacket(FTimeout);
  if FSock.LastError = 0 then
  begin
    x := Length(FBuffer);
    t4 := GetUTTime;
    if x >= SizeOf(NTPReply) then
    begin
      FNTPReply := StrToNTP(FBuffer);
      FLi := (NTPReply.mode and $C0) shr 6;
      FVn := (NTPReply.mode and $38) shr 3;
      Fmode := NTPReply.mode and $07;
      if (Fli < 3) and (Fmode = 4) and
         (NTPReply.stratum >= 1) and (NTPReply.stratum <= 15) and
         (NTPReply.Rcv1 <> 0) and (NTPReply.Xmit1 <> 0)
         then begin
           t2 := DecodeTs(NTPReply.Rcv1, NTPReply.Rcv2);
           t3 := DecodeTs(NTPReply.Xmit1, NTPReply.Xmit2);
           FNTPDelay := (T4 - T1) - (T2 - T3);
           FNTPTime := t3 + FNTPDelay / 2;
           FNTPOffset := (((T2 - T1) + (T3 - T4)) / 2) * 86400;
           FNTPDelay := FNTPDelay * 86400;
           if FSyncTime and ((abs(FNTPTime - t1) * 86400) <= FMaxSyncDiff) then
             SetUTTime(FNTPTime);
           Result := True;
           end
         else result:=false;
    end;
  end;
end;

end.
