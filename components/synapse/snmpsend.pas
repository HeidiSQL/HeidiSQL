{==============================================================================|
| Project : Ararat Synapse                                       | 004.000.000 |
|==============================================================================|
| Content: SNMP client                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2000-2011.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Jean-Fabien Connault (cycocrew@worldnet.fr)                                |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(SNMP client)
Supports SNMPv1 include traps, SNMPv2c and SNMPv3 include authorization
and privacy encryption.

Used RFC: RFC-1157, RFC-1901, RFC-3412, RFC-3414, RFC-3416, RFC-3826

Supported Authorization hashes: MD5, SHA1
Supported Privacy encryptions: DES, 3DES, AES
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

unit snmpsend;

interface

uses
  Classes, SysUtils,
  blcksock, synautil, asn1util, synaip, synacode, synacrypt;

const
  cSnmpProtocol = '161';
  cSnmpTrapProtocol = '162';

  SNMP_V1 = 0;
  SNMP_V2C = 1;
  SNMP_V3 = 3;

  //PDU type
  PDUGetRequest = $A0;
  PDUGetNextRequest = $A1;
  PDUGetResponse = $A2;
  PDUSetRequest = $A3;
  PDUTrap = $A4; //Obsolete
  //for SNMPv2
  PDUGetBulkRequest = $A5;
  PDUInformRequest = $A6;
  PDUTrapV2 = $A7;
  PDUReport = $A8;

  //errors
  ENoError = 0;
  ETooBig = 1;
  ENoSuchName = 2;
  EBadValue = 3;
  EReadOnly = 4;
  EGenErr = 5;
  //errors SNMPv2
  ENoAccess = 6;
  EWrongType = 7;
  EWrongLength = 8;
  EWrongEncoding = 9;
  EWrongValue = 10;
  ENoCreation = 11;
  EInconsistentValue = 12;
  EResourceUnavailable = 13;
  ECommitFailed = 14;
  EUndoFailed = 15;
  EAuthorizationError = 16;
  ENotWritable = 17;
  EInconsistentName = 18;

type

  {:@abstract(Possible values for SNMPv3 flags.)
   This flags specify level of authorization and encryption.}
  TV3Flags = (
    NoAuthNoPriv,
    AuthNoPriv,
    AuthPriv);

  {:@abstract(Type of SNMPv3 authorization)}
  TV3Auth = (
    AuthMD5,
    AuthSHA1);

  {:@abstract(Type of SNMPv3 privacy)}
  TV3Priv = (
    PrivDES,
    Priv3DES,
    PrivAES);

  {:@abstract(Data object with one record of MIB OID and corresponding values.)}
  TSNMPMib = class(TObject)
  protected
    FOID: AnsiString;
    FValue: AnsiString;
    FValueType: Integer;
  published
    {:OID number in string format.}
    property OID: AnsiString read FOID write FOID;

    {:Value of OID object in string format.}
    property Value: AnsiString read FValue write FValue;

    {:Define type of Value. Supported values are defined in @link(asn1util).
     For queries use ASN1_NULL, becouse you don't know type in response!}
    property ValueType: Integer read FValueType write FValueType;
  end;

  {:@abstract(It holding all information for SNMPv3 agent synchronization)
   Used internally.}
  TV3Sync = record
    EngineID: AnsiString;
    EngineBoots: integer;
    EngineTime: integer;
    EngineStamp: Cardinal;
  end;

  {:@abstract(Data object abstracts SNMP data packet)}
  TSNMPRec = class(TObject)
  protected
    FVersion: Integer;
    FPDUType: Integer;
    FID: Integer;
    FErrorStatus: Integer;
    FErrorIndex: Integer;
    FCommunity: AnsiString;
    FSNMPMibList: TList;
    FMaxSize: Integer;
    FFlags: TV3Flags;
    FFlagReportable: Boolean;
    FContextEngineID: AnsiString;
    FContextName: AnsiString;
    FAuthMode: TV3Auth;
    FAuthEngineID: AnsiString;
    FAuthEngineBoots: integer;
    FAuthEngineTime: integer;
    FAuthEngineTimeStamp: cardinal;
    FUserName: AnsiString;
    FPassword: AnsiString;
    FAuthKey: AnsiString;
    FPrivMode: TV3Priv;
    FPrivPassword: AnsiString;
    FPrivKey: AnsiString;
    FPrivSalt: AnsiString;
    FPrivSaltCounter: integer;
    FOldTrapEnterprise: AnsiString;
    FOldTrapHost: AnsiString;
    FOldTrapGen: Integer;
    FOldTrapSpec: Integer;
    FOldTrapTimeTicks: Integer;
    function Pass2Key(const Value: AnsiString): AnsiString;
    function EncryptPDU(const value: AnsiString): AnsiString;
    function DecryptPDU(const value: AnsiString): AnsiString;
  public
    constructor Create;
    destructor Destroy; override;

    {:Decode SNMP packet in buffer to object properties.}
    function DecodeBuf(Buffer: AnsiString): Boolean;

    {:Encode obeject properties to SNMP packet.}
    function EncodeBuf: AnsiString;

    {:Clears all object properties to default values.}
    procedure Clear;

    {:Add entry to @link(SNMPMibList). For queries use value as empty string,
     and ValueType as ASN1_NULL.}
    procedure MIBAdd(const MIB, Value: AnsiString; ValueType: Integer);

    {:Delete entry from @link(SNMPMibList).}
    procedure MIBDelete(Index: Integer);

    {:Search @link(SNMPMibList) list for MIB and return correspond value.}
    function MIBGet(const MIB: AnsiString): AnsiString;

    {:return number of entries in MIB array.}
    function MIBCount: integer;

    {:Return MIB information from given row of MIB array.}
    function MIBByIndex(Index: Integer): TSNMPMib;

    {:List of @link(TSNMPMib) objects.}
    property SNMPMibList: TList read FSNMPMibList;
  published
    {:Version of SNMP packet. Default value is 0 (SNMP ver. 1). You can use
     value 1 for SNMPv2c or value 3 for SNMPv3.}
    property Version: Integer read FVersion write FVersion;

    {:Community string for autorize access to SNMP server. (Case sensitive!)
     Community string is not used in SNMPv3! Use @link(Username) and
     @link(password) instead!}
    property Community: AnsiString read FCommunity write FCommunity;

    {:Define type of SNMP operation.}
    property PDUType: Integer read FPDUType write FPDUType;

    {:Contains ID number. Not need to use.}
    property ID: Integer read FID write FID;

    {:When packet is reply, contains error code. Supported values are defined by
     E* constants.}
    property ErrorStatus: Integer read FErrorStatus write FErrorStatus;

    {:Point to error position in reply packet. Not usefull for users. It only
     good for debugging!}
    property ErrorIndex: Integer read FErrorIndex write FErrorIndex;

    {:special value for GetBulkRequest of SNMPv2 and v3.}
    property NonRepeaters: Integer read FErrorStatus write FErrorStatus;

    {:special value for GetBulkRequest of SNMPv2 and v3.}
    property MaxRepetitions: Integer read FErrorIndex write FErrorIndex;

    {:Maximum message size in bytes for SNMPv3. For sending is default 1472 bytes.}
    property MaxSize: Integer read FMaxSize write FMaxSize;

    {:Specify if message is authorised or encrypted. Used only in SNMPv3.}
    property Flags: TV3Flags read FFlags write FFlags;

    {:For SNMPv3.... If is @true, SNMP agent must send reply (at least with some
     error).}
    property FlagReportable: Boolean read FFlagReportable write FFlagReportable;

    {:For SNMPv3. If not specified, is used value from @link(AuthEngineID)}
    property ContextEngineID: AnsiString read FContextEngineID write FContextEngineID;

    {:For SNMPv3.}
    property ContextName: AnsiString read FContextName write FContextName;

    {:For SNMPv3. Specify Authorization mode. (specify used hash for
     authorization)}
    property AuthMode: TV3Auth read FAuthMode write FAuthMode;

    {:For SNMPv3. Specify Privacy mode.}
    property PrivMode: TV3Priv read FPrivMode write FPrivMode;

    {:value used by SNMPv3 authorisation for synchronization with SNMP agent.}
    property AuthEngineID: AnsiString read FAuthEngineID write FAuthEngineID;

    {:value used by SNMPv3 authorisation for synchronization with SNMP agent.}
    property AuthEngineBoots: Integer read FAuthEngineBoots write FAuthEngineBoots;

    {:value used by SNMPv3 authorisation for synchronization with SNMP agent.}
    property AuthEngineTime: Integer read FAuthEngineTime write FAuthEngineTime;

    {:value used by SNMPv3 authorisation for synchronization with SNMP agent.}
    property AuthEngineTimeStamp: Cardinal read FAuthEngineTimeStamp Write FAuthEngineTimeStamp;

    {:SNMPv3 authorization username}
    property UserName: AnsiString read FUserName write FUserName;

    {:SNMPv3 authorization password}
    property Password: AnsiString read FPassword write FPassword;

    {:For SNMPv3. Computed Athorization key from @link(password).}
    property AuthKey: AnsiString read FAuthKey write FAuthKey;

    {:SNMPv3 privacy password}
    property PrivPassword: AnsiString read FPrivPassword write FPrivPassword;

    {:For SNMPv3. Computed Privacy key from @link(PrivPassword).}
    property PrivKey: AnsiString read FPrivKey write FPrivKey;

    {:MIB value to identify the object that sent the TRAPv1.}
    property OldTrapEnterprise: AnsiString read FOldTrapEnterprise write FOldTrapEnterprise;

    {:Address of TRAPv1 sender (IP address).}
    property OldTrapHost: AnsiString read FOldTrapHost write FOldTrapHost;

    {:Generic TRAPv1 identification.}
    property OldTrapGen: Integer read FOldTrapGen write FOldTrapGen;

    {:Specific TRAPv1 identification.}
    property OldTrapSpec: Integer read FOldTrapSpec write FOldTrapSpec;

    {:Number of 1/100th of seconds since last reboot or power up. (for TRAPv1)}
    property OldTrapTimeTicks: Integer read FOldTrapTimeTicks write FOldTrapTimeTicks;
  end;

  {:@abstract(Implementation of SNMP protocol.)

   Note: Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}
  TSNMPSend = class(TSynaClient)
  protected
    FSock: TUDPBlockSocket;
    FBuffer: AnsiString;
    FHostIP: AnsiString;
    FQuery: TSNMPRec;
    FReply: TSNMPRec;
    function InternalSendSnmp(const Value: TSNMPRec): Boolean;
    function InternalRecvSnmp(const Value: TSNMPRec): Boolean;
    function InternalSendRequest(const QValue, RValue: TSNMPRec): Boolean;
    function GetV3EngineID: AnsiString;
    function GetV3Sync: TV3Sync;
  public
    constructor Create;
    destructor Destroy; override;

    {:Connects to a Host and send there query. If in timeout SNMP server send
     back query, result is @true. If is used SNMPv3, then it synchronize self
     with SNMPv3 agent first. (It is needed for SNMPv3 auhorization!)}
    function SendRequest: Boolean;

    {:Send SNMP packet only, but not waits for reply. Good for sending traps.}
    function SendTrap: Boolean;

    {:Receive SNMP packet only. Good for receiving traps.}
    function RecvTrap: Boolean;

    {:Mapped to @link(SendRequest) internally. This function is only for
     backward compatibility.}
    function DoIt: Boolean;
  published
    {:contains raw binary form of SNMP packet. Good for debugging.}
    property Buffer: AnsiString read FBuffer write FBuffer;

    {:After SNMP operation hold IP address of remote side.}
    property HostIP: AnsiString read FHostIP;

    {:Data object contains SNMP query.}
    property Query: TSNMPRec read FQuery;

    {:Data object contains SNMP reply.}
    property Reply: TSNMPRec read FReply;

    {:Socket object used for TCP/IP operation. Good for seting OnStatus hook, etc.}
    property Sock: TUDPBlockSocket read FSock;
  end;

{:A very useful function and example of its use would be found in the TSNMPSend
 object. It implements basic GET method of the SNMP protocol. The MIB value is
 located in the "OID" variable, and is sent to the requested "SNMPHost" with
 the proper "Community" access identifier. Upon a successful retrieval, "Value"
 will contain the information requested. If the SNMP operation is successful,
 the result returns @true.}
function SNMPGet(const OID, Community, SNMPHost: AnsiString; var Value: AnsiString): Boolean;

{:This is useful function and example of use TSNMPSend object. It implements
 the basic SET method of the SNMP protocol. If the SNMP operation is successful,
 the result is @true. "Value" is value of MIB Oid for "SNMPHost" with "Community"
 access identifier. You must specify "ValueType" too.}
function SNMPSet(const OID, Community, SNMPHost, Value: AnsiString; ValueType: Integer): Boolean;

{:A very useful function and example of its use would be found in the TSNMPSend
 object. It implements basic GETNEXT method of the SNMP protocol. The MIB value
 is located in the "OID" variable, and is sent to the requested "SNMPHost" with
 the proper "Community" access identifier. Upon a successful retrieval, "Value"
 will contain the information requested. If the SNMP operation is successful,
 the result returns @true.}
function SNMPGetNext(var OID: AnsiString; const Community, SNMPHost: AnsiString; var Value: AnsiString): Boolean;

{:A very useful function and example of its use would be found in the TSNMPSend
 object. It implements basic read of SNMP MIB tables. As BaseOID you must
 specify basic MIB OID of requested table (base IOD is OID without row and
 column specificator!)
 Table is readed into stringlist, where each string is comma delimited string.

 Warning: this function is not have best performance. For better performance
 you must write your own function. best performace you can get by knowledge
 of structuture of table and by more then one MIB on one query. }
function SNMPGetTable(const BaseOID, Community, SNMPHost: AnsiString; const Value: TStrings): Boolean;

{:A very useful function and example of its use would be found in the TSNMPSend
 object. It implements basic read of SNMP MIB table element. As BaseOID you must
 specify basic MIB OID of requested table (base IOD is OID without row and
 column specificator!)
 As next you must specify identificator of row and column for specify of needed
 field of table.}
function SNMPGetTableElement(const BaseOID, RowID, ColID, Community, SNMPHost: AnsiString; var Value: AnsiString): Boolean;

{:A very useful function and example of its use would be found in the TSNMPSend
 object. It implements a TRAPv1 to send with all data in the parameters.}
function SendTrap(const Dest, Source, Enterprise, Community: AnsiString;
  Generic, Specific, Seconds: Integer; const MIBName, MIBValue: AnsiString;
  MIBtype: Integer): Integer;

{:A very useful function and example of its use would be found in the TSNMPSend
 object. It receives a TRAPv1 and returns all the data that comes with it.}
function RecvTrap(var Dest, Source, Enterprise, Community: AnsiString;
  var Generic, Specific, Seconds: Integer; const MIBName,
  MIBValue: TStringList): Integer;

implementation

{==============================================================================}

constructor TSNMPRec.Create;
begin
  inherited Create;
  FSNMPMibList := TList.Create;
  Clear;
  FAuthMode := AuthMD5;
  FPassword := '';
  FPrivMode := PrivDES;
  FPrivPassword := '';
  FID := 1;
  FMaxSize := 1472;
end;

destructor TSNMPRec.Destroy;
var
  i: Integer;
begin
  for i := 0 to FSNMPMibList.Count - 1 do
    TSNMPMib(FSNMPMibList[i]).Free;
  FSNMPMibList.Clear;
  FSNMPMibList.Free;
  inherited Destroy;
end;

function TSNMPRec.Pass2Key(const Value: AnsiString): AnsiString;
var
  key: AnsiString;
begin
  case FAuthMode of
    AuthMD5:
      begin
        key := MD5LongHash(Value, 1048576);
        Result := MD5(key + FAuthEngineID + key);
      end;
    AuthSHA1:
      begin
        key := SHA1LongHash(Value, 1048576);
        Result := SHA1(key + FAuthEngineID + key);
      end;
  else
    Result := '';
  end;
end;

function TSNMPRec.DecryptPDU(const value: AnsiString): AnsiString;
var
  des: TSynaDes;
  des3: TSyna3Des;
  aes: TSynaAes;
  s: string;
begin
  FPrivKey := '';
  if FFlags <> AuthPriv then
    Result := value
  else
  begin
    case FPrivMode of
      Priv3DES:
        begin
          FPrivKey := Pass2Key(FPrivPassword);
          FPrivKey := FPrivKey + Pass2Key(FPrivKey);
          des3 := TSyna3Des.Create(PadString(FPrivKey, 24, #0));
          try
            s := PadString(FPrivKey, 32, #0);
            delete(s, 1, 24);
            des3.SetIV(xorstring(s, FPrivSalt));
            s := des3.DecryptCBC(value);
            Result := s;
          finally
            des3.free;
          end;
        end;
      PrivAES:
        begin
          FPrivKey := Pass2Key(FPrivPassword);
          aes := TSynaAes.Create(PadString(FPrivKey, 16, #0));
          try
            s := CodeLongInt(FAuthEngineBoots) + CodeLongInt(FAuthEngineTime) + FPrivSalt;
            aes.SetIV(s);
            s := aes.DecryptCFBblock(value);
            Result := s;
          finally
            aes.free;
          end;
        end;
    else //PrivDES as default
      begin
        FPrivKey := Pass2Key(FPrivPassword);
        des := TSynaDes.Create(PadString(FPrivKey, 8, #0));
        try
          s := PadString(FPrivKey, 16, #0);
          delete(s, 1, 8);
          des.SetIV(xorstring(s, FPrivSalt));
          s := des.DecryptCBC(value);
          Result := s;
        finally
          des.free;
        end;
      end;
    end;
  end;
end;

function TSNMPRec.DecodeBuf(Buffer: AnsiString): Boolean;
var
  Pos: Integer;
  EndPos: Integer;
  sm, sv: AnsiString;
  Svt: Integer;
  s: AnsiString;
  Spos: integer;
  x: Byte;
begin
  Clear;
  Result := False;
  if Length(Buffer) < 2 then
    Exit;
  if (Ord(Buffer[1]) and $20) = 0 then
    Exit;
  Pos := 2;
  EndPos := ASNDecLen(Pos, Buffer);
  if Length(Buffer) < (EndPos + 2) then
    Exit;
  Self.FVersion := StrToIntDef(ASNItem(Pos, Buffer, Svt), 0);

  if FVersion = 3 then
  begin
    ASNItem(Pos, Buffer, Svt);  //header data seq
    ASNItem(Pos, Buffer, Svt);  //ID
    FMaxSize := StrToIntDef(ASNItem(Pos, Buffer, Svt), 0);
    s := ASNItem(Pos, Buffer, Svt);
    x := 0;
    if s <> '' then
      x := Ord(s[1]);
    FFlagReportable := (x and 4) > 0;
    x := x and 3;
    case x of
      1:
        FFlags := AuthNoPriv;
      3:
        FFlags := AuthPriv;
    else
      FFlags := NoAuthNoPriv;
    end;

    x := StrToIntDef(ASNItem(Pos, Buffer, Svt), 0);
    s := ASNItem(Pos, Buffer, Svt); //SecurityParameters
    //if SecurityModel is USM, then try to decode SecurityParameters
    if (x = 3) and (s <> '') then
    begin
      spos := 1;
      ASNItem(SPos, s, Svt);
      FAuthEngineID := ASNItem(SPos, s, Svt);
      FAuthEngineBoots := StrToIntDef(ASNItem(SPos, s, Svt), 0);
      FAuthEngineTime := StrToIntDef(ASNItem(SPos, s, Svt), 0);
      FAuthEngineTimeStamp := GetTick;
      FUserName := ASNItem(SPos, s, Svt);
      FAuthKey := ASNItem(SPos, s, Svt);
      FPrivSalt := ASNItem(SPos, s, Svt);
    end;
    //scopedPDU
    if FFlags = AuthPriv then
    begin
      x := Pos;
      s := ASNItem(Pos, Buffer, Svt);
      if Svt <> ASN1_OCTSTR then
        exit;
      s := DecryptPDU(s);
      //replace encoded content by decoded version and continue
      Buffer := copy(Buffer, 1, x - 1);
      Buffer := Buffer + s;
      Pos := x;
      if length(Buffer) < EndPos then
        EndPos := length(buffer);
    end;
    ASNItem(Pos, Buffer, Svt); //skip sequence mark
    FContextEngineID := ASNItem(Pos, Buffer, Svt);
    FContextName := ASNItem(Pos, Buffer, Svt);
  end
  else
  begin
    //old packet
    Self.FCommunity := ASNItem(Pos, Buffer, Svt);
  end;

  ASNItem(Pos, Buffer, Svt);
  Self.FPDUType := Svt;
  if Self.FPDUType = PDUTrap then
  begin
    FOldTrapEnterprise := ASNItem(Pos, Buffer, Svt);
    FOldTrapHost := ASNItem(Pos, Buffer, Svt);
    FOldTrapGen := StrToIntDef(ASNItem(Pos, Buffer, Svt), 0);
    FOldTrapSpec := StrToIntDef(ASNItem(Pos, Buffer, Svt), 0);
    FOldTrapTimeTicks := StrToIntDef(ASNItem(Pos, Buffer, Svt), 0);
  end
  else
  begin
    Self.FID := StrToIntDef(ASNItem(Pos, Buffer, Svt), 0);
    Self.FErrorStatus := StrToIntDef(ASNItem(Pos, Buffer, Svt), 0);
    Self.FErrorIndex := StrToIntDef(ASNItem(Pos, Buffer, Svt), 0);
  end;
  ASNItem(Pos, Buffer, Svt);
  while Pos < EndPos do
  begin
    ASNItem(Pos, Buffer, Svt);
    Sm := ASNItem(Pos, Buffer, Svt);
    Sv := ASNItem(Pos, Buffer, Svt);
    if sm <> '' then
      Self.MIBAdd(sm, sv, Svt);
  end;
  Result := True;
end;

function TSNMPRec.EncryptPDU(const value: AnsiString): AnsiString;
var
  des: TSynaDes;
  des3: TSyna3Des;
  aes: TSynaAes;
  s: string;
  x: integer;
begin
  FPrivKey := '';
  if FFlags <> AuthPriv then
    Result := Value
  else
  begin
    case FPrivMode of
      Priv3DES:
        begin
          FPrivKey := Pass2Key(FPrivPassword);
          FPrivKey := FPrivKey + Pass2Key(FPrivKey);
          des3 := TSyna3Des.Create(PadString(FPrivKey, 24, #0));
          try
            s := PadString(FPrivKey, 32, #0);
            delete(s, 1, 24);
            FPrivSalt := CodeLongInt(FAuthEngineBoots) + CodeLongInt(FPrivSaltCounter);
            inc(FPrivSaltCounter);
            s := xorstring(s, FPrivSalt);
            des3.SetIV(s);
            x := length(value) mod 8;
            x := 8 - x;
            if x = 8 then
              x := 0;
            s := des3.EncryptCBC(value + Stringofchar(#0, x));
            Result := ASNObject(s, ASN1_OCTSTR);
          finally
            des3.free;
          end;
        end;
      PrivAES:
        begin
          FPrivKey := Pass2Key(FPrivPassword);
          aes := TSynaAes.Create(PadString(FPrivKey, 16, #0));
          try
            FPrivSalt := CodeLongInt(0) + CodeLongInt(FPrivSaltCounter);
            inc(FPrivSaltCounter);
            s := CodeLongInt(FAuthEngineBoots) + CodeLongInt(FAuthEngineTime) + FPrivSalt;
            aes.SetIV(s);
            s := aes.EncryptCFBblock(value);
            Result := ASNObject(s, ASN1_OCTSTR);
          finally
            aes.free;
          end;
        end;
    else //PrivDES as default
      begin
        FPrivKey := Pass2Key(FPrivPassword);
        des := TSynaDes.Create(PadString(FPrivKey, 8, #0));
        try
          s := PadString(FPrivKey, 16, #0);
          delete(s, 1, 8);
          FPrivSalt := CodeLongInt(FAuthEngineBoots) + CodeLongInt(FPrivSaltCounter);
          inc(FPrivSaltCounter);
          s := xorstring(s, FPrivSalt);
          des.SetIV(s);
          x := length(value) mod 8;
          x := 8 - x;
          if x = 8 then
            x := 0;
          s := des.EncryptCBC(value + Stringofchar(#0, x));
          Result := ASNObject(s, ASN1_OCTSTR);
        finally
          des.free;
        end;
      end;
    end;
  end;
end;

function TSNMPRec.EncodeBuf: AnsiString;
var
  s: AnsiString;
  SNMPMib: TSNMPMib;
  n: Integer;
  pdu, head, auth, authbeg: AnsiString;
  x: Byte;
begin
  pdu := '';
  for n := 0 to FSNMPMibList.Count - 1 do
  begin
    SNMPMib := TSNMPMib(FSNMPMibList[n]);
    case SNMPMib.ValueType of
      ASN1_INT:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject(ASNEncInt(StrToIntDef(SNMPMib.Value, 0)), SNMPMib.ValueType);
      ASN1_COUNTER, ASN1_GAUGE, ASN1_TIMETICKS:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject(ASNEncUInt(StrToIntDef(SNMPMib.Value, 0)), SNMPMib.ValueType);
      ASN1_OBJID:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject(MibToID(SNMPMib.Value), SNMPMib.ValueType);
      ASN1_IPADDR:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject(IPToID(SNMPMib.Value), SNMPMib.ValueType);
      ASN1_NULL:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject('', ASN1_NULL);
    else
      s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
        ASNObject(SNMPMib.Value, SNMPMib.ValueType);
    end;
    pdu := pdu + ASNObject(s, ASN1_SEQ);
  end;
  pdu := ASNObject(pdu, ASN1_SEQ);

  if Self.FPDUType = PDUTrap then
    pdu := ASNObject(MibToID(FOldTrapEnterprise), ASN1_OBJID) +
      ASNObject(IPToID(FOldTrapHost), ASN1_IPADDR) +
      ASNObject(ASNEncInt(FOldTrapGen), ASN1_INT) +
      ASNObject(ASNEncInt(FOldTrapSpec), ASN1_INT) +
      ASNObject(ASNEncUInt(FOldTrapTimeTicks), ASN1_TIMETICKS) +
      pdu
  else
    pdu := ASNObject(ASNEncInt(Self.FID), ASN1_INT) +
      ASNObject(ASNEncInt(Self.FErrorStatus), ASN1_INT) +
      ASNObject(ASNEncInt(Self.FErrorIndex), ASN1_INT) +
      pdu;
  pdu := ASNObject(pdu, Self.FPDUType);

  if FVersion = 3 then
  begin
    if FContextEngineID = '' then
      FContextEngineID := FAuthEngineID;
    //complete PDUv3...
    pdu := ASNObject(FContextEngineID, ASN1_OCTSTR)
      + ASNObject(FContextName, ASN1_OCTSTR)
      + pdu;
    pdu := ASNObject(pdu, ASN1_SEQ);
    //encrypt PDU if Priv mode is enabled
    pdu := EncryptPDU(pdu);

    //prepare flags
    case FFlags of
      AuthNoPriv:
        x := 1;
      AuthPriv:
        x := 3;
    else
      x := 0;
    end;
    if FFlagReportable then
      x := x or 4;
    head := ASNObject(ASNEncInt(Self.FVersion), ASN1_INT);
    s := ASNObject(ASNEncInt(FID), ASN1_INT)
      + ASNObject(ASNEncInt(FMaxSize), ASN1_INT)
      + ASNObject(AnsiChar(x), ASN1_OCTSTR)
    //encode security model USM
      + ASNObject(ASNEncInt(3), ASN1_INT);
    head := head + ASNObject(s, ASN1_SEQ);

    //compute engine time difference
    if FAuthEngineTimeStamp = 0 then //out of sync
      x := 0
    else
      x := TickDelta(FAuthEngineTimeStamp, GetTick) div 1000;

    authbeg := ASNObject(FAuthEngineID, ASN1_OCTSTR)
      + ASNObject(ASNEncInt(FAuthEngineBoots), ASN1_INT)
      + ASNObject(ASNEncInt(FAuthEngineTime + x), ASN1_INT)
      + ASNObject(FUserName, ASN1_OCTSTR);


    case FFlags of
      AuthNoPriv,
      AuthPriv:
        begin
          s := authbeg + ASNObject(StringOfChar(#0, 12), ASN1_OCTSTR)
             + ASNObject(FPrivSalt, ASN1_OCTSTR);
          s := ASNObject(s, ASN1_SEQ);
          s := head + ASNObject(s, ASN1_OCTSTR);
          s := ASNObject(s + pdu, ASN1_SEQ);
          //in s is entire packet without auth info...
          case FAuthMode of
            AuthMD5:
              begin
                s := HMAC_MD5(s, Pass2Key(FPassword) + StringOfChar(#0, 48));
                //strip to HMAC-MD5-96
                delete(s, 13, 4);
              end;
            AuthSHA1:
              begin
                s := HMAC_SHA1(s, Pass2Key(FPassword) + StringOfChar(#0, 44));
                //strip to HMAC-SHA-96
                delete(s, 13, 8);
              end;
          else
            s := '';
          end;
          FAuthKey := s;
        end;
    end;

    auth := authbeg + ASNObject(FAuthKey, ASN1_OCTSTR)
     + ASNObject(FPrivSalt, ASN1_OCTSTR);
    auth := ASNObject(auth, ASN1_SEQ);

    head := head + ASNObject(auth, ASN1_OCTSTR);
    Result := ASNObject(head + pdu, ASN1_SEQ);
  end
  else
  begin
    head := ASNObject(ASNEncInt(Self.FVersion), ASN1_INT) +
      ASNObject(Self.FCommunity, ASN1_OCTSTR);
    Result := ASNObject(head + pdu, ASN1_SEQ);
  end;
  inc(self.FID);
end;

procedure TSNMPRec.Clear;
var
  i: Integer;
begin
  FVersion := SNMP_V1;
  FCommunity := 'public';
  FUserName := '';
  FPDUType := 0;
  FErrorStatus := 0;
  FErrorIndex := 0;
  for i := 0 to FSNMPMibList.Count - 1 do
    TSNMPMib(FSNMPMibList[i]).Free;
  FSNMPMibList.Clear;
  FOldTrapEnterprise := '';
  FOldTrapHost := '';
  FOldTrapGen := 0;
  FOldTrapSpec := 0;
  FOldTrapTimeTicks := 0;
  FFlags := NoAuthNoPriv;
  FFlagReportable := false;
  FContextEngineID := '';
  FContextName := '';
  FAuthEngineID := '';
  FAuthEngineBoots := 0;
  FAuthEngineTime := 0;
  FAuthEngineTimeStamp := 0;
  FAuthKey := '';
  FPrivKey := '';
  FPrivSalt := '';
  FPrivSaltCounter := random(maxint);
end;

procedure TSNMPRec.MIBAdd(const MIB, Value: AnsiString; ValueType: Integer);
var
  SNMPMib: TSNMPMib;
begin
  SNMPMib := TSNMPMib.Create;
  SNMPMib.OID := MIB;
  SNMPMib.Value := Value;
  SNMPMib.ValueType := ValueType;
  FSNMPMibList.Add(SNMPMib);
end;

procedure TSNMPRec.MIBDelete(Index: Integer);
begin
  if (Index >= 0) and (Index < MIBCount) then
  begin
    TSNMPMib(FSNMPMibList[Index]).Free;
    FSNMPMibList.Delete(Index);
  end;
end;

function TSNMPRec.MIBCount: integer;
begin
  Result := FSNMPMibList.Count;
end;

function TSNMPRec.MIBByIndex(Index: Integer): TSNMPMib;
begin
  Result := nil;
  if (Index >= 0) and (Index < MIBCount) then
    Result := TSNMPMib(FSNMPMibList[Index]);
end;

function TSNMPRec.MIBGet(const MIB: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to MIBCount - 1 do
  begin
    if ((TSNMPMib(FSNMPMibList[i])).OID = MIB) then
    begin
      Result := (TSNMPMib(FSNMPMibList[i])).Value;
      Break;
    end;
  end;
end;

{==============================================================================}

constructor TSNMPSend.Create;
begin
  inherited Create;
  FQuery := TSNMPRec.Create;
  FReply := TSNMPRec.Create;
  FQuery.Clear;
  FReply.Clear;
  FSock := TUDPBlockSocket.Create;
  FSock.Owner := self;
  FTimeout := 5000;
  FTargetPort := cSnmpProtocol;
  FHostIP := '';
end;

destructor TSNMPSend.Destroy;
begin
  FSock.Free;
  FReply.Free;
  FQuery.Free;
  inherited Destroy;
end;

function TSNMPSend.InternalSendSnmp(const Value: TSNMPRec): Boolean;
begin
  FBuffer := Value.EncodeBuf;
  FSock.SendString(FBuffer);
  Result := FSock.LastError = 0;
end;

function TSNMPSend.InternalRecvSnmp(const Value: TSNMPRec): Boolean;
begin
  Result := False;
  FReply.Clear;
  FHostIP := cAnyHost;
  FBuffer := FSock.RecvPacket(FTimeout);
  if FSock.LastError = 0 then
  begin
    FHostIP := FSock.GetRemoteSinIP;
    Result := Value.DecodeBuf(FBuffer);
  end;
end;

function TSNMPSend.InternalSendRequest(const QValue, RValue: TSNMPRec): Boolean;
begin
  Result := False;
  RValue.AuthMode := QValue.AuthMode;
  RValue.Password := QValue.Password;
  RValue.PrivMode := QValue.PrivMode;
  RValue.PrivPassword := QValue.PrivPassword;
  FSock.Bind(FIPInterface, cAnyPort);
  FSock.Connect(FTargetHost, FTargetPort);
  if InternalSendSnmp(QValue) then
    Result := InternalRecvSnmp(RValue);
end;

function TSNMPSend.SendRequest: Boolean;
var
  sync: TV3Sync;
begin
  Result := False;
  if FQuery.FVersion = 3 then
  begin
    sync := GetV3Sync;
    FQuery.AuthEngineBoots := Sync.EngineBoots;
    FQuery.AuthEngineTime := Sync.EngineTime;
    FQuery.AuthEngineTimeStamp := Sync.EngineStamp;
    FQuery.AuthEngineID := Sync.EngineID;
  end;
  Result := InternalSendRequest(FQuery, FReply);
end;

function TSNMPSend.SendTrap: Boolean;
begin
  FSock.Bind(FIPInterface, cAnyPort);
  FSock.Connect(FTargetHost, FTargetPort);
  Result := InternalSendSnmp(FQuery);
end;

function TSNMPSend.RecvTrap: Boolean;
begin
  FSock.Bind(FIPInterface, FTargetPort);
  Result := InternalRecvSnmp(FReply);
end;

function TSNMPSend.DoIt: Boolean;
begin
  Result := SendRequest;
end;

function TSNMPSend.GetV3EngineID: AnsiString;
var
  DisQuery: TSNMPRec;
begin
  Result := '';
  DisQuery := TSNMPRec.Create;
  try
    DisQuery.Version := 3;
    DisQuery.UserName := '';
    DisQuery.FlagReportable := True;
    DisQuery.PDUType := PDUGetRequest;
    if InternalSendRequest(DisQuery, FReply) then
      Result := FReply.FAuthEngineID;
  finally
    DisQuery.Free;
  end;
end;

function TSNMPSend.GetV3Sync: TV3Sync;
var
  SyncQuery: TSNMPRec;
begin
  Result.EngineID := GetV3EngineID;
  Result.EngineBoots := FReply.AuthEngineBoots;
  Result.EngineTime := FReply.AuthEngineTime;
  Result.EngineStamp := FReply.AuthEngineTimeStamp;
  if Result.EngineTime = 0 then
  begin
    //still not have sync...
    SyncQuery := TSNMPRec.Create;
    try
      SyncQuery.Version := 3;
      SyncQuery.UserName := FQuery.UserName;
      SyncQuery.Password := FQuery.Password;
      SyncQuery.FlagReportable := True;
      SyncQuery.Flags := FQuery.Flags;
      SyncQuery.AuthMode := FQuery.AuthMode;
      SyncQuery.PrivMode := FQuery.PrivMode;
      SyncQuery.PrivPassword := FQuery.PrivPassword;
      SyncQuery.PDUType := PDUGetRequest;
      SyncQuery.AuthEngineID := FReply.FAuthEngineID;
      if InternalSendRequest(SyncQuery, FReply) then
      begin
        Result.EngineBoots := FReply.AuthEngineBoots;
        Result.EngineTime := FReply.AuthEngineTime;
        Result.EngineStamp := FReply.AuthEngineTimeStamp;
      end;
    finally
      SyncQuery.Free;
    end;
  end;
end;

{==============================================================================}

function SNMPGet(const OID, Community, SNMPHost: AnsiString; var Value: AnsiString): Boolean;
var
  SNMPSend: TSNMPSend;
begin
  SNMPSend := TSNMPSend.Create;
  try
    SNMPSend.Query.Clear;
    SNMPSend.Query.Community := Community;
    SNMPSend.Query.PDUType := PDUGetRequest;
    SNMPSend.Query.MIBAdd(OID, '', ASN1_NULL);
    SNMPSend.TargetHost := SNMPHost;
    Result := SNMPSend.SendRequest;
    Value := '';
    if Result then
      Value := SNMPSend.Reply.MIBGet(OID);
  finally
    SNMPSend.Free;
  end;
end;

function SNMPSet(const OID, Community, SNMPHost, Value: AnsiString; ValueType: Integer): Boolean;
var
  SNMPSend: TSNMPSend;
begin
  SNMPSend := TSNMPSend.Create;
  try
    SNMPSend.Query.Clear;
    SNMPSend.Query.Community := Community;
    SNMPSend.Query.PDUType := PDUSetRequest;
    SNMPSend.Query.MIBAdd(OID, Value, ValueType);
    SNMPSend.TargetHost := SNMPHost;
    Result := SNMPSend.Sendrequest = True;
  finally
    SNMPSend.Free;
  end;
end;

function InternalGetNext(const SNMPSend: TSNMPSend; var OID: AnsiString;
  const Community: AnsiString; var Value: AnsiString): Boolean;
begin
  SNMPSend.Query.Clear;
  SNMPSend.Query.ID := SNMPSend.Query.ID + 1;
  SNMPSend.Query.Community := Community;
  SNMPSend.Query.PDUType := PDUGetNextRequest;
  SNMPSend.Query.MIBAdd(OID, '', ASN1_NULL);
  Result := SNMPSend.Sendrequest;
  Value := '';
  if Result then
    if SNMPSend.Reply.SNMPMibList.Count > 0 then
    begin
      OID := TSNMPMib(SNMPSend.Reply.SNMPMibList[0]).OID;
      Value := TSNMPMib(SNMPSend.Reply.SNMPMibList[0]).Value;
    end;
end;

function SNMPGetNext(var OID: AnsiString; const Community, SNMPHost: AnsiString; var Value: AnsiString): Boolean;
var
  SNMPSend: TSNMPSend;
begin
  SNMPSend := TSNMPSend.Create;
  try
    SNMPSend.TargetHost := SNMPHost;
    Result := InternalGetNext(SNMPSend, OID, Community, Value);
  finally
    SNMPSend.Free;
  end;
end;

function SNMPGetTable(const BaseOID, Community, SNMPHost: AnsiString; const Value: TStrings): Boolean;
var
  OID: AnsiString;
  s: AnsiString;
  col,row: String;
  x: integer;
  SNMPSend: TSNMPSend;
  RowList: TStringList;
begin
  Value.Clear;
  SNMPSend := TSNMPSend.Create;
  RowList := TStringList.Create;
  try
    SNMPSend.TargetHost := SNMPHost;
    OID := BaseOID;
    repeat
      Result := InternalGetNext(SNMPSend, OID, Community, s);
      if Pos(BaseOID, OID) <> 1 then
          break;
      row := separateright(oid, baseoid + '.');
      col := fetch(row, '.');

      if IsBinaryString(s) then
        s := StrToHex(s);
      x := RowList.indexOf(Row);
      if x < 0 then
      begin
        x := RowList.add(Row);
        Value.Add('');
      end;
      if (Value[x] <> '') then
        Value[x] := Value[x] + ',';
      Value[x] := Value[x] + AnsiQuotedStr(s, '"');
    until not result;
  finally
    SNMPSend.Free;
    RowList.Free;
  end;
end;

function SNMPGetTableElement(const BaseOID, RowID, ColID, Community, SNMPHost: AnsiString; var Value: AnsiString): Boolean;
var
  s: AnsiString;
begin
  s := BaseOID + '.' + ColID + '.' + RowID;
  Result := SnmpGet(s, Community, SNMPHost, Value);
end;

function SendTrap(const Dest, Source, Enterprise, Community: AnsiString;
  Generic, Specific, Seconds: Integer; const MIBName, MIBValue: AnsiString;
  MIBtype: Integer): Integer;
var
  SNMPSend: TSNMPSend;
begin
  SNMPSend := TSNMPSend.Create;
  try
    SNMPSend.TargetHost := Dest;
    SNMPSend.TargetPort := cSnmpTrapProtocol;
    SNMPSend.Query.Community := Community;
    SNMPSend.Query.Version := SNMP_V1;
    SNMPSend.Query.PDUType := PDUTrap;
    SNMPSend.Query.OldTrapHost := Source;
    SNMPSend.Query.OldTrapEnterprise := Enterprise;
    SNMPSend.Query.OldTrapGen := Generic;
    SNMPSend.Query.OldTrapSpec := Specific;
    SNMPSend.Query.OldTrapTimeTicks := Seconds;
    SNMPSend.Query.MIBAdd(MIBName, MIBValue, MIBType);
    Result := Ord(SNMPSend.SendTrap);
  finally
    SNMPSend.Free;
  end;
end;

function RecvTrap(var Dest, Source, Enterprise, Community: AnsiString;
  var Generic, Specific, Seconds: Integer;
  const MIBName, MIBValue: TStringList): Integer;
var
  SNMPSend: TSNMPSend;
  i: Integer;
begin
  SNMPSend := TSNMPSend.Create;
  try
    Result := 0;
    SNMPSend.TargetPort := cSnmpTrapProtocol;
    if SNMPSend.RecvTrap then
    begin
      Result := 1;
      Dest := SNMPSend.HostIP;
      Community := SNMPSend.Reply.Community;
      Source := SNMPSend.Reply.OldTrapHost;
      Enterprise := SNMPSend.Reply.OldTrapEnterprise;
      Generic := SNMPSend.Reply.OldTrapGen;
      Specific := SNMPSend.Reply.OldTrapSpec;
      Seconds := SNMPSend.Reply.OldTrapTimeTicks;
      MIBName.Clear;
      MIBValue.Clear;
      for i := 0 to SNMPSend.Reply.SNMPMibList.Count - 1 do
      begin
        MIBName.Add(TSNMPMib(SNMPSend.Reply.SNMPMibList[i]).OID);
        MIBValue.Add(TSNMPMib(SNMPSend.Reply.SNMPMibList[i]).Value);
      end;
    end;
  finally
    SNMPSend.Free;
  end;
end;


end.


