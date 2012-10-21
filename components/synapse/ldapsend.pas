{==============================================================================|
| Project : Ararat Synapse                                       | 001.007.000 |
|==============================================================================|
| Content: LDAP client                                                         |
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

{:@abstract(LDAP client)

Used RFC: RFC-2251, RFC-2254, RFC-2829, RFC-2830
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit ldapsend;

interface

uses
  SysUtils, Classes,
  blcksock, synautil, asn1util, synacode;

const
  cLDAPProtocol = '389';

  LDAP_ASN1_BIND_REQUEST = $60;
  LDAP_ASN1_BIND_RESPONSE = $61;
  LDAP_ASN1_UNBIND_REQUEST = $42;
  LDAP_ASN1_SEARCH_REQUEST = $63;
  LDAP_ASN1_SEARCH_ENTRY = $64;
  LDAP_ASN1_SEARCH_DONE = $65;
  LDAP_ASN1_SEARCH_REFERENCE = $73;
  LDAP_ASN1_MODIFY_REQUEST = $66;
  LDAP_ASN1_MODIFY_RESPONSE = $67;
  LDAP_ASN1_ADD_REQUEST = $68;
  LDAP_ASN1_ADD_RESPONSE = $69;
  LDAP_ASN1_DEL_REQUEST = $4A;
  LDAP_ASN1_DEL_RESPONSE = $6B;
  LDAP_ASN1_MODIFYDN_REQUEST = $6C;
  LDAP_ASN1_MODIFYDN_RESPONSE = $6D;
  LDAP_ASN1_COMPARE_REQUEST = $6E;
  LDAP_ASN1_COMPARE_RESPONSE = $6F;
  LDAP_ASN1_ABANDON_REQUEST = $70;
  LDAP_ASN1_EXT_REQUEST = $77;
  LDAP_ASN1_EXT_RESPONSE = $78;


type

  {:@abstract(LDAP attribute with list of their values)
   This class holding name of LDAP attribute and list of their values. This is
   descendant of TStringList class enhanced by some new properties.}
  TLDAPAttribute = class(TStringList)
  private
    FAttributeName: AnsiString;
    FIsBinary: Boolean;
  protected
    function Get(Index: integer): string; override;
    procedure Put(Index: integer; const Value: string); override;
    procedure SetAttributeName(Value: AnsiString);
  published
    {:Name of LDAP attribute.}
    property AttributeName: AnsiString read FAttributeName Write SetAttributeName;
    {:Return @true when attribute contains binary data.}
    property IsBinary: Boolean read FIsBinary;
  end;

  {:@abstract(List of @link(TLDAPAttribute))
   This object can hold list of TLDAPAttribute objects.}
  TLDAPAttributeList = class(TObject)
  private
    FAttributeList: TList;
    function GetAttribute(Index: integer): TLDAPAttribute;
  public
    constructor Create;
    destructor Destroy; override;
    {:Clear list.}
    procedure Clear;
    {:Return count of TLDAPAttribute objects in list.}
    function Count: integer;
    {:Add new TLDAPAttribute object to list.}
    function Add: TLDAPAttribute;
    {:Delete one TLDAPAttribute object from list.}
    procedure Del(Index: integer);
    {:Find and return attribute with requested name. Returns nil if not found.}
    function Find(AttributeName: AnsiString): TLDAPAttribute;
    {:Find and return attribute value with requested name. Returns empty string if not found.}
    function Get(AttributeName: AnsiString): string;
    {:List of TLDAPAttribute objects.}
    property Items[Index: Integer]: TLDAPAttribute read GetAttribute; default;
  end;

  {:@abstract(LDAP result object)
   This object can hold LDAP object. (their name and all their attributes with
   values)}
  TLDAPResult = class(TObject)
  private
    FObjectName: AnsiString;
    FAttributes: TLDAPAttributeList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    {:Name of this LDAP object.}
    property ObjectName: AnsiString read FObjectName write FObjectName;
    {:Here is list of object attributes.}
    property Attributes: TLDAPAttributeList read FAttributes;
  end;

  {:@abstract(List of LDAP result objects)
   This object can hold list of LDAP objects. (for example result of LDAP SEARCH.)}
  TLDAPResultList = class(TObject)
  private
    FResultList: TList;
    function GetResult(Index: integer): TLDAPResult;
  public
    constructor Create;
    destructor Destroy; override;
    {:Clear all TLDAPResult objects in list.}
    procedure Clear;
    {:Return count of TLDAPResult objects in list.}
    function Count: integer;
    {:Create and add new TLDAPResult object to list.}
    function Add: TLDAPResult;
    {:List of TLDAPResult objects.}
    property Items[Index: Integer]: TLDAPResult read GetResult; default;
  end;

  {:Define possible operations for LDAP MODIFY operations.}
  TLDAPModifyOp = (
    MO_Add,
    MO_Delete,
    MO_Replace
  );

  {:Specify possible values for search scope.}
  TLDAPSearchScope = (
    SS_BaseObject,
    SS_SingleLevel,
    SS_WholeSubtree
  );

  {:Specify possible values about alias dereferencing.}
  TLDAPSearchAliases = (
    SA_NeverDeref,
    SA_InSearching,
    SA_FindingBaseObj,
    SA_Always
  );

  {:@abstract(Implementation of LDAP client)
   (version 2 and 3)

   Note: Are you missing properties for setting Username and Password? Look to
   parent @link(TSynaClient) object!

   Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}
  TLDAPSend = class(TSynaClient)
  private
    FSock: TTCPBlockSocket;
    FResultCode: Integer;
    FResultString: AnsiString;
    FFullResult: AnsiString;
    FAutoTLS: Boolean;
    FFullSSL: Boolean;
    FSeq: integer;
    FResponseCode: integer;
    FResponseDN: AnsiString;
    FReferals: TStringList;
    FVersion: integer;
    FSearchScope: TLDAPSearchScope;
    FSearchAliases: TLDAPSearchAliases;
    FSearchSizeLimit: integer;
    FSearchTimeLimit: integer;
    FSearchResult: TLDAPResultList;
    FExtName: AnsiString;
    FExtValue: AnsiString;
    function Connect: Boolean;
    function BuildPacket(const Value: AnsiString): AnsiString;
    function ReceiveResponse: AnsiString;
    function DecodeResponse(const Value: AnsiString): AnsiString;
    function LdapSasl(Value: AnsiString): AnsiString;
    function TranslateFilter(Value: AnsiString): AnsiString;
    function GetErrorString(Value: integer): AnsiString;
  public
    constructor Create;
    destructor Destroy; override;

    {:Try to connect to LDAP server and start secure channel, when it is required.}
    function Login: Boolean;

    {:Try to bind to LDAP server with @link(TSynaClient.Username) and
     @link(TSynaClient.Password). If this is empty strings, then it do annonymous
     Bind. When you not call Bind on LDAPv3, then is automaticly used anonymous
     mode.

     This method using plaintext transport of password! It is not secure!}
    function Bind: Boolean;

    {:Try to bind to LDAP server with @link(TSynaClient.Username) and
     @link(TSynaClient.Password). If this is empty strings, then it do annonymous
     Bind. When you not call Bind on LDAPv3, then is automaticly used anonymous
     mode.

     This method using SASL with DIGEST-MD5 method for secure transfer of your
     password.}
    function BindSasl: Boolean;

    {:Close connection to LDAP server.}
    function Logout: Boolean;

    {:Modify content of LDAP attribute on this object.}
    function Modify(obj: AnsiString; Op: TLDAPModifyOp; const Value: TLDAPAttribute): Boolean;

    {:Add list of attributes to specified object.}
    function Add(obj: AnsiString; const Value: TLDAPAttributeList): Boolean;

    {:Delete this LDAP object from server.}
    function Delete(obj: AnsiString): Boolean;

    {:Modify object name of this LDAP object.}
    function ModifyDN(obj, newRDN, newSuperior: AnsiString; DeleteoldRDN: Boolean): Boolean;

    {:Try to compare Attribute value with this LDAP object.}
    function Compare(obj, AttributeValue: AnsiString): Boolean;

    {:Search LDAP base for LDAP objects by Filter.}
    function Search(obj: AnsiString; TypesOnly: Boolean; Filter: AnsiString;
      const Attributes: TStrings): Boolean;

    {:Call any LDAPv3 extended command.}
    function Extended(const Name, Value: AnsiString): Boolean;

    {:Try to start SSL/TLS connection to LDAP server.}
    function StartTLS: Boolean;
  published
    {:Specify version of used LDAP protocol. Default value is 3.}
    property Version: integer read FVersion Write FVersion;

    {:Result code of last LDAP operation.}
    property ResultCode: Integer read FResultCode;

    {:Human readable description of result code of last LDAP operation.}
    property ResultString: AnsiString read FResultString;

    {:Binary string with full last response of LDAP server. This string is
     encoded by ASN.1 BER encoding! You need this only for debugging.}
    property FullResult: AnsiString read FFullResult;

    {:If @true, then try to start TSL mode in Login procedure.}
    property AutoTLS: Boolean read FAutoTLS Write FAutoTLS;

    {:If @true, then use connection to LDAP server through SSL/TLS tunnel.}
    property FullSSL: Boolean read FFullSSL Write FFullSSL;

    {:Sequence number of last LDAp command. It is incremented by any LDAP command.}
    property Seq: integer read FSeq;

    {:Specify what search scope is used in search command.}
    property SearchScope: TLDAPSearchScope read FSearchScope Write FSearchScope;

    {:Specify how to handle aliases in search command.}
    property SearchAliases: TLDAPSearchAliases read FSearchAliases Write FSearchAliases;

    {:Specify result size limit in search command. Value 0 means without limit.}
    property SearchSizeLimit: integer read FSearchSizeLimit Write FSearchSizeLimit;

    {:Specify search time limit in search command (seconds). Value 0 means
     without limit.}
    property SearchTimeLimit: integer read FSearchTimeLimit Write FSearchTimeLimit;

    {:Here is result of search command.}
    property SearchResult: TLDAPResultList read FSearchResult;

    {:On each LDAP operation can LDAP server return some referals URLs. Here is
     their list.}
    property Referals: TStringList read FReferals;

    {:When you call @link(Extended) operation, then here is result Name returned
     by server.}
    property ExtName: AnsiString read FExtName;

    {:When you call @link(Extended) operation, then here is result Value returned
     by server.}
    property ExtValue: AnsiString read FExtValue;

    {:TCP socket used by all LDAP operations.}
    property Sock: TTCPBlockSocket read FSock;
  end;

{:Dump result of LDAP SEARCH into human readable form. Good for debugging.}
function LDAPResultDump(const Value: TLDAPResultList): AnsiString;

implementation

{==============================================================================}
function TLDAPAttribute.Get(Index: integer): string;
begin
  Result := inherited Get(Index);
  if FIsbinary then
    Result := DecodeBase64(Result);
end;

procedure TLDAPAttribute.Put(Index: integer; const Value: string);
var
  s: AnsiString;
begin
  s := Value;
  if FIsbinary then
    s := EncodeBase64(Value)
  else
    s :=UnquoteStr(s, '"');
  inherited Put(Index, s);
end;

procedure TLDAPAttribute.SetAttributeName(Value: AnsiString);
begin
  FAttributeName := Value;
  FIsBinary := Pos(';binary', Lowercase(value)) > 0;
end;

{==============================================================================}
constructor TLDAPAttributeList.Create;
begin
  inherited Create;
  FAttributeList := TList.Create;
end;

destructor TLDAPAttributeList.Destroy;
begin
  Clear;
  FAttributeList.Free;
  inherited Destroy;
end;

procedure TLDAPAttributeList.Clear;
var
  n: integer;
  x: TLDAPAttribute;
begin
  for n := Count - 1 downto 0 do
  begin
    x := GetAttribute(n);
    if Assigned(x) then
      x.Free;
  end;
  FAttributeList.Clear;
end;

function TLDAPAttributeList.Count: integer;
begin
  Result := FAttributeList.Count;
end;

function TLDAPAttributeList.Get(AttributeName: AnsiString): string;
var
  x: TLDAPAttribute;
begin
  Result := '';
  x := self.Find(AttributeName);
  if x <> nil then
    if x.Count > 0 then
      Result := x[0];
end;

function TLDAPAttributeList.GetAttribute(Index: integer): TLDAPAttribute;
begin
  Result := nil;
  if Index < Count then
    Result := TLDAPAttribute(FAttributeList[Index]);
end;

function TLDAPAttributeList.Add: TLDAPAttribute;
begin
  Result := TLDAPAttribute.Create;
  FAttributeList.Add(Result);
end;

procedure TLDAPAttributeList.Del(Index: integer);
var
  x: TLDAPAttribute;
begin
  x := GetAttribute(Index);
  if Assigned(x) then
    x.free;
  FAttributeList.Delete(Index);
end;

function TLDAPAttributeList.Find(AttributeName: AnsiString): TLDAPAttribute;
var
  n: integer;
  x: TLDAPAttribute;
begin
  Result := nil;
  AttributeName := lowercase(AttributeName);
  for n := 0 to Count - 1 do
  begin
    x := GetAttribute(n);
    if Assigned(x) then
      if lowercase(x.AttributeName) = Attributename then
      begin
        result := x;
        break;
      end;
  end;
end;

{==============================================================================}
constructor TLDAPResult.Create;
begin
  inherited Create;
  FAttributes := TLDAPAttributeList.Create;
end;

destructor TLDAPResult.Destroy;
begin
  FAttributes.Free;
  inherited Destroy;
end;

{==============================================================================}
constructor TLDAPResultList.Create;
begin
  inherited Create;
  FResultList := TList.Create;
end;

destructor TLDAPResultList.Destroy;
begin
  Clear;
  FResultList.Free;
  inherited Destroy;
end;

procedure TLDAPResultList.Clear;
var
  n: integer;
  x: TLDAPResult;
begin
  for n := Count - 1 downto 0 do
  begin
    x := GetResult(n);
    if Assigned(x) then
      x.Free;
  end;
  FResultList.Clear;
end;

function TLDAPResultList.Count: integer;
begin
  Result := FResultList.Count;
end;

function TLDAPResultList.GetResult(Index: integer): TLDAPResult;
begin
  Result := nil;
  if Index < Count then
    Result := TLDAPResult(FResultList[Index]);
end;

function TLDAPResultList.Add: TLDAPResult;
begin
  Result := TLDAPResult.Create;
  FResultList.Add(Result);
end;

{==============================================================================}
constructor TLDAPSend.Create;
begin
  inherited Create;
  FReferals := TStringList.Create;
  FFullResult := '';
  FSock := TTCPBlockSocket.Create;
  FSock.Owner := self;
  FTimeout := 60000;
  FTargetPort := cLDAPProtocol;
  FAutoTLS := False;
  FFullSSL := False;
  FSeq := 0;
  FVersion := 3;
  FSearchScope := SS_WholeSubtree;
  FSearchAliases := SA_Always;
  FSearchSizeLimit := 0;
  FSearchTimeLimit := 0;
  FSearchResult := TLDAPResultList.Create;
end;

destructor TLDAPSend.Destroy;
begin
  FSock.Free;
  FSearchResult.Free;
  FReferals.Free;
  inherited Destroy;
end;

function TLDAPSend.GetErrorString(Value: integer): AnsiString;
begin
  case Value of
    0:
      Result := 'Success';
    1:
      Result := 'Operations error';
    2:
      Result := 'Protocol error';
    3:
      Result := 'Time limit Exceeded';
    4:
      Result := 'Size limit Exceeded';
    5:
      Result := 'Compare FALSE';
    6:
      Result := 'Compare TRUE';
    7:
      Result := 'Auth method not supported';
    8:
      Result := 'Strong auth required';
    9:
      Result := '-- reserved --';
    10:
      Result := 'Referal';
    11:
      Result := 'Admin limit exceeded';
    12:
      Result := 'Unavailable critical extension';
    13:
      Result := 'Confidentality required';
    14:
      Result := 'Sasl bind in progress';
    16:
      Result := 'No such attribute';
    17:
      Result := 'Undefined attribute type';
    18:
      Result := 'Inappropriate matching';
    19:
      Result := 'Constraint violation';
    20:
      Result := 'Attribute or value exists';
    21:
      Result := 'Invalid attribute syntax';
    32:
      Result := 'No such object';
    33:
      Result := 'Alias problem';
    34:
      Result := 'Invalid DN syntax';
    36:
      Result := 'Alias dereferencing problem';
    48:
      Result := 'Inappropriate authentication';
    49:
      Result := 'Invalid credentials';
    50:
      Result := 'Insufficient access rights';
    51:
      Result := 'Busy';
    52:
      Result := 'Unavailable';
    53:
      Result := 'Unwilling to perform';
    54:
      Result := 'Loop detect';
    64:
      Result := 'Naming violation';
    65:
      Result := 'Object class violation';
    66:
      Result := 'Not allowed on non leaf';
    67:
      Result := 'Not allowed on RDN';
    68:
      Result := 'Entry already exists';
    69:
      Result := 'Object class mods prohibited';
    71:
      Result := 'Affects multiple DSAs';
    80:
      Result := 'Other';
  else
    Result := '--unknown--';
  end;
end;

function TLDAPSend.Connect: Boolean;
begin
  // Do not call this function! It is calling by LOGIN method!
  FSock.CloseSocket;
  FSock.LineBuffer := '';
  FSeq := 0;
  FSock.Bind(FIPInterface, cAnyPort);
  if FSock.LastError = 0 then
    FSock.Connect(FTargetHost, FTargetPort);
  if FSock.LastError = 0 then
    if FFullSSL then
      FSock.SSLDoConnect;
  Result := FSock.LastError = 0;
end;

function TLDAPSend.BuildPacket(const Value: AnsiString): AnsiString;
begin
  Inc(FSeq);
  Result := ASNObject(ASNObject(ASNEncInt(FSeq), ASN1_INT) + Value,  ASN1_SEQ);
end;

function TLDAPSend.ReceiveResponse: AnsiString;
var
  x: Byte;
  i,j: integer;
begin
  Result := '';
  FFullResult := '';
  x := FSock.RecvByte(FTimeout);
  if x <> ASN1_SEQ then
    Exit;
  Result := AnsiChar(x);
  x := FSock.RecvByte(FTimeout);
  Result := Result + AnsiChar(x);
  if x < $80 then
    i := 0
  else
    i := x and $7F;
  if i > 0 then
    Result := Result + FSock.RecvBufferStr(i, Ftimeout);
  if FSock.LastError <> 0 then
  begin
    Result := '';
    Exit;
  end;
  //get length of LDAP packet
  j := 2;
  i := ASNDecLen(j, Result);
  //retreive rest of LDAP packet
  if i > 0 then
    Result := Result + FSock.RecvBufferStr(i, Ftimeout);
  if FSock.LastError <> 0 then
  begin
    Result := '';
    Exit;
  end;
  FFullResult := Result;
end;

function TLDAPSend.DecodeResponse(const Value: AnsiString): AnsiString;
var
  i, x: integer;
  Svt: Integer;
  s, t: AnsiString;
begin
  Result := '';
  FResultCode := -1;
  FResultstring := '';
  FResponseCode := -1;
  FResponseDN := '';
  FReferals.Clear;
  i := 1;
  ASNItem(i, Value, Svt);
  x := StrToIntDef(ASNItem(i, Value, Svt), 0);
  if (svt <> ASN1_INT) or (x <> FSeq) then
    Exit;
  s := ASNItem(i, Value, Svt);
  FResponseCode := svt;
  if FResponseCode in [LDAP_ASN1_BIND_RESPONSE, LDAP_ASN1_SEARCH_DONE,
    LDAP_ASN1_MODIFY_RESPONSE, LDAP_ASN1_ADD_RESPONSE, LDAP_ASN1_DEL_RESPONSE,
    LDAP_ASN1_MODIFYDN_RESPONSE, LDAP_ASN1_COMPARE_RESPONSE,
    LDAP_ASN1_EXT_RESPONSE] then
  begin
    FResultCode := StrToIntDef(ASNItem(i, Value, Svt), -1);
    FResponseDN := ASNItem(i, Value, Svt);
    FResultString := ASNItem(i, Value, Svt);
    if FResultString = '' then
      FResultString := GetErrorString(FResultCode);
    if FResultCode = 10 then
    begin
      s := ASNItem(i, Value, Svt);
      if svt = $A3 then
      begin
        x := 1;
        while x < Length(s) do
        begin
          t := ASNItem(x, s, Svt);
          FReferals.Add(t);
        end;
      end;
    end;
  end;
  Result := Copy(Value, i, Length(Value) - i + 1);
end;

function TLDAPSend.LdapSasl(Value: AnsiString): AnsiString;
var
  nonce, cnonce, nc, realm, qop, uri, response: AnsiString;
  s: AnsiString;
  a1, a2: AnsiString;
  l: TStringList;
  n: integer;
begin
  l := TStringList.Create;
  try
    nonce := '';
    realm := '';
    l.CommaText := Value;
    n := IndexByBegin('nonce=', l);
    if n >= 0 then
      nonce := UnQuoteStr(Trim(SeparateRight(l[n], 'nonce=')), '"');
    n := IndexByBegin('realm=', l);
    if n >= 0 then
      realm := UnQuoteStr(Trim(SeparateRight(l[n], 'realm=')), '"');
    cnonce := IntToHex(GetTick, 8);
    nc := '00000001';
    qop := 'auth';
    uri := 'ldap/' + FSock.ResolveIpToName(FSock.GetRemoteSinIP);
    a1 := md5(FUsername + ':' + realm + ':' + FPassword)
      + ':' + nonce + ':' + cnonce;
    a2 := 'AUTHENTICATE:' + uri;
    s := strtohex(md5(a1))+':' + nonce + ':' + nc + ':' + cnonce + ':'
      + qop +':'+strtohex(md5(a2));
    response := strtohex(md5(s));

    Result := 'username="' + Fusername + '",realm="' + realm + '",nonce="';
    Result := Result + nonce + '",cnonce="' + cnonce + '",nc=' + nc + ',qop=';
    Result := Result + qop + ',digest-uri="' + uri + '",response=' + response;
  finally
    l.Free;
  end;
end;

function TLDAPSend.TranslateFilter(Value: AnsiString): AnsiString;
var
  x: integer;
  s, t, l: AnsiString;
  r: string;
  c: Ansichar;
  attr, rule: AnsiString;
  dn: Boolean;
begin
  Result := '';
  if Value = '' then
    Exit;
  s := Value;
  if Value[1] = '(' then
  begin
    x := RPos(')', Value);
    s := Copy(Value, 2, x - 2);
  end;
  if s = '' then
    Exit;
  case s[1] of
    '!':
      // NOT rule (recursive call)
      begin
        Result := ASNOBject(TranslateFilter(GetBetween('(', ')', s)), $A2);
      end;
    '&':
      // AND rule (recursive call)
      begin
        repeat
          t := GetBetween('(', ')', s);
          s := Trim(SeparateRight(s, t));
          if s <> '' then
            if s[1] = ')' then
              {$IFDEF CIL}Borland.Delphi.{$ENDIF}System.Delete(s, 1, 1);
          Result := Result + TranslateFilter(t);
        until s = '';
        Result := ASNOBject(Result, $A0);
      end;
    '|':
      // OR rule (recursive call)
      begin
        repeat
          t := GetBetween('(', ')', s);
          s := Trim(SeparateRight(s, t));
          if s <> '' then
            if s[1] = ')' then
              {$IFDEF CIL}Borland.Delphi.{$ENDIF}System.Delete(s, 1, 1);
          Result := Result + TranslateFilter(t);
        until s = '';
        Result := ASNOBject(Result, $A1);
      end;
    else
      begin
        l := Trim(SeparateLeft(s, '='));
        r := Trim(SeparateRight(s, '='));
        if l <> '' then
        begin
          c := l[Length(l)];
          case c of
            ':':
              // Extensible match
              begin
                {$IFDEF CIL}Borland.Delphi.{$ENDIF}System.Delete(l, Length(l), 1);
                dn := False;
                attr := '';
                rule := '';
                if Pos(':dn', l) > 0 then
                begin
                  dn := True;
                  l := ReplaceString(l, ':dn', '');
                end;
                attr := Trim(SeparateLeft(l, ':'));
                rule := Trim(SeparateRight(l, ':'));
                if rule = l then
                  rule := '';
                if rule <> '' then
                  Result := ASNObject(rule, $81);
                if attr <> '' then
                  Result := Result + ASNObject(attr, $82);
                Result := Result + ASNObject(DecodeTriplet(r, '\'), $83);
                if dn then
                  Result := Result + ASNObject(AsnEncInt($ff), $84)
                else
                  Result := Result + ASNObject(AsnEncInt(0), $84);
                Result := ASNOBject(Result, $a9);
              end;
            '~':
              // Approx match
              begin
                {$IFDEF CIL}Borland.Delphi.{$ENDIF}System.Delete(l, Length(l), 1);
                Result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(DecodeTriplet(r, '\'), ASN1_OCTSTR);
                Result := ASNOBject(Result, $a8);
              end;
            '>':
              // Greater or equal match
              begin
                {$IFDEF CIL}Borland.Delphi.{$ENDIF}System.Delete(l, Length(l), 1);
                Result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(DecodeTriplet(r, '\'), ASN1_OCTSTR);
                Result := ASNOBject(Result, $a5);
              end;
            '<':
              // Less or equal match
              begin
                {$IFDEF CIL}Borland.Delphi.{$ENDIF}System.Delete(l, Length(l), 1);
                Result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(DecodeTriplet(r, '\'), ASN1_OCTSTR);
                Result := ASNOBject(Result, $a6);
              end;
          else
            // present
            if r = '*' then
              Result := ASNOBject(l, $87)
            else
              if Pos('*', r) > 0 then
              // substrings
              begin
                s := Fetch(r, '*');
                if s <> '' then
                  Result := ASNOBject(DecodeTriplet(s, '\'), $80);
                while r <> '' do
                begin
                  if Pos('*', r) <= 0 then
                    break;
                  s := Fetch(r, '*');
                  Result := Result + ASNOBject(DecodeTriplet(s, '\'), $81);
                end;
                if r <> '' then
                  Result := Result + ASNOBject(DecodeTriplet(r, '\'), $82);
                Result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(Result, ASN1_SEQ);
                Result := ASNOBject(Result, $a4);
              end
              else
              begin
                // Equality match
                Result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(DecodeTriplet(r, '\'), ASN1_OCTSTR);
                Result := ASNOBject(Result, $a3);
              end;
          end;
        end;
      end;
  end;
end;

function TLDAPSend.Login: Boolean;
begin
  Result := False;
  if not Connect then
    Exit;
  Result := True;
  if FAutoTLS then
    Result := StartTLS;
end;

function TLDAPSend.Bind: Boolean;
var
  s: AnsiString;
begin
  s := ASNObject(ASNEncInt(FVersion), ASN1_INT)
    + ASNObject(FUsername, ASN1_OCTSTR)
    + ASNObject(FPassword, $80);
  s := ASNObject(s, LDAP_ASN1_BIND_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  DecodeResponse(s);
  Result := FResultCode = 0;
end;

function TLDAPSend.BindSasl: Boolean;
var
  s, t: AnsiString;
  x, xt: integer;
  digreq: AnsiString;
begin
  Result := False;
  if FPassword = '' then
    Result := Bind
  else
  begin
    digreq := ASNObject(ASNEncInt(FVersion), ASN1_INT)
      + ASNObject('', ASN1_OCTSTR)
      + ASNObject(ASNObject('DIGEST-MD5', ASN1_OCTSTR), $A3);
    digreq := ASNObject(digreq, LDAP_ASN1_BIND_REQUEST);
    Fsock.SendString(BuildPacket(digreq));
    s := ReceiveResponse;
    t := DecodeResponse(s);
    if FResultCode = 14 then
    begin
      s := t;
      x := 1;
      t := ASNItem(x, s, xt);
      s := ASNObject(ASNEncInt(FVersion), ASN1_INT)
        + ASNObject('', ASN1_OCTSTR)
        + ASNObject(ASNObject('DIGEST-MD5', ASN1_OCTSTR)
          + ASNObject(LdapSasl(t), ASN1_OCTSTR), $A3);
      s := ASNObject(s, LDAP_ASN1_BIND_REQUEST);
      Fsock.SendString(BuildPacket(s));
      s := ReceiveResponse;
      DecodeResponse(s);
      if FResultCode = 14 then
      begin
        Fsock.SendString(BuildPacket(digreq));
        s := ReceiveResponse;
        DecodeResponse(s);
      end;
      Result := FResultCode = 0;
    end;
  end;
end;

function TLDAPSend.Logout: Boolean;
begin
  Fsock.SendString(BuildPacket(ASNObject('', LDAP_ASN1_UNBIND_REQUEST)));
  FSock.CloseSocket;
  Result := True;
end;

function TLDAPSend.Modify(obj: AnsiString; Op: TLDAPModifyOp; const Value: TLDAPAttribute): Boolean;
var
  s: AnsiString;
  n: integer;
begin
  s := '';
  for n := 0 to Value.Count -1 do
    s := s + ASNObject(Value[n], ASN1_OCTSTR);
  s := ASNObject(Value.AttributeName, ASN1_OCTSTR) + ASNObject(s, ASN1_SETOF);
  s := ASNObject(ASNEncInt(Ord(Op)), ASN1_ENUM) + ASNObject(s, ASN1_SEQ);
  s := ASNObject(s, ASN1_SEQ);
  s := ASNObject(obj, ASN1_OCTSTR) + ASNObject(s, ASN1_SEQ);
  s := ASNObject(s, LDAP_ASN1_MODIFY_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  DecodeResponse(s);
  Result := FResultCode = 0;
end;

function TLDAPSend.Add(obj: AnsiString; const Value: TLDAPAttributeList): Boolean;
var
  s, t: AnsiString;
  n, m: integer;
begin
  s := '';
  for n := 0 to Value.Count - 1 do
  begin
    t := '';
    for m := 0 to Value[n].Count - 1 do
      t := t + ASNObject(Value[n][m], ASN1_OCTSTR);
    t := ASNObject(Value[n].AttributeName, ASN1_OCTSTR)
      + ASNObject(t, ASN1_SETOF);
    s := s + ASNObject(t, ASN1_SEQ);
  end;
  s := ASNObject(obj, ASN1_OCTSTR) + ASNObject(s, ASN1_SEQ);
  s := ASNObject(s, LDAP_ASN1_ADD_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  DecodeResponse(s);
  Result := FResultCode = 0;
end;

function TLDAPSend.Delete(obj: AnsiString): Boolean;
var
  s: AnsiString;
begin
  s := ASNObject(obj, LDAP_ASN1_DEL_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  DecodeResponse(s);
  Result := FResultCode = 0;
end;

function TLDAPSend.ModifyDN(obj, newRDN, newSuperior: AnsiString; DeleteOldRDN: Boolean): Boolean;
var
  s: AnsiString;
begin
  s := ASNObject(obj, ASN1_OCTSTR) + ASNObject(newRDN, ASN1_OCTSTR);
  if DeleteOldRDN then
    s := s + ASNObject(ASNEncInt($ff), ASN1_BOOL)
  else
    s := s + ASNObject(ASNEncInt(0), ASN1_BOOL);
  if newSuperior <> '' then
    s := s + ASNObject(newSuperior, $80);
  s := ASNObject(s, LDAP_ASN1_MODIFYDN_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  DecodeResponse(s);
  Result := FResultCode = 0;
end;

function TLDAPSend.Compare(obj, AttributeValue: AnsiString): Boolean;
var
  s: AnsiString;
begin
  s := ASNObject(Trim(SeparateLeft(AttributeValue, '=')), ASN1_OCTSTR)
    + ASNObject(Trim(SeparateRight(AttributeValue, '=')), ASN1_OCTSTR);
  s := ASNObject(obj, ASN1_OCTSTR) + ASNObject(s, ASN1_SEQ);
  s := ASNObject(s, LDAP_ASN1_COMPARE_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  DecodeResponse(s);
  Result := FResultCode = 0;
end;

function TLDAPSend.Search(obj: AnsiString; TypesOnly: Boolean; Filter: AnsiString;
  const Attributes: TStrings): Boolean;
var
  s, t, u: AnsiString;
  n, i, x: integer;
  r: TLDAPResult;
  a: TLDAPAttribute;
begin
  FSearchResult.Clear;
  FReferals.Clear;
  s := ASNObject(obj, ASN1_OCTSTR);
  s := s + ASNObject(ASNEncInt(Ord(FSearchScope)), ASN1_ENUM);
  s := s + ASNObject(ASNEncInt(Ord(FSearchAliases)), ASN1_ENUM);
  s := s + ASNObject(ASNEncInt(FSearchSizeLimit), ASN1_INT);
  s := s + ASNObject(ASNEncInt(FSearchTimeLimit), ASN1_INT);
  if TypesOnly then
    s := s + ASNObject(ASNEncInt($ff), ASN1_BOOL)
  else
    s := s + ASNObject(ASNEncInt(0), ASN1_BOOL);
  if Filter = '' then
    Filter := '(objectclass=*)';
  t := TranslateFilter(Filter);
  if t = '' then
    s := s + ASNObject('', ASN1_NULL)
  else
    s := s + t;
  t := '';
  for n := 0 to Attributes.Count - 1 do
    t := t + ASNObject(Attributes[n], ASN1_OCTSTR);
  s := s + ASNObject(t, ASN1_SEQ);
  s := ASNObject(s, LDAP_ASN1_SEARCH_REQUEST);
  Fsock.SendString(BuildPacket(s));
  repeat
    s := ReceiveResponse;
    t := DecodeResponse(s);
    if FResponseCode = LDAP_ASN1_SEARCH_ENTRY then
    begin
      //dekoduj zaznam
      r := FSearchResult.Add;
      n := 1;
      r.ObjectName := ASNItem(n, t, x);
      ASNItem(n, t, x);
      if x = ASN1_SEQ then
      begin
        while n < Length(t) do
        begin
          s := ASNItem(n, t, x);
          if x = ASN1_SEQ then
          begin
            i := n + Length(s);
            a := r.Attributes.Add;
            u := ASNItem(n, t, x);
            a.AttributeName := u;
            ASNItem(n, t, x);
            if x = ASN1_SETOF then
              while n < i do
              begin
                u := ASNItem(n, t, x);
                a.Add(u);
              end;
          end;
        end;
      end;
    end;
    if FResponseCode = LDAP_ASN1_SEARCH_REFERENCE then
    begin
      n := 1;
      while n < Length(t) do
        FReferals.Add(ASNItem(n, t, x));
    end;
  until FResponseCode = LDAP_ASN1_SEARCH_DONE;
  Result := FResultCode = 0;
end;

function TLDAPSend.Extended(const Name, Value: AnsiString): Boolean;
var
  s, t: AnsiString;
  x, xt: integer;
begin
  s := ASNObject(Name, $80);
  if Value <> '' then
    s := s + ASNObject(Value, $81);
  s := ASNObject(s, LDAP_ASN1_EXT_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  t := DecodeResponse(s);
  Result := FResultCode = 0;
  if Result then
  begin
    x := 1;
    FExtName := ASNItem(x, t, xt);
    FExtValue := ASNItem(x, t, xt);
  end;
end;


function TLDAPSend.StartTLS: Boolean;
begin
  Result := Extended('1.3.6.1.4.1.1466.20037', '');
  if Result then
  begin
    Fsock.SSLDoConnect;
    Result := FSock.LastError = 0;
  end;
end;

{==============================================================================}
function LDAPResultDump(const Value: TLDAPResultList): AnsiString;
var
  n, m, o: integer;
  r: TLDAPResult;
  a: TLDAPAttribute;
begin
  Result := 'Results: ' + IntToStr(Value.Count) + CRLF +CRLF;
  for n := 0 to Value.Count - 1 do
  begin
    Result := Result + 'Result: ' + IntToStr(n) + CRLF;
    r := Value[n];
    Result := Result + '  Object: ' + r.ObjectName + CRLF;
    for m := 0 to r.Attributes.Count - 1 do
    begin
      a := r.Attributes[m];
      Result := Result + '  Attribute: ' + a.AttributeName + CRLF;
      for o := 0 to a.Count - 1 do
        Result := Result + '    ' + a[o] + CRLF;
    end;
  end;
end;

end.
