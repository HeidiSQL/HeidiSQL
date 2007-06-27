{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                Classes for Winsock API                 }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZSocket;

interface

uses Classes, Windows, Winsock, SysUtils, ZToken, ZExtra;

{$INCLUDE ..\Zeos.inc}

const
  BACKLOG_NUM = 5;

type
  EWinsockError = class(Exception);

  { Class to incapsulate Universal Resource Locator }
  TURL = class
  private
    FHostName: string;
    FProto:    string;
    FFileName: string;
    FPath:     string;
    FPort:     Integer;
    FIP:       u_long;

    function IP2Name(IP: u_long): string;
    function Name2IP(const HostNm: string): u_long;

    procedure FillUrlByName(const NewHost: string; NewPort: Integer;
      const NewFile: string);
    procedure FillUrlByIP(NewIP: u_long; NewPort: Integer; const NewFile: string);
  public
    constructor Create;
    constructor CreateByName(const NewHost: string; NewPort: Integer; NewFile: string);
    constructor CreateByIP(NewIP: u_long; NewPort: Integer; NewFile: string);

    function  FillAddr(var Addr: TSockAddrIn): Integer;
    function  GetHostName: string;
    procedure SetHostName(const NewHost: string);

    function  GetIP: u_long;
    procedure SetIP(NewIP: u_long);
    procedure Assign(NewURL: TURL );
    function  GetUrl: string;
    procedure SetUrl(Value: string);

    property Proto: string read FProto write FProto;
    property FileName: string read FFileName write FFileName;
    property Port: Integer read FPort write FPort;
    property URL: string read GetUrl write SetUrl;
    property HostName: string read GetHostName write SetHostName;
    property IP: u_long read GetIP write SetIP;
  end;

  { TCP-IP socket abstract class }
  TInetSocket = class
  private
    FServer, FClient: TURL;
    FSid: TSocket;
    FRc: Integer;
  public
    constructor Create;
    constructor CreateByHandle(NewSid: Integer);
    destructor Destroy; override;

    function  IsGood: Boolean;
    function  QueueSize: LongInt;
    procedure SetOptions(Cmd: Longint; var Arg: u_long);
    procedure SetEvents(Handle: HWND; Msg: u_int; Event: Longint);

    procedure CloseConnect;

    property Handle: TSocket read FSid;
    property Server: TURL read FServer;
    property Client: TURL read FClient;
  end;

  { TCP-IP client socket class }
  TInetClientSocket = class(TInetSocket)
  public
    constructor Create;
    constructor CreateByName(const Name: string; Port: Integer);
    constructor CreateByIP(IP: u_long; Port: Integer);
    constructor CreateByURL(NewURL: TURL);
    constructor CreateByHandle(NewSid: Integer);

    function IsArrive: Boolean;
    function ConnectSocket(const HostNm: string; Port: Integer): Integer;
    function ConnectSocketByURL(Url: TURL): Integer;

    function Write(var Buf; Len, Flag: Integer): Integer;
    function Read (var Buf; Len, Flag: Integer): Integer;
end;

  { TCP-IP server socket class }
  TInetServerSocket = class(TInetSocket)
  private
    function BindSocket: Integer;
  public
    constructor Create;
    constructor CreateByName(const Name: string; Port: Integer);
    constructor CreateByIP(IP: u_long; Port: Integer);
    constructor CreateByURL(NewURL: TURL);
    constructor CreateByHandle(NewSid: Integer);

    function ListenConnect: Integer;
    function AcceptConnect: TInetClientSocket;
    function ShutdownConnect(Mode: Integer): Integer;
  end;

{ Initialize Winsock library }
function WinSocketStartup: Boolean;

{ Deinitialize Winsock library }
procedure WinSocketCleanup;

{ Define host name by IP-address }
function IP2Str(IP: u_long): string;

{ Define IP-address by host name }
function Str2IP(Buff: string): u_long;

{ Define host name of the local comp }
function GetLocalHost: string;

{ Process Winsock errors }
procedure WinSocketCheckError;

implementation

uses ZUtilsConst;

{********************* Common functions implementation ************************}

{ Initialize Winsock library }
function WinSocketStartup: Boolean;
var
  wVersionRequested: Word;
  wsaData: TWSADATA;
begin
  wVersionRequested := $0101;
  Result := WSAStartup(wVersionRequested, wsaData) = 0;

  if not Result then
    raise EWinsockError.Create(SLoadWinsockError);
end;

{ Deinialize Winsock labrary }
procedure WinSocketCleanup;
begin
  WSACleanup;
end;

{ Process Winsock errors }
procedure WinSocketCheckError;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAGetLastError;
  if (ErrorCode <> 0) and (ErrorCode <> WSAEWOULDBLOCK) then
    raise EWinsockError.CreateFmt(SWinsockError,
      [SysErrorMessage(ErrorCode), ErrorCode]);
end;

{ Get localhost name }
function GetLocalHost: string;
var
  PHostName: PChar;
begin
  GetMem(PHostName, 100);
  try
    if GetHostName(PHostName, 100) <> -1 then
      Result := PHostName
    else
      Result := '';
  finally
    FreeMem(PHostName)
  end;
end;

{ Invert 4 bytes number }
function RevertInt(Value: u_long): u_long;
begin
  Result := ((Value shr 24) or ((Value and $ff0000) shr 8) or
    ((Value and $ff00) shl 8) or ((Value and $ff) shl 24));
end;

{ IP-Address to string }
function IP2Str(IP: u_long): string;
begin
  Result:=Format('%d.%d.%d.%d', [(IP shr 24) and $ff,
    (IP shr 16) and $ff, (IP shr 8) and $ff, IP and $ff]);
end;

{ string to IP-address }
function Str2IP(Buff: string): u_long;
var
  Addr: array[0..3] of u_long;
begin
  Addr[0] := StrToIntDef(StrTok(Buff,'. '),0);
  Addr[1] := StrToIntDef(StrTok(Buff,'. '),0);
  Addr[2] := StrToIntDef(StrTok(Buff,'. '),0);
  Addr[3] := StrToIntDef(StrTok(Buff,'. '),0);
  Result  := ((Addr[0] shl 24) or (Addr[1] shl 16) or (Addr[2] shl 8) or Addr[3]);
end;

{*****************  TURL implementation *******************}

{ Class constructor }
constructor TURL.Create;
begin
  FHostName := '';
  FFileName := '/';
  FIP   := 0;
  FPort := 0;
end;

constructor TURL.CreateByName(const NewHost: string; NewPort: Integer;
  NewFile: string);
begin
  if NewFile <> '' then
    FillUrlByName(NewHost, newPort, newFile)
  else
    FillUrlByName(NewHost, NewPort, '/');
end;

constructor TURL.CreateByIP(NewIP: u_long; NewPort: Integer; NewFile: string);
begin
  if NewFile <> '' then
    FillUrlByIP(NewIP, NewPort, NewFile)
  else
    FillUrlByIP(NewIP, NewPort, '/');
end;

function TURL.GetIP: u_long;
begin
  Result:= RevertInt(FIP);
end;

{ Convert IP-address to host name }
function TURL.IP2Name(IP: u_long): string;
var
  InAddr: TInAddr;
  LAddr: u_long;
  Hp: PHostEnt;
  P: ^PChar;
begin
  InAddr.S_addr := IP;
  Result := '';
  LAddr := inet_addr(inet_ntoa(InAddr));
  if LAddr = -1 then Exit;
  Hp := gethostbyaddr(@LAddr, SizeOf (LAddr), AF_INET);
  WinSocketCheckError;
  if not Assigned(Hp) then Exit;
  P := @Hp^.h_addr_list;
  while Assigned(P^) do
  begin
    Move(p^, inaddr.s_addr, SizeOf (inaddr.s_addr));
    if Assigned(hp^.h_name) then
    begin
      Result := StrPas(hp^.h_name);
      Break;
    end;
    Inc(P);
  end;
end;

function TURL.Name2IP(const HostNm: string ): u_long;
var
  Hp:   PHostEnt;
  Buff: array[0..255] of Char;
begin
  Result := 0;
  Hp := gethostbyname(StrPCopy(buff,HostNm));
  if Assigned(Hp) then
    Move(Hp^.h_addr^^, Result, Hp^.h_length);
  WinSocketCheckError;
end;

procedure TURL.FillUrlByName(const NewHost: string; NewPort: Integer; const NewFile: string);
begin
  FHostName := NewHost;
  FIP       := Name2IP(NewHost);
  FPort     := NewPort;
  FFileName := NewFile;
end;

procedure TURL.FillUrlByIp(NewIP: u_long; NewPort: Integer; const NewFile: string);
begin
  FIP       := RevertInt(NewIP);
  FHostName := IP2Name(FIP);
  FPort     := NewPort;
  FFileName := NewFile;
end;

function TURL.FillAddr(var Addr: TSockAddrIn): Integer;
begin
  Addr.sin_family      := AF_INET;
  Addr.sin_addr.s_addr := FIP;
  Addr.sin_port        := htons(FPort);
  Result               := SizeOf(addr);
end;

function TURL.GetHostName: string;
begin
  if FHostName = '' then
    FHostName := IP2Str(RevertInt(FIP));
  Result := FHostName;
end;

procedure TURL.SetHostName(const NewHost: string);
begin
  FHostName := NewHost;
  FIP := Name2IP(FHostName);
end;

procedure TURL.SetIP(NewIP: u_long);
begin
  FIP := RevertInt(NewIP);
  FHostName := '';
  if FIP = $100007F then
    FHostName := 'localhost'
  else
    try
      FHostName := IP2Name(FIP);
    except
      FHostName := IP2Str(FIP);
    end;
end;

function TURL.GetURL: string;
begin
  if FPath = '' then
    FPath := HostName + ':' + IntToStr(FPort) + FileName;
  Result := FPath;
end;

procedure TURL.SetUrl(Value: string);
var
  Temp: string;
begin
  FPath  := Value;
  Temp   := StrTok(Value,':/@ ');
  FPort  := 0;
  FProto := '';
  if StrCmpBegin(Value, '://') then
  begin
    FProto := LowerCase(Temp);
    Value  := Copy(Value,4,Length(Value)-3);
    Temp   := StrTok(Value,':/@ ');
  end;

  if FProto = 'file' then
  begin
    FHostName := '';
    FIP       := 0;
    FFileName := Temp + StrTok(Value,'?');
  end
  else
  begin
    FHostName := Temp;
    FIP       := Name2IP(FHostName);
    if StrCmpBegin(Value,':') then
    begin
      Temp  := StrTok(Value,':/@ ');
      FPort := StrToIntDef(Temp,0);
    end;
    FFileName := Value;
  end;

  if FProto = '' then FProto := 'http';
  if FFileName = '' then FFileName := '/';

  if FProto <> 'file' then
  begin
    Temp := IP2Name(FIP);
    if Temp<>'' then FHostName := Temp;
  end;

  FPath := FProto + '://' + FHostName;
  if FPort <> 0 then FPath := FPath + IntToStr(FPort);
  if (FPath[Length(FPath)]<>'/') and (FFileName[1]<>'/') then
    FPath := FPath + '/';
  FPath := FPath + FFileName;

  if (FProto = 'http') and (FPort = 0) then FPort := 80;
  if (FProto = 'ftp') and (FPort = 0) then  FPort := 21;
end;

procedure TURL.Assign(NewURL: TURL);
begin
  FPort     := NewURL.FPort;
  FHostName := NewURL.FHostName;
  FFileName := NewURL.FFileName;
  FIP       := NewURL.FIP;
end;

{******************* TInetSocket implementation ***********************}

{ Check socket status }
function TInetSocket.IsGood: Boolean;
begin
  Result := (FSid >= 0);
end;

function TInetSocket.QueueSize: LongInt;
begin
  Result:=0;
  ioctlsocket(FSid, FIONREAD, Result);
end;

procedure TInetSocket.SetEvents(Handle: HWND; Msg: u_int; Event: LongInt);
begin
  WSAAsyncSelect(FSid, Handle, Msg, Event);
  WinSocketCheckError;
end;

procedure TInetSocket.SetOptions(Cmd: LongInt; var Arg: u_long);
begin
  ioctlsocket(FSid, cmd, arg);
end;

constructor TInetSocket.Create;
begin
  FServer := TURL.Create;
  FClient := TURL.Create;
  FSid    := 0;
end;

constructor TInetSocket.CreateByHandle(NewSid: Integer);
begin
  FServer := TURL.CreateByName('localhost', 0, '/');
  FClient := TURL.CreateByIP(0, 0, '/');
  FSid    := NewSid;
end;

destructor TInetSocket.Destroy;
begin
  try
    CloseConnect;
  finally
    FServer.Free;
    FClient.Free;
  end;
end;

procedure TInetSocket.CloseConnect;
begin
  shutdown (FSid,2);
//  WinSocketCheckError;
  closesocket(FSid);
//  WinSocketCheckError;
end;

{************** TInetClientSocket implementation ***************}

function TInetClientSocket.IsArrive: Boolean;
begin
  Result := (QueueSize > 0);
end;

function TInetClientSocket.Write(var Buf; Len,Flag: Integer): Integer;
begin
  Result := Send(FSid, Buf, Len, Flag );
  if Result = SOCKET_ERROR then WinSocketCheckError;
end;

function TInetClientSocket.Read(var Buf; Len,Flag: Integer ): Integer;
begin
  Result := Recv(FSid, Buf, Len, Flag);
  if Result = SOCKET_ERROR then WinSocketCheckError;
end;

constructor TInetClientSocket.Create;
begin
  FServer := TURL.Create;
  FClient := TURL.Create;
  FSid    := 0;
end;

constructor TInetClientSocket.CreateByName(const Name: string; Port: Integer);
begin
  FServer := TURL.CreateByName(Name, Port, '/');
  FClient := TURL.Create;
  FSid    := Socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  if FSid = INVALID_SOCKET then WinSocketCheckError;
end;

constructor TInetClientSocket.CreateByIP(IP: u_long; Port: Integer);
begin
  FServer := TURL.CreateByIP(IP, Port, '/');
  FClient := TURL.Create;
  FSid    := Socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  if FSid = INVALID_SOCKET then WinSocketCheckError;
end;

constructor TInetClientSocket.CreateByUrl(NewURL: TURL);
begin
  FServer := TURL.Create;
  FServer.Assign(newURL);
  FClient := TURL.Create;
  FSid    := Socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  if FSid = INVALID_SOCKET then WinSocketCheckError;
end;

constructor TInetClientSocket.CreateByHandle(NewSid: Integer);
begin
  FServer := TURL.CreateByName('localhost', 0, '/');
  FClient := TURL.CreateByIP( 0, 0, '/');
  FSid    := NewSid;
end;

function TInetClientSocket.ConnectSocket(const HostNm: string; Port: Integer): Integer;
var
  Url: TURL;
begin
  Url := TURL.CreateByName(HostNm, Port, '/');
  try
    Result := ConnectSocketByURL(url);
  finally
    Url.Free;
  end;
end;

function TInetClientSocket.ConnectSocketByURL(Url: TURL): Integer;
var
  Addr: TSockAddrIn;
  Len:  Integer;
begin
  Len := url.FillAddr(Addr);
  FRc := Connect(FSid, Addr, Len);
  if FRc = SOCKET_ERROR then WinSocketCheckError;
  Result := FRc;
end;

{**************** TInetServerSocket implementation ***************}

function TInetServerSocket.BindSocket: Integer;
var
  addr: TSockAddrIn;
  len:  u_long;
begin
  FSid := socket(AF_INET, SOCK_STREAM, 0);
  WinSocketCheckError;
  len  := FServer.FillAddr( addr );
  FRc  := bind(FSid, addr, len);
  WinSocketCheckError;
  if FRc < 0 then
  begin
    FRc := getsockname(FSid, addr, len);
    if FRc < 0 then CloseConnect;
    FSid := -1;
  end;
  Result := FRc;
end;

constructor TInetServerSocket.Create;
begin
  FServer := TURL.Create;
  FClient := TURL.Create;
end;

constructor TInetServerSocket.CreateByName(const Name: string; Port: Integer);
begin
  FServer := TURL.CreateByName(Name, Port, '/');
  FClient := TURL.Create;
  BindSocket;
end;

constructor  TInetServerSocket.CreateByIP(IP: u_long; Port: Integer);
begin
  FServer := TURL.CreateByIP(IP, Port, '/');
  FClient := TURL.Create;
  BindSocket;
end;

constructor TInetServerSocket.CreateByURL(NewURL: TURL);
begin
  FServer := TURL.Create;
  FServer.Assign(NewURL);
  FClient := TURL.Create;
  BindSocket;
end;

constructor TInetServerSocket.CreateByHandle(NewSid: Integer);
begin
  FServer := TURL.CreateByName('localhost', 0, '/');
  FClient := TURL.Create;
  FSid    := NewSid;
end;

function TInetServerSocket.ListenConnect: Integer;
begin
  Result := listen(FSid,BACKLOG_NUM);
  WinSocketCheckError;
end;

function TInetServerSocket.AcceptConnect: TInetClientSocket;
var
  Sock: TInetClientSocket;
  Addr: TSockAddrIn;
  Len:  u_long;
begin
  Len := SizeOf(Addr);
  FRc := accept(FSid, @Addr, @Len);
//  WinSocketCheckError;
  if FRc > 0 then
  begin
    FClient.IP   := RevertInt(Addr.sin_addr.s_addr);
    FClient.Port := Addr.sin_port;
    Sock         := TInetClientSocket.CreateByHandle(FRc);
    Sock.Server.Assign(FServer);
    Sock.Client.Assign(FClient);
    Result       := Sock;
  end else
    Result := nil;
end;

function TInetServerSocket.ShutdownConnect(Mode: Integer): Integer;
begin
  Result := shutdown(FSid,Mode);
  WinSocketCheckError;
end;

initialization
  WinSocketStartup;
finalization
  WinSocketCleanup;
end.
