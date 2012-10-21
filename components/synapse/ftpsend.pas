{==============================================================================|
| Project : Ararat Synapse                                       | 004.000.000 |
|==============================================================================|
| Content: FTP client                                                          |
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
| Portions created by Lukas Gebauer are Copyright (c) 1999-2010.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Petr Esner <petr.esner@atlas.cz>                                           |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{: @abstract(FTP client protocol)

Used RFC: RFC-959, RFC-2228, RFC-2428
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}
{$TYPEINFO ON}// Borland changed defualt Visibility from Public to Published
                // and it requires RTTI to be generated $M+
{$M+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit ftpsend;

interface

uses
  SysUtils, Classes,
  blcksock, synautil, synaip, synsock;

const
  cFtpProtocol = '21';
  cFtpDataProtocol = '20';

  {:Terminating value for TLogonActions}
  FTP_OK = 255;
  {:Terminating value for TLogonActions}
  FTP_ERR = 254;

type
  {:Array for holding definition of logon sequence.}
  TLogonActions = array [0..17] of byte;

  {:Procedural type for OnStatus event. Sender is calling @link(TFTPSend) object.
   Value is FTP command or reply to this comand. (if it is reply, Response
   is @True).}
  TFTPStatus = procedure(Sender: TObject; Response: Boolean;
    const Value: string) of object;

  {: @abstract(Object for holding file information) parsed from directory
   listing of FTP server.}
  TFTPListRec = class(TObject)
  private
    FFileName: String;
    FDirectory: Boolean;
    FReadable: Boolean;
    FFileSize: int64;
    FFileTime: TDateTime;
    FOriginalLine: string;
    FMask: string;
    FPermission: String;
  public
    {: You can assign another TFTPListRec to this object.}
    procedure Assign(Value: TFTPListRec); virtual;
    {:name of file}
    property FileName: string read FFileName write FFileName;
    {:if name is subdirectory not file.}
    property Directory: Boolean read FDirectory write FDirectory;
    {:if you have rights to read}
    property Readable: Boolean read FReadable write FReadable;
    {:size of file in bytes}
    property FileSize: int64 read FFileSize write FFileSize;
    {:date and time of file. Local server timezone is used. Any timezone
     conversions was not done!}
    property FileTime: TDateTime read FFileTime write FFileTime;
    {:original unparsed line}
    property OriginalLine: string read FOriginalLine write FOriginalLine;
    {:mask what was used for parsing}
    property Mask: string read FMask write FMask;
    {:permission string (depending on used mask!)}
    property Permission: string read FPermission write FPermission;
  end;

  {:@abstract(This is TList of TFTPListRec objects.)
   This object is used for holding lististing of all files information in listed
   directory on FTP server.}
  TFTPList = class(TObject)
  protected
    FList: TList;
    FLines: TStringList;
    FMasks: TStringList;
    FUnparsedLines: TStringList;
    Monthnames: string;
    BlockSize: string;
    DirFlagValue: string;
    FileName: string;
    VMSFileName: string;
    Day: string;
    Month: string;
    ThreeMonth: string;
    YearTime: string;
    Year: string;
    Hours: string;
    HoursModif: Ansistring;
    Minutes: string;
    Seconds: string;
    Size: Ansistring;
    Permissions: Ansistring;
    DirFlag: string;
    function GetListItem(Index: integer): TFTPListRec; virtual;
    function ParseEPLF(Value: string): Boolean; virtual;
    procedure ClearStore; virtual;
    function ParseByMask(Value, NextValue, Mask: ansistring): Integer; virtual;
    function CheckValues: Boolean; virtual;
    procedure FillRecord(const Value: TFTPListRec); virtual;
  public
    {:Constructor. You not need create this object, it is created by TFTPSend
     class as their property.}
    constructor Create;
    destructor Destroy; override;

    {:Clear list.}
    procedure Clear; virtual;

    {:count of holded @link(TFTPListRec) objects}
    function Count: integer; virtual;

    {:Assigns one list to another}
    procedure Assign(Value: TFTPList); virtual;

    {:try to parse raw directory listing in @link(lines) to list of
     @link(TFTPListRec).}
    procedure ParseLines; virtual;

    {:By this property you have access to list of @link(TFTPListRec).
     This is for compatibility only. Please, use @link(Items) instead.}
    property List: TList read FList;

    {:By this property you have access to list of @link(TFTPListRec).}
    property Items[Index: Integer]: TFTPListRec read GetListItem; default;

    {:Set of lines with RAW directory listing for @link(parseLines)}
    property Lines: TStringList read FLines;

    {:Set of masks for directory listing parser. It is predefined by default,
    however you can modify it as you need. (for example, you can add your own
    definition mask.) Mask is same as mask used in TotalCommander.}
    property Masks: TStringList read FMasks;

    {:After @link(ParseLines) it holding lines what was not sucessfully parsed.}
    property UnparsedLines: TStringList read FUnparsedLines;
  end;

  {:@abstract(Implementation of FTP protocol.)
   Note: Are you missing properties for setting Username and Password? Look to
   parent @link(TSynaClient) object! (Username and Password have default values
   for "anonymous" FTP login)

   Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}
  TFTPSend = class(TSynaClient)
  protected
    FOnStatus: TFTPStatus;
    FSock: TTCPBlockSocket;
    FDSock: TTCPBlockSocket;
    FResultCode: Integer;
    FResultString: string;
    FFullResult: TStringList;
    FAccount: string;
    FFWHost: string;
    FFWPort: string;
    FFWUsername: string;
    FFWPassword: string;
    FFWMode: integer;
    FDataStream: TMemoryStream;
    FDataIP: string;
    FDataPort: string;
    FDirectFile: Boolean;
    FDirectFileName: string;
    FCanResume: Boolean;
    FPassiveMode: Boolean;
    FForceDefaultPort: Boolean;
    FForceOldPort: Boolean;
    FFtpList: TFTPList;
    FBinaryMode: Boolean;
    FAutoTLS: Boolean;
    FIsTLS: Boolean;
    FIsDataTLS: Boolean;
    FTLSonData: Boolean;
    FFullSSL: Boolean;
    function Auth(Mode: integer): Boolean; virtual;
    function Connect: Boolean; virtual;
    function InternalStor(const Command: string; RestoreAt: int64): Boolean; virtual;
    function DataSocket: Boolean; virtual;
    function AcceptDataSocket: Boolean; virtual;
    procedure DoStatus(Response: Boolean; const Value: string); virtual;
  public
    {:Custom definition of login sequence. You can use this when you set
     @link(FWMode) to value -1.}
    CustomLogon: TLogonActions;

    constructor Create;
    destructor Destroy; override;

    {:Waits and read FTP server response. You need this only in special cases!}
    function ReadResult: Integer; virtual;

    {:Parse remote side information of data channel from value string (returned
     by PASV command). This function you need only in special cases!}
    procedure ParseRemote(Value: string); virtual;

    {:Parse remote side information of data channel from value string (returned
     by EPSV command). This function you need only in special cases!}
    procedure ParseRemoteEPSV(Value: string); virtual;

    {:Send Value as FTP command to FTP server. Returned result code is result of
     this function.
     This command is good for sending site specific command, or non-standard
     commands.}
    function FTPCommand(const Value: string): integer; virtual;

    {:Connect and logon to FTP server. If you specify any FireWall, connect to
     firewall and throw them connect to FTP server. Login sequence depending on
     @link(FWMode).}
    function Login: Boolean; virtual;

    {:Logoff and disconnect from FTP server.}
    function Logout: Boolean; virtual;

    {:Break current transmission of data. (You can call this method from
     Sock.OnStatus event, or from another thread.)}
    procedure Abort; virtual;

    {:Break current transmission of data. It is same as Abort, but it send abort
     telnet commands prior ABOR FTP command. Some servers need it. (You can call
     this method from Sock.OnStatus event, or from another thread.)}
    procedure TelnetAbort; virtual;

    {:Download directory listing of Directory on FTP server. If Directory is
     empty string, download listing of current working directory.
     If NameList is @true, download only names of files in directory.
     (internally use NLST command instead LIST command)
     If NameList is @false, returned list is also parsed to @link(FTPList)
     property.}
    function List(Directory: string; NameList: Boolean): Boolean; virtual;

    {:Read data from FileName on FTP server. If Restore is @true and server
     supports resume dowloads, download is resumed. (received is only rest
     of file)}
    function RetrieveFile(const FileName: string; Restore: Boolean): Boolean; virtual;

    {:Send data to FileName on FTP server. If Restore is @true and server
     supports resume upload, upload is resumed. (send only rest of file)
     In this case if remote file is same length as local file, nothing will be
     done. If remote file is larger then local, resume is disabled and file is
     transfered from begin!}
    function StoreFile(const FileName: string; Restore: Boolean): Boolean; virtual;

    {:Send data to FTP server and assing unique name for this file.}
    function StoreUniqueFile: Boolean; virtual;

    {:Append data to FileName on FTP server.}
    function AppendFile(const FileName: string): Boolean; virtual;

    {:Rename on FTP server file with OldName to NewName.}
    function RenameFile(const OldName, NewName: string): Boolean; virtual;

    {:Delete file FileName on FTP server.}
    function DeleteFile(const FileName: string): Boolean; virtual;

    {:Return size of Filename file on FTP server. If command failed (i.e. not
     implemented), return -1.}
    function FileSize(const FileName: string): int64; virtual;

    {:Send NOOP command to FTP server for preserve of disconnect by inactivity
     timeout.}
    function NoOp: Boolean; virtual;

    {:Change currect working directory to Directory on FTP server.}
    function ChangeWorkingDir(const Directory: string): Boolean; virtual;

    {:walk to upper directory on FTP server.}
    function ChangeToParentDir: Boolean; virtual;

    {:walk to root directory on FTP server. (May not work with all servers properly!)}
    function ChangeToRootDir: Boolean; virtual;

    {:Delete Directory on FTP server.}
    function DeleteDir(const Directory: string): Boolean; virtual;

    {:Create Directory on FTP server.}
    function CreateDir(const Directory: string): Boolean; virtual;

    {:Return current working directory on FTP server.}
    function GetCurrentDir: String; virtual;

    {:Establish data channel to FTP server and retrieve data.
     This function you need only in special cases, i.e. when you need to implement
     some special unsupported FTP command!}
    function DataRead(const DestStream: TStream): Boolean; virtual;

    {:Establish data channel to FTP server and send data.
     This function you need only in special cases, i.e. when you need to implement
     some special unsupported FTP command.}
    function DataWrite(const SourceStream: TStream): Boolean; virtual;
  published
    {:After FTP command contains result number of this operation.}
    property ResultCode: Integer read FResultCode;

    {:After FTP command contains main line of result.}
    property ResultString: string read FResultString;

    {:After any FTP command it contains all lines of FTP server reply.}
    property FullResult: TStringList read FFullResult;

    {:Account information used in some cases inside login sequence.}
    property Account: string read FAccount Write FAccount;

    {:Address of firewall. If empty string (default), firewall not used.}
    property FWHost: string read FFWHost Write FFWHost;

    {:port of firewall. standard value is same port as ftp server used. (21)}
    property FWPort: string read FFWPort Write FFWPort;

    {:Username for login to firewall. (if needed)}
    property FWUsername: string read FFWUsername Write FFWUsername;

    {:password for login to firewall. (if needed)}
    property FWPassword: string read FFWPassword Write FFWPassword;

    {:Type of Firewall. Used only if you set some firewall address. Supported
     predefined firewall login sequences are described by comments in source
     file where you can see pseudocode decribing each sequence.}
    property FWMode: integer read FFWMode Write FFWMode;

    {:Socket object used for TCP/IP operation on control channel. Good for
     seting OnStatus hook, etc.}
    property Sock: TTCPBlockSocket read FSock;

    {:Socket object used for TCP/IP operation on data channel. Good for seting
     OnStatus hook, etc.}
    property DSock: TTCPBlockSocket read FDSock;

    {:If you not use @link(DirectFile) mode, all data transfers is made to or
     from this stream.}
    property DataStream: TMemoryStream read FDataStream;

    {:After data connection is established, contains remote side IP of this
     connection.}
    property DataIP: string read FDataIP;

    {:After data connection is established, contains remote side port of this
     connection.}
    property DataPort: string read FDataPort;

    {:Mode of data handling by data connection. If @False, all data operations
     are made to or from @link(DataStream) TMemoryStream.
     If @true, data operations is made directly to file in your disk. (filename
     is specified by @link(DirectFileName) property.) Dafault is @False!}
    property DirectFile: Boolean read FDirectFile Write FDirectFile;

    {:Filename for direct disk data operations.}
    property DirectFileName: string read FDirectFileName Write FDirectFileName;

    {:Indicate after @link(Login) if remote server support resume downloads and
     uploads.}
    property CanResume: Boolean read FCanResume;

    {:If true (default value), all transfers is made by passive method.
     It is safer method for various firewalls.}
    property PassiveMode: Boolean read FPassiveMode Write FPassiveMode;

    {:Force to listen for dataconnection on standard port (20). Default is @false,
     dataconnections will be made to any non-standard port reported by PORT FTP
     command. This setting is not used, if you use passive mode.}
    property ForceDefaultPort: Boolean read FForceDefaultPort Write FForceDefaultPort;

    {:When is @true, then is disabled EPSV and EPRT support. However without this
     commands you cannot use IPv6! (Disabling of this commands is needed only
     when you are behind some crap firewall/NAT.}
    property ForceOldPort: Boolean read FForceOldPort Write FForceOldPort;

    {:You may set this hook for monitoring FTP commands and replies.}
    property OnStatus: TFTPStatus read FOnStatus write FOnStatus;

    {:After LIST command is here parsed list of files in given directory.}
    property FtpList: TFTPList read FFtpList;

    {:if @true (default), then data transfers is in binary mode. If this is set
     to @false, then ASCII mode is used.}
    property BinaryMode: Boolean read FBinaryMode Write FBinaryMode;

    {:if is true, then if server support upgrade to SSL/TLS mode, then use them.}
    property AutoTLS: Boolean read FAutoTLS Write FAutoTLS;

    {:if server listen on SSL/TLS port, then you set this to true.}
    property FullSSL: Boolean read FFullSSL Write FFullSSL;

    {:Signalise, if control channel is in SSL/TLS mode.}
    property IsTLS: Boolean read FIsTLS;

    {:Signalise, if data transfers is in SSL/TLS mode.}
    property IsDataTLS: Boolean read FIsDataTLS;

    {:If @true (default), then try to use SSL/TLS on data transfers too.
     If @false, then SSL/TLS is used only for control connection.}
    property TLSonData: Boolean read FTLSonData write FTLSonData;
  end;

{:A very useful function, and example of use can be found in the TFtpSend object.
 Dowload specified file from FTP server to LocalFile.}
function FtpGetFile(const IP, Port, FileName, LocalFile,
  User, Pass: string): Boolean;

{:A very useful function, and example of use can be found in the TFtpSend object.
 Upload specified LocalFile to FTP server.}
function FtpPutFile(const IP, Port, FileName, LocalFile,
  User, Pass: string): Boolean;

{:A very useful function, and example of use can be found in the TFtpSend object.
 Initiate transfer of file between two FTP servers.}
function FtpInterServerTransfer(
  const FromIP, FromPort, FromFile, FromUser, FromPass: string;
  const ToIP, ToPort, ToFile, ToUser, ToPass: string): Boolean;

implementation

constructor TFTPSend.Create;
begin
  inherited Create;
  FFullResult := TStringList.Create;
  FDataStream := TMemoryStream.Create;
  FSock := TTCPBlockSocket.Create;
  FSock.Owner := self;
  FSock.ConvertLineEnd := True;
  FDSock := TTCPBlockSocket.Create;
  FDSock.Owner := self;
  FFtpList := TFTPList.Create;
  FTimeout := 300000;
  FTargetPort := cFtpProtocol;
  FUsername := 'anonymous';
  FPassword := 'anonymous@' + FSock.LocalName;
  FDirectFile := False;
  FPassiveMode := True;
  FForceDefaultPort := False;
  FForceOldPort := false;
  FAccount := '';
  FFWHost := '';
  FFWPort := cFtpProtocol;
  FFWUsername := '';
  FFWPassword := '';
  FFWMode := 0;
  FBinaryMode := True;
  FAutoTLS := False;
  FFullSSL := False;
  FIsTLS := False;
  FIsDataTLS := False;
  FTLSonData := True;
end;

destructor TFTPSend.Destroy;
begin
  FDSock.Free;
  FSock.Free;
  FFTPList.Free;
  FDataStream.Free;
  FFullResult.Free;
  inherited Destroy;
end;

procedure TFTPSend.DoStatus(Response: Boolean; const Value: string);
begin
  if assigned(OnStatus) then
    OnStatus(Self, Response, Value);
end;

function TFTPSend.ReadResult: Integer;
var
  s, c: AnsiString;
begin
  FFullResult.Clear;
  c := '';
  repeat
    s := FSock.RecvString(FTimeout);
    if c = '' then
      if length(s) > 3 then
        if s[4] in [' ', '-'] then
          c :=Copy(s, 1, 3);
    FResultString := s;
    FFullResult.Add(s);
    DoStatus(True, s);
    if FSock.LastError <> 0 then
      Break;
  until (c <> '') and (Pos(c + ' ', s) = 1);
  Result := StrToIntDef(c, 0);
  FResultCode := Result;
end;

function TFTPSend.FTPCommand(const Value: string): integer;
begin
  FSock.Purge;
  FSock.SendString(Value + CRLF);
  DoStatus(False, Value);
  Result := ReadResult;
end;

// based on idea by Petr Esner <petr.esner@atlas.cz>
function TFTPSend.Auth(Mode: integer): Boolean;
const
  //if not USER <username> then
  //  if not PASS <password> then
  //    if not ACCT <account> then ERROR!
  //OK!
  Action0: TLogonActions =
    (0, FTP_OK, 3,
     1, FTP_OK, 6,
     2, FTP_OK, FTP_ERR,
     0, 0, 0, 0, 0, 0, 0, 0, 0);

  //if not USER <FWusername> then
  //  if not PASS <FWPassword> then ERROR!
  //if SITE <FTPServer> then ERROR!
  //if not USER <username> then
  //  if not PASS <password> then
  //    if not ACCT <account> then ERROR!
  //OK!
  Action1: TLogonActions =
    (3, 6, 3,
     4, 6, FTP_ERR,
     5, FTP_ERR, 9,
     0, FTP_OK, 12,
     1, FTP_OK, 15,
     2, FTP_OK, FTP_ERR);

  //if not USER <FWusername> then
  //  if not PASS <FWPassword> then ERROR!
  //if USER <UserName>'@'<FTPServer> then OK!
  //if not PASS <password> then
  //  if not ACCT <account> then ERROR!
  //OK!
  Action2: TLogonActions =
    (3, 6, 3,
     4, 6, FTP_ERR,
     6, FTP_OK, 9,
     1, FTP_OK, 12,
     2, FTP_OK, FTP_ERR,
     0, 0, 0);

  //if not USER <FWusername> then
  //  if not PASS <FWPassword> then ERROR!
  //if not USER <username> then
  //  if not PASS <password> then
  //    if not ACCT <account> then ERROR!
  //OK!
  Action3: TLogonActions =
    (3, 6, 3,
     4, 6, FTP_ERR,
     0, FTP_OK, 9,
     1, FTP_OK, 12,
     2, FTP_OK, FTP_ERR,
     0, 0, 0);

  //OPEN <FTPserver>
  //if not USER <username> then
  //  if not PASS <password> then
  //    if not ACCT <account> then ERROR!
  //OK!
  Action4: TLogonActions =
    (7, 3, 3,
     0, FTP_OK, 6,
     1, FTP_OK, 9,
     2, FTP_OK, FTP_ERR,
     0, 0, 0, 0, 0, 0);

  //if USER <UserName>'@'<FTPServer> then OK!
  //if not PASS <password> then
  //  if not ACCT <account> then ERROR!
  //OK!
  Action5: TLogonActions =
    (6, FTP_OK, 3,
     1, FTP_OK, 6,
     2, FTP_OK, FTP_ERR,
     0, 0, 0, 0, 0, 0, 0, 0, 0);

  //if not USER <FWUserName>@<FTPServer> then
  //  if not PASS <FWPassword> then ERROR!
  //if not USER <username> then
  //  if not PASS <password> then
  //    if not ACCT <account> then ERROR!
  //OK!
  Action6: TLogonActions =
    (8, 6, 3,
     4, 6, FTP_ERR,
     0, FTP_OK, 9,
     1, FTP_OK, 12,
     2, FTP_OK, FTP_ERR,
     0, 0, 0);

  //if USER <UserName>@<FTPServer> <FWUserName> then ERROR!
  //if not PASS <password> then
  //  if not ACCT <account> then ERROR!
  //OK!
  Action7: TLogonActions =
    (9, FTP_ERR, 3,
     1, FTP_OK, 6,
     2, FTP_OK, FTP_ERR,
     0, 0, 0, 0, 0, 0, 0, 0, 0);

  //if not USER <UserName>@<FWUserName>@<FTPServer> then
  //  if not PASS <Password>@<FWPassword> then
  //    if not ACCT <account> then ERROR!
  //OK!
  Action8: TLogonActions =
    (10, FTP_OK, 3,
     11, FTP_OK, 6,
     2, FTP_OK, FTP_ERR,
     0, 0, 0, 0, 0, 0, 0, 0, 0);
var
  FTPServer: string;
  LogonActions: TLogonActions;
  i: integer;
  s: string;
  x: integer;
begin
  Result := False;
  if FFWHost = '' then
    Mode := 0;
  if (FTargetPort = cFtpProtocol) or (FTargetPort = '21') then
    FTPServer := FTargetHost
  else
    FTPServer := FTargetHost + ':' + FTargetPort;
  case Mode of
    -1:
      LogonActions := CustomLogon;
    1:
      LogonActions := Action1;
    2:
      LogonActions := Action2;
    3:
      LogonActions := Action3;
    4:
      LogonActions := Action4;
    5:
      LogonActions := Action5;
    6:
      LogonActions := Action6;
    7:
      LogonActions := Action7;
    8:
      LogonActions := Action8;
  else
    LogonActions := Action0;
  end;
  i := 0;
  repeat
    case LogonActions[i] of
      0:  s := 'USER ' + FUserName;
      1:  s := 'PASS ' + FPassword;
      2:  s := 'ACCT ' + FAccount;
      3:  s := 'USER ' + FFWUserName;
      4:  s := 'PASS ' + FFWPassword;
      5:  s := 'SITE ' + FTPServer;
      6:  s := 'USER ' + FUserName + '@' + FTPServer;
      7:  s := 'OPEN ' + FTPServer;
      8:  s := 'USER ' + FFWUserName + '@' + FTPServer;
      9:  s := 'USER ' + FUserName + '@' + FTPServer + ' ' + FFWUserName;
      10: s := 'USER ' + FUserName + '@' + FFWUserName + '@' + FTPServer;
      11: s := 'PASS ' + FPassword + '@' + FFWPassword;
    end;
    x := FTPCommand(s);
    x := x div 100;
    if (x <> 2) and (x <> 3) then
      Exit;
    i := LogonActions[i + x - 1];
    case i of
      FTP_ERR:
        Exit;
      FTP_OK:
        begin
          Result := True;
          Exit;
        end;
    end;
  until False;
end;


function TFTPSend.Connect: Boolean;
begin
  FSock.CloseSocket;
  FSock.Bind(FIPInterface, cAnyPort);
  if FSock.LastError = 0 then
    if FFWHost = '' then
      FSock.Connect(FTargetHost, FTargetPort)
    else
      FSock.Connect(FFWHost, FFWPort);
  if FSock.LastError = 0 then
    if FFullSSL then
      FSock.SSLDoConnect;
  Result := FSock.LastError = 0;
end;

function TFTPSend.Login: Boolean;
var
  x: integer;
begin
  Result := False;
  FCanResume := False;
  if not Connect then
    Exit;
  FIsTLS := FFullSSL;
  FIsDataTLS := False;
  repeat
    x := ReadResult div 100;
  until x <> 1;
  if x <> 2 then
    Exit;
  if FAutoTLS and not(FIsTLS) then
    if (FTPCommand('AUTH TLS') div 100) = 2 then
    begin
      FSock.SSLDoConnect;
      FIsTLS := FSock.LastError = 0;
      if not FIsTLS then
      begin
        Result := False;
        Exit;
      end;
    end;
  if not Auth(FFWMode) then
    Exit;
  if FIsTLS then
  begin
    FTPCommand('PBSZ 0');
    if FTLSonData then
      FIsDataTLS := (FTPCommand('PROT P') div 100) = 2;
    if not FIsDataTLS then
      FTPCommand('PROT C');
  end;
  FTPCommand('TYPE I');
  FTPCommand('STRU F');
  FTPCommand('MODE S');
  if FTPCommand('REST 0') = 350 then
    if FTPCommand('REST 1') = 350 then
    begin
      FTPCommand('REST 0');
      FCanResume := True;
    end;
  Result := True;
end;

function TFTPSend.Logout: Boolean;
begin
  Result := (FTPCommand('QUIT') div 100) = 2;
  FSock.CloseSocket;
end;

procedure TFTPSend.ParseRemote(Value: string);
var
  n: integer;
  nb, ne: integer;
  s: string;
  x: integer;
begin
  Value := trim(Value);
  nb := Pos('(',Value);
  ne := Pos(')',Value);
  if (nb = 0) or (ne = 0) then
  begin
    nb:=RPos(' ',Value);
    s:=Copy(Value, nb + 1, Length(Value) - nb);
  end
  else
  begin
    s:=Copy(Value,nb+1,ne-nb-1);
  end;
  for n := 1 to 4 do
    if n = 1 then
      FDataIP := Fetch(s, ',')
    else
      FDataIP := FDataIP + '.' + Fetch(s, ',');
  x := StrToIntDef(Fetch(s, ','), 0) * 256;
  x := x + StrToIntDef(Fetch(s, ','), 0);
  FDataPort := IntToStr(x);
end;

procedure TFTPSend.ParseRemoteEPSV(Value: string);
var
  n: integer;
  s, v: AnsiString;
begin
  s := SeparateRight(Value, '(');
  s := Trim(SeparateLeft(s, ')'));
  Delete(s, Length(s), 1);
  v := '';
  for n := Length(s) downto 1 do
    if s[n] in ['0'..'9'] then
      v := s[n] + v
    else
      Break;
  FDataPort := v;
  FDataIP := FTargetHost;
end;

function TFTPSend.DataSocket: boolean;
var
  s: string;
begin
  Result := False;
  if FIsDataTLS then
    FPassiveMode := True;
  if FPassiveMode then
  begin
    if FSock.IP6used then
      s := '2'
    else
      s := '1';
    if FSock.IP6used and not(FForceOldPort) and ((FTPCommand('EPSV ' + s) div 100) = 2) then
    begin
      ParseRemoteEPSV(FResultString);
    end
    else
      if FSock.IP6used then
        Exit
      else
      begin
        if (FTPCommand('PASV') div 100) <> 2 then
          Exit;
        ParseRemote(FResultString);
      end;
    FDSock.CloseSocket;
    FDSock.Bind(FIPInterface, cAnyPort);
    FDSock.Connect(FDataIP, FDataPort);
    Result := FDSock.LastError = 0;
  end
  else
  begin
    FDSock.CloseSocket;
    if FForceDefaultPort then
      s := cFtpDataProtocol
    else
      s := '0';
    //data conection from same interface as command connection
    FDSock.Bind(FSock.GetLocalSinIP, s);
    if FDSock.LastError <> 0 then
      Exit;
    FDSock.SetLinger(True, 10000);
    FDSock.Listen;
    FDSock.GetSins;
    FDataIP := FDSock.GetLocalSinIP;
    FDataIP := FDSock.ResolveName(FDataIP);
    FDataPort := IntToStr(FDSock.GetLocalSinPort);
    if FSock.IP6used and (not FForceOldPort) then
    begin
      if IsIp6(FDataIP) then
        s := '2'
      else
        s := '1';
      s := 'EPRT |' + s +'|' + FDataIP + '|' + FDataPort + '|';
      Result := (FTPCommand(s) div 100) = 2;
    end;
    if not Result and IsIP(FDataIP) then
    begin
      s := ReplaceString(FDataIP, '.', ',');
      s := 'PORT ' + s + ',' + IntToStr(FDSock.GetLocalSinPort div 256)
        + ',' + IntToStr(FDSock.GetLocalSinPort mod 256);
      Result := (FTPCommand(s) div 100) = 2;
    end;
  end;
end;

function TFTPSend.AcceptDataSocket: Boolean;
var
  x: TSocket;
begin
  if FPassiveMode then
    Result := True
  else
  begin
    Result := False;
    if FDSock.CanRead(FTimeout) then
    begin
      x := FDSock.Accept;
      if not FDSock.UsingSocks then
        FDSock.CloseSocket;
      FDSock.Socket := x;
      Result := True;
    end;
  end;
  if Result and FIsDataTLS then
  begin
    FDSock.SSL.Assign(FSock.SSL);
    FDSock.SSLDoConnect;
    Result := FDSock.LastError = 0;
  end;
end;

function TFTPSend.DataRead(const DestStream: TStream): Boolean;
var
  x: integer;
begin
  Result := False;
  try
    if not AcceptDataSocket then
      Exit;
    FDSock.RecvStreamRaw(DestStream, FTimeout);
    FDSock.CloseSocket;
    x := ReadResult;
    Result := (x div 100) = 2;
  finally
    FDSock.CloseSocket;
  end;
end;

function TFTPSend.DataWrite(const SourceStream: TStream): Boolean;
var
  x: integer;
  b: Boolean;
begin
  Result := False;
  try
    if not AcceptDataSocket then
      Exit;
    FDSock.SendStreamRaw(SourceStream);
    b := FDSock.LastError = 0;
    FDSock.CloseSocket;
    x := ReadResult;
    Result := b and ((x div 100) = 2);
  finally
    FDSock.CloseSocket;
  end;
end;

function TFTPSend.List(Directory: string; NameList: Boolean): Boolean;
var
  x: integer;
begin
  Result := False;
  FDataStream.Clear;
  FFTPList.Clear;
  if Directory <> '' then
    Directory := ' ' + Directory;
  FTPCommand('TYPE A');
  if not DataSocket then
    Exit;
  if NameList then
    x := FTPCommand('NLST' + Directory)
  else
    x := FTPCommand('LIST' + Directory);
  if (x div 100) <> 1 then
    Exit;
  Result := DataRead(FDataStream);
  if (not NameList) and Result then
  begin
    FDataStream.Position := 0;
    FFTPList.Lines.LoadFromStream(FDataStream);
    FFTPList.ParseLines;
  end;
  FDataStream.Position := 0;
end;

function TFTPSend.RetrieveFile(const FileName: string; Restore: Boolean): Boolean;
var
  RetrStream: TStream;
begin
  Result := False;
  if FileName = '' then
    Exit;
  if not DataSocket then
    Exit;
  Restore := Restore and FCanResume;
  if FDirectFile then
    if Restore and FileExists(FDirectFileName) then
      RetrStream := TFileStream.Create(FDirectFileName,
        fmOpenReadWrite  or fmShareExclusive)
    else
      RetrStream := TFileStream.Create(FDirectFileName,
        fmCreate or fmShareDenyWrite)
  else
    RetrStream := FDataStream;
  try
    if FBinaryMode then
      FTPCommand('TYPE I')
    else
      FTPCommand('TYPE A');
    if Restore then
    begin
      RetrStream.Position := RetrStream.Size;
      if (FTPCommand('REST ' + IntToStr(RetrStream.Size)) div 100) <> 3 then
        Exit;
    end
    else
      if RetrStream is TMemoryStream then
        TMemoryStream(RetrStream).Clear;
    if (FTPCommand('RETR ' + FileName) div 100) <> 1 then
      Exit;
    Result := DataRead(RetrStream);
    if not FDirectFile then
      RetrStream.Position := 0;
  finally
    if FDirectFile then
      RetrStream.Free;
  end;
end;

function TFTPSend.InternalStor(const Command: string; RestoreAt: int64): Boolean;
var
  SendStream: TStream;
  StorSize: int64;
begin
  Result := False;
  if FDirectFile then
    if not FileExists(FDirectFileName) then
      Exit
    else
      SendStream := TFileStream.Create(FDirectFileName,
        fmOpenRead or fmShareDenyWrite)
  else
    SendStream := FDataStream;
  try
    if not DataSocket then
      Exit;
    if FBinaryMode then
      FTPCommand('TYPE I')
    else
      FTPCommand('TYPE A');
    StorSize := SendStream.Size;
    if not FCanResume then
      RestoreAt := 0;
    if (StorSize > 0) and (RestoreAt = StorSize) then
    begin
      Result := True;
      Exit;
    end;
    if RestoreAt > StorSize then
      RestoreAt := 0;
    FTPCommand('ALLO ' + IntToStr(StorSize - RestoreAt));
    if FCanResume then
      if (FTPCommand('REST ' + IntToStr(RestoreAt)) div 100) <> 3 then
        Exit;
    SendStream.Position := RestoreAt;
    if (FTPCommand(Command) div 100) <> 1 then
      Exit;
    Result := DataWrite(SendStream);
  finally
    if FDirectFile then
      SendStream.Free;
  end;
end;

function TFTPSend.StoreFile(const FileName: string; Restore: Boolean): Boolean;
var
  RestoreAt: int64;
begin
  Result := False;
  if FileName = '' then
    Exit;
  RestoreAt := 0;
  Restore := Restore and FCanResume;
  if Restore then
  begin
    RestoreAt := Self.FileSize(FileName);
    if RestoreAt < 0 then
      RestoreAt := 0;
  end;
  Result := InternalStor('STOR ' + FileName, RestoreAt);
end;

function TFTPSend.StoreUniqueFile: Boolean;
begin
  Result := InternalStor('STOU', 0);
end;

function TFTPSend.AppendFile(const FileName: string): Boolean;
begin
  Result := False;
  if FileName = '' then
    Exit;
  Result := InternalStor('APPE ' + FileName, 0);
end;

function TFTPSend.NoOp: Boolean;
begin
  Result := (FTPCommand('NOOP') div 100) = 2;
end;

function TFTPSend.RenameFile(const OldName, NewName: string): Boolean;
begin
  Result := False;
  if (FTPCommand('RNFR ' + OldName) div 100) <> 3  then
    Exit;
  Result := (FTPCommand('RNTO ' + NewName) div 100) = 2;
end;

function TFTPSend.DeleteFile(const FileName: string): Boolean;
begin
  Result := (FTPCommand('DELE ' + FileName) div 100) = 2;
end;

function TFTPSend.FileSize(const FileName: string): int64;
var
  s: string;
begin
  Result := -1;
  if (FTPCommand('SIZE ' + FileName) div 100) = 2 then
  begin
    s := Trim(SeparateRight(ResultString, ' '));
    s := Trim(SeparateLeft(s, ' '));
    {$IFDEF VER100}
      Result := StrToIntDef(s, -1);
    {$ELSE}
      Result := StrToInt64Def(s, -1);
    {$ENDIF}
  end;
end;

function TFTPSend.ChangeWorkingDir(const Directory: string): Boolean;
begin
  Result := (FTPCommand('CWD ' + Directory) div 100) = 2;
end;

function TFTPSend.ChangeToParentDir: Boolean;
begin
  Result := (FTPCommand('CDUP') div 100) = 2;
end;

function TFTPSend.ChangeToRootDir: Boolean;
begin
  Result := ChangeWorkingDir('/');
end;

function TFTPSend.DeleteDir(const Directory: string): Boolean;
begin
  Result := (FTPCommand('RMD ' + Directory) div 100) = 2;
end;

function TFTPSend.CreateDir(const Directory: string): Boolean;
begin
  Result := (FTPCommand('MKD ' + Directory) div 100) = 2;
end;

function TFTPSend.GetCurrentDir: String;
begin
  Result := '';
  if (FTPCommand('PWD') div 100) = 2 then
  begin
    Result := SeparateRight(FResultString, '"');
    Result := Trim(Separateleft(Result, '"'));
  end;
end;

procedure TFTPSend.Abort;
begin
  FSock.SendString('ABOR' + CRLF);
  FDSock.StopFlag := True;
end;

procedure TFTPSend.TelnetAbort;
begin
  FSock.SendString(#$FF + #$F4 + #$FF + #$F2);
  Abort;
end;

{==============================================================================}

procedure TFTPListRec.Assign(Value: TFTPListRec);
begin
  FFileName := Value.FileName;
  FDirectory := Value.Directory;
  FReadable := Value.Readable;
  FFileSize := Value.FileSize;
  FFileTime := Value.FileTime;
  FOriginalLine := Value.OriginalLine;
  FMask := Value.Mask;
end;

constructor TFTPList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FLines := TStringList.Create;
  FMasks := TStringList.Create;
  FUnparsedLines := TStringList.Create;
  //various UNIX
  FMasks.add('pppppppppp $!!!S*$TTT$DD$hh mm ss$YYYY$n*');
  FMasks.add('pppppppppp $!!!S*$DD$TTT$hh mm ss$YYYY$n*');
  FMasks.add('pppppppppp $!!!S*$TTT$DD$UUUUU$n*');  //mostly used UNIX format
  FMasks.add('pppppppppp $!!!S*$DD$TTT$UUUUU$n*');
  //MacOS
  FMasks.add('pppppppppp $!!S*$TTT$DD$UUUUU$n*');
  FMasks.add('pppppppppp $!S*$TTT$DD$UUUUU$n*');
  //Novell
  FMasks.add('d            $!S*$TTT$DD$UUUUU$n*');
  //Windows
  FMasks.add('MM DD YY  hh mmH !S* n*');
  FMasks.add('MM DD YY  hh mmH $ d!n*');
  FMasks.add('MM DD YYYY  hh mmH !S* n*');
  FMasks.add('MM DD YYYY  hh mmH $ d!n*');
  FMasks.add('DD MM YYYY  hh mmH !S* n*');
  FMasks.add('DD MM YYYY  hh mmH $ d!n*');
  //VMS
  FMasks.add('v*$  DD TTT YYYY hh mm');
  FMasks.add('v*$!DD TTT YYYY hh mm');
  FMasks.add('n*$                 YYYY MM DD hh mm$S*');
  //AS400
  FMasks.add('!S*$MM DD YY hh mm ss !n*');
  FMasks.add('!S*$DD MM YY hh mm ss !n*');
  FMasks.add('n*!S*$MM DD YY hh mm ss d');
  FMasks.add('n*!S*$DD MM YY hh mm ss d');
  //VxWorks
  FMasks.add('$S*    TTT DD YYYY  hh mm ss $n* $ d');
  FMasks.add('$S*    TTT DD YYYY  hh mm ss $n*');
  //Distinct
  FMasks.add('d    $S*$TTT DD YYYY  hh mm$n*');
  FMasks.add('d    $S*$TTT DD$hh mm$n*');
  //PC-NFSD
  FMasks.add('nnnnnnnn.nnn  dSSSSSSSSSSS MM DD YY  hh mmH');
  //VOS
  FMasks.add('-   SSSSS            YY MM DD hh mm ss  n*');
  FMasks.add('- d=  SSSSS  YY MM DD hh mm ss  n*');
  //Unissys ClearPath
  FMasks.add('nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn               SSSSSSSSS MM DD YYYY hh mm');
  FMasks.add('n*\x                                               SSSSSSSSS MM DD YYYY hh mm');
  //IBM
  FMasks.add('-     SSSSSSSSSSSS           d   MM DD YYYY   hh mm  n*');
  //OS9
  FMasks.add('-         YY MM DD hhmm d                        SSSSSSSSS n*');
  //tandem
  FMasks.add('nnnnnnnn                   SSSSSSS DD TTT YY hh mm ss');
  //MVS
  FMasks.add('-             YYYY MM DD                     SSSSS   d=O n*');
  //BullGCOS8
  FMasks.add('             $S* MM DD YY hh mm ss  !n*');
  FMasks.add('d            $S* MM DD YY           !n*');
  //BullGCOS7
  FMasks.add('                                         TTT DD  YYYY n*');
  FMasks.add('  d                                                   n*');
end;

destructor TFTPList.Destroy;
begin
  Clear;
  FList.Free;
  FLines.Free;
  FMasks.Free;
  FUnparsedLines.Free;
  inherited Destroy;
end;

procedure TFTPList.Clear;
var
  n:integer;
begin
  for n := 0 to FList.Count - 1 do
    if Assigned(FList[n]) then
      TFTPListRec(FList[n]).Free;
  FList.Clear;
  FLines.Clear;
  FUnparsedLines.Clear;
end;

function TFTPList.Count: integer;
begin
  Result := FList.Count;
end;

function TFTPList.GetListItem(Index: integer): TFTPListRec;
begin
  Result := nil;
  if Index < Count then
    Result := TFTPListRec(FList[Index]);
end;

procedure TFTPList.Assign(Value: TFTPList);
var
  flr: TFTPListRec;
  n: integer;
begin
  Clear;
  for n := 0 to Value.Count - 1 do
  begin
    flr := TFTPListRec.Create;
    flr.Assign(Value[n]);
    Flist.Add(flr);
  end;
  Lines.Assign(Value.Lines);
  Masks.Assign(Value.Masks);
  UnparsedLines.Assign(Value.UnparsedLines);
end;

procedure TFTPList.ClearStore;
begin
  Monthnames := '';
  BlockSize := '';
  DirFlagValue := '';
  FileName := '';
  VMSFileName := '';
  Day := '';
  Month := '';
  ThreeMonth := '';
  YearTime := '';
  Year := '';
  Hours := '';
  HoursModif := '';
  Minutes := '';
  Seconds := '';
  Size := '';
  Permissions := '';
  DirFlag := '';
end;

function TFTPList.ParseByMask(Value, NextValue, Mask: AnsiString): Integer;
var
  Ivalue, IMask: integer;
  MaskC, LastMaskC: AnsiChar;
  c: AnsiChar;
  s: string;
begin
  ClearStore;
  Result := 0;
  if Value = '' then
    Exit;
  if Mask = '' then
    Exit;
  Ivalue := 1;
  IMask := 1;
  Result := 1;
  LastMaskC := ' ';
  while Imask <= Length(mask) do
  begin
    if (Mask[Imask] <> '*') and (Ivalue > Length(Value)) then
    begin
      Result := 0;
      Exit;
    end;
    MaskC := Mask[Imask];
    if Ivalue > Length(Value) then
      Exit;
    c := Value[Ivalue];
    case MaskC of
      'n':
        FileName := FileName + c;
      'v':
        VMSFileName := VMSFileName + c;
      '.':
        begin
          if c in ['.', ' '] then
            FileName := TrimSP(FileName) + '.'
          else
          begin
            Result := 0;
            Exit;
          end;
        end;
      'D':
        Day := Day + c;
      'M':
        Month := Month + c;
      'T':
        ThreeMonth := ThreeMonth + c;
      'U':
        YearTime := YearTime + c;
      'Y':
        Year := Year + c;
      'h':
        Hours := Hours + c;
      'H':
        HoursModif := HoursModif + c;
      'm':
        Minutes := Minutes + c;
      's':
        Seconds := Seconds + c;
      'S':
        Size := Size + c;
      'p':
        Permissions := Permissions + c;
      'd':
        DirFlag := DirFlag + c;
      'x':
        if c <> ' ' then
          begin
            Result := 0;
            Exit;
          end;
      '*':
        begin
          s := '';
          if LastMaskC in ['n', 'v'] then
          begin
            if Imask = Length(Mask) then
              s := Copy(Value, IValue, Maxint)
            else
              while IValue <= Length(Value) do
              begin
                if Value[Ivalue] = ' ' then
                  break;
                s := s + Value[Ivalue];
                Inc(Ivalue);
              end;
            if LastMaskC = 'n' then
              FileName := FileName + s
            else
              VMSFileName := VMSFileName + s;
          end
          else
          begin
            while IValue <= Length(Value) do
            begin
              if not(Value[Ivalue] in ['0'..'9']) then
                break;
              s := s + Value[Ivalue];
              Inc(Ivalue);
            end;
            case LastMaskC of
              'S':
                Size := Size + s;
            end;
          end;
          Dec(IValue);
        end;
      '!':
        begin
          while IValue <= Length(Value) do
          begin
            if Value[Ivalue] = ' ' then
              break;
            Inc(Ivalue);
          end;
          while IValue <= Length(Value) do
          begin
            if Value[Ivalue] <> ' ' then
              break;
            Inc(Ivalue);
          end;
          Dec(IValue);
        end;
      '$':
        begin
          while IValue <= Length(Value) do
          begin
            if not(Value[Ivalue] in [' ', #9]) then
              break;
            Inc(Ivalue);
          end;
          Dec(IValue);
        end;
      '=':
        begin
          s := '';
          case LastmaskC of
            'S':
              begin
                while Imask <= Length(Mask) do
                begin
                  if not(Mask[Imask] in ['0'..'9']) then
                    break;
                  s := s + Mask[Imask];
                  Inc(Imask);
                end;
                Dec(Imask);
                BlockSize := s;
              end;
            'T':
              begin
                Monthnames := Copy(Mask, IMask, 12 * 3);
                Inc(IMask, 12 * 3);
              end;
            'd':
              begin
                Inc(Imask);
                DirFlagValue := Mask[Imask];
              end;
          end;
        end;
      '\':
        begin
          Value := NextValue;
          IValue := 0;
          Result := 2;
        end;
    end;
    Inc(Ivalue);
    Inc(Imask);
    LastMaskC := MaskC;
  end;
end;

function TFTPList.CheckValues: Boolean;
var
  x, n: integer;
begin
  Result := false;
  if FileName <> '' then
  begin
    if pos('?', VMSFilename) > 0 then
      Exit;
    if pos('*', VMSFilename) > 0 then
      Exit;
  end;
  if VMSFileName <> '' then
    if pos(';', VMSFilename) <= 0 then
      Exit;
  if (FileName = '') and (VMSFileName = '') then
    Exit;
  if Permissions <> '' then
  begin
    if length(Permissions) <> 10 then
      Exit;
    for n := 1 to 10 do
      if not(Permissions[n] in
        ['a', 'b', 'c', 'd', 'h', 'l', 'p', 'r', 's', 't', 'w', 'x', 'y', '-']) then
        Exit;
  end;
  if Day <> '' then
  begin
    Day := TrimSP(Day);
    x := StrToIntDef(day, -1);
    if (x < 1) or (x > 31) then
      Exit;
  end;
  if Month <> '' then
  begin
    Month := TrimSP(Month);
    x := StrToIntDef(Month, -1);
    if (x < 1) or (x > 12) then
      Exit;
  end;
  if Hours <> '' then
  begin
    Hours := TrimSP(Hours);
    x := StrToIntDef(Hours, -1);
    if (x < 0) or (x > 24) then
      Exit;
  end;
  if HoursModif <> '' then
  begin
    if not (HoursModif[1] in ['a', 'A', 'p', 'P']) then
      Exit;
  end;
  if Minutes <> '' then
  begin
    Minutes := TrimSP(Minutes);
    x := StrToIntDef(Minutes, -1);
    if (x < 0) or (x > 59) then
      Exit;
  end;
  if Seconds <> '' then
  begin
    Seconds := TrimSP(Seconds);
    x := StrToIntDef(Seconds, -1);
    if (x < 0) or (x > 59) then
      Exit;
  end;
  if Size <> '' then
  begin
    Size := TrimSP(Size);
    for n := 1 to Length(Size) do
      if not (Size[n] in ['0'..'9']) then
        Exit;
  end;

  if length(Monthnames) = (12 * 3) then
    for n := 1 to 12 do
      CustomMonthNames[n] := Copy(Monthnames, ((n - 1) * 3) + 1, 3);
  if ThreeMonth <> '' then
  begin
    x := GetMonthNumber(ThreeMonth);
    if (x = 0) then
      Exit;
  end;
  if YearTime <> '' then
  begin
    YearTime := ReplaceString(YearTime, '-', ':');
    if pos(':', YearTime) > 0 then
    begin
      if (GetTimeFromstr(YearTime) = -1) then
        Exit;
    end
    else
    begin
      YearTime := TrimSP(YearTime);
      x := StrToIntDef(YearTime, -1);
      if (x = -1) then
        Exit;
      if (x < 1900) or (x > 2100) then
        Exit;
    end;
  end;
  if Year <> '' then
  begin
    Year := TrimSP(Year);
    x := StrToIntDef(Year, -1);
    if (x = -1) then
      Exit;
    if Length(Year) = 4 then
    begin
      if not((x > 1900) and (x < 2100)) then
        Exit;
    end
    else
      if Length(Year) = 2 then
      begin
        if not((x >= 0) and (x <= 99)) then
          Exit;
      end
      else
        if Length(Year) = 3 then
        begin
          if not((x >= 100) and (x <= 110)) then
            Exit;
        end
        else
          Exit;
  end;
  Result := True;
end;

procedure TFTPList.FillRecord(const Value: TFTPListRec);
var
  s: string;
  x: integer;
  myear: Word;
  mmonth: Word;
  mday: Word;
  mhours, mminutes, mseconds: word;
  n: integer;
begin
  s := DirFlagValue;
  if s = '' then
    s := 'D';
  s := Uppercase(s);
  Value.Directory :=  s = Uppercase(DirFlag);
  if FileName <> '' then
    Value.FileName := SeparateLeft(Filename, ' -> ');
  if VMSFileName <> '' then
  begin
    Value.FileName := VMSFilename;
    Value.Directory := Pos('.DIR;',VMSFilename) > 0;
  end;
  Value.FileName := TrimSPRight(Value.FileName);
  Value.Readable := not Value.Directory;
  if BlockSize <> '' then
    x := StrToIntDef(BlockSize, 1)
  else
    x := 1;
  {$IFDEF VER100}
  Value.FileSize := x * StrToIntDef(Size, 0);
  {$ELSE}
  Value.FileSize := x * StrToInt64Def(Size, 0);
  {$ENDIF}

  DecodeDate(Date,myear,mmonth,mday);
  mhours := 0;
  mminutes := 0;
  mseconds := 0;

  if Day <> '' then
    mday := StrToIntDef(day, 1);
  if Month <> '' then
    mmonth := StrToIntDef(Month, 1);
  if length(Monthnames) = (12 * 3) then
    for n := 1 to 12 do
      CustomMonthNames[n] := Copy(Monthnames, ((n - 1) * 3) + 1, 3);
  if ThreeMonth <> '' then
    mmonth := GetMonthNumber(ThreeMonth);
  if Year <> '' then
  begin
    myear := StrToIntDef(Year, 0);
    if (myear <= 99) and (myear > 50) then
      myear := myear + 1900;
    if myear <= 50 then
      myear := myear + 2000;
  end;
  if YearTime <> '' then
  begin
    if pos(':', YearTime) > 0 then
    begin
      YearTime := TrimSP(YearTime);
      mhours := StrToIntDef(Separateleft(YearTime, ':'), 0);
      mminutes := StrToIntDef(SeparateRight(YearTime, ':'), 0);
      if (Encodedate(myear, mmonth, mday)
        + EncodeTime(mHours, mminutes, 0, 0)) > now then
        Dec(mYear);
    end
    else
      myear := StrToIntDef(YearTime, 0);
  end;
  if Minutes <> '' then
    mminutes := StrToIntDef(Minutes, 0);
  if Seconds <> '' then
    mseconds := StrToIntDef(Seconds, 0);
  if Hours <> '' then
  begin
    mHours := StrToIntDef(Hours, 0);
    if HoursModif <> '' then
      if Uppercase(HoursModif[1]) = 'P' then
        if mHours <> 12 then
          mHours := MHours + 12;
  end;
  Value.FileTime := Encodedate(myear, mmonth, mday)
    + EncodeTime(mHours, mminutes, mseconds, 0);
  if Permissions <> '' then
  begin
    Value.Permission := Permissions;
    Value.Readable := Uppercase(permissions)[2] = 'R';
    if Uppercase(permissions)[1] = 'D' then
    begin
      Value.Directory := True;
      Value.Readable := false;
    end
    else
      if Uppercase(permissions)[1] = 'L' then
        Value.Directory := True;
  end;
end;

function TFTPList.ParseEPLF(Value: string): Boolean;
var
  s, os: string;
  flr: TFTPListRec;
begin
  Result := False;
  if Value <> '' then
    if Value[1] = '+' then
    begin
      os := Value;
      Delete(Value, 1, 1);
      flr := TFTPListRec.create;
      flr.FileName := SeparateRight(Value, #9);
      s := Fetch(Value, ',');
      while s <> '' do
      begin
        if s[1] = #9 then
          Break;
        case s[1] of
          '/':
            flr.Directory := true;
          'r':
            flr.Readable := true;
          's':
            {$IFDEF VER100}
            flr.FileSize := StrToIntDef(Copy(s, 2, Length(s) - 1), 0);
            {$ELSE}
            flr.FileSize := StrToInt64Def(Copy(s, 2, Length(s) - 1), 0);
            {$ENDIF}
          'm':
            flr.FileTime := (StrToIntDef(Copy(s, 2, Length(s) - 1), 0) / 86400)
              + 25569;
        end;
        s := Fetch(Value, ',');
      end;
      if flr.FileName <> '' then
      if (flr.Directory and ((flr.FileName = '.') or (flr.FileName = '..')))
        or (flr.FileName = '') then
        flr.free
      else
      begin
        flr.OriginalLine := os;
        flr.Mask := 'EPLF';
        Flist.Add(flr);
        Result := True;
      end;
    end;
end;

procedure TFTPList.ParseLines;
var
  flr: TFTPListRec;
  n, m: Integer;
  S: string;
  x: integer;
  b: Boolean;
begin
  n := 0;
  while n < Lines.Count do
  begin
    if n = Lines.Count - 1 then
      s := ''
    else
      s := Lines[n + 1];
    b := False;
    x := 0;
    if ParseEPLF(Lines[n]) then
    begin
      b := True;
      x := 1;
    end
    else
      for m := 0 to Masks.Count - 1 do
      begin
        x := ParseByMask(Lines[n], s, Masks[m]);
        if x > 0 then
          if CheckValues then
          begin
            flr := TFTPListRec.create;
            FillRecord(flr);
            flr.OriginalLine := Lines[n];
            flr.Mask := Masks[m];
            if flr.Directory and ((flr.FileName = '.') or (flr.FileName = '..')) then
              flr.free
            else
              Flist.Add(flr);
            b := True;
            Break;
          end;
      end;
    if not b then
      FUnparsedLines.Add(Lines[n]);
    Inc(n);
    if x > 1 then
      Inc(n, x - 1);
  end;
end;

{==============================================================================}

function FtpGetFile(const IP, Port, FileName, LocalFile,
  User, Pass: string): Boolean;
begin
  Result := False;
  with TFTPSend.Create do
  try
    if User <> '' then
    begin
      Username := User;
      Password := Pass;
    end;
    TargetHost := IP;
    TargetPort := Port;
    if not Login then
      Exit;
    DirectFileName := LocalFile;
    DirectFile:=True;
    Result := RetrieveFile(FileName, False);
    Logout;
  finally
    Free;
  end;
end;

function FtpPutFile(const IP, Port, FileName, LocalFile,
  User, Pass: string): Boolean;
begin
  Result := False;
  with TFTPSend.Create do
  try
    if User <> '' then
    begin
      Username := User;
      Password := Pass;
    end;
    TargetHost := IP;
    TargetPort := Port;
    if not Login then
      Exit;
    DirectFileName := LocalFile;
    DirectFile:=True;
    Result := StoreFile(FileName, False);
    Logout;
  finally
    Free;
  end;
end;

function FtpInterServerTransfer(
  const FromIP, FromPort, FromFile, FromUser, FromPass: string;
  const ToIP, ToPort, ToFile, ToUser, ToPass: string): Boolean;
var
  FromFTP, ToFTP: TFTPSend;
  s: string;
  x: integer;
begin
  Result := False;
  FromFTP := TFTPSend.Create;
  toFTP := TFTPSend.Create;
  try
    if FromUser <> '' then
    begin
      FromFTP.Username := FromUser;
      FromFTP.Password := FromPass;
    end;
    if ToUser <> '' then
    begin
      ToFTP.Username := ToUser;
      ToFTP.Password := ToPass;
    end;
    FromFTP.TargetHost := FromIP;
    FromFTP.TargetPort := FromPort;
    ToFTP.TargetHost := ToIP;
    ToFTP.TargetPort := ToPort;
    if not FromFTP.Login then
      Exit;
    if not ToFTP.Login then
      Exit;
    if (FromFTP.FTPCommand('PASV') div 100) <> 2 then
      Exit;
    FromFTP.ParseRemote(FromFTP.ResultString);
    s := ReplaceString(FromFTP.DataIP, '.', ',');
    s := 'PORT ' + s + ',' + IntToStr(StrToIntDef(FromFTP.DataPort, 0) div 256)
      + ',' + IntToStr(StrToIntDef(FromFTP.DataPort, 0) mod 256);
    if (ToFTP.FTPCommand(s) div 100) <> 2 then
      Exit;
    x := ToFTP.FTPCommand('RETR ' + FromFile);
    if (x div 100) <> 1 then
      Exit;
    x := FromFTP.FTPCommand('STOR ' + ToFile);
    if (x div 100) <> 1 then
      Exit;
    FromFTP.Timeout := 21600000;
    x := FromFTP.ReadResult;
    if (x  div 100) <> 2 then
      Exit;
    ToFTP.Timeout := 21600000;
    x := ToFTP.ReadResult;
    if (x div 100) <> 2 then
      Exit;
    Result := True;
  finally
    ToFTP.Free;
    FromFTP.Free;
  end;
end;

end.
