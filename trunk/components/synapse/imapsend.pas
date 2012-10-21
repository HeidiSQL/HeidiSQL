{==============================================================================|
| Project : Ararat Synapse                                       | 002.005.003 |
|==============================================================================|
| Content: IMAP4rev1 client                                                    |
|==============================================================================|
| Copyright (c)1999-2012, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)2001-2012.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(IMAP4 rev1 protocol client)

Used RFC: RFC-2060, RFC-2595
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit imapsend;

interface

uses
  SysUtils, Classes,
  blcksock, synautil;

const
  cIMAPProtocol = '143';

type
  {:@abstract(Implementation of IMAP4 protocol.)
   Note: Are you missing properties for setting Username and Password? Look to
   parent @link(TSynaClient) object!

   Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}
  TIMAPSend = class(TSynaClient)
  protected
    FSock: TTCPBlockSocket;
    FTagCommand: integer;
    FResultString: string;
    FFullResult: TStringList;
    FIMAPcap: TStringList;
    FAuthDone: Boolean;
    FSelectedFolder: string;
    FSelectedCount: integer;
    FSelectedRecent: integer;
    FSelectedUIDvalidity: integer;
    FUID: Boolean;
    FAutoTLS: Boolean;
    FFullSSL: Boolean;
    function ReadResult: string;
    function AuthLogin: Boolean;
    function Connect: Boolean;
    procedure ParseMess(Value:TStrings);
    procedure ParseFolderList(Value:TStrings);
    procedure ParseSelect;
    procedure ParseSearch(Value:TStrings);
    procedure ProcessLiterals;
  public
    constructor Create;
    destructor Destroy; override;

    {:By this function you can call any IMAP command. Result of this command is
     in adequate properties.}
    function IMAPcommand(Value: string): string;

    {:By this function you can call any IMAP command what need upload any data.
     Result of this command is in adequate properties.}
    function IMAPuploadCommand(Value: string; const Data:TStrings): string;

    {:Call CAPABILITY command and fill IMAPcap property by new values.}
    function Capability: Boolean;

    {:Connect to IMAP server and do login to this server. This command begin
     session.}
    function Login: Boolean;

    {:Disconnect from IMAP server and terminate session session. If exists some
     deleted and non-purged messages, these messages are not deleted!}
    function Logout: Boolean;

    {:Do NOOP. It is for prevent disconnect by timeout.}
    function NoOp: Boolean;

    {:Lists folder names. You may specify level of listing. If you specify
     FromFolder as empty string, return is all folders in system.}
    function List(FromFolder: string; const FolderList: TStrings): Boolean;

    {:Lists folder names what match search criteria. You may specify level of
     listing. If you specify FromFolder as empty string, return is all folders
     in system.}
    function ListSearch(FromFolder, Search: string; const FolderList: TStrings): Boolean;

    {:Lists subscribed folder names. You may specify level of listing. If you
     specify FromFolder as empty string, return is all subscribed folders in
     system.}
    function ListSubscribed(FromFolder: string; const FolderList: TStrings): Boolean;

    {:Lists subscribed folder names what matching search criteria. You may
     specify level of listing. If you specify FromFolder as empty string, return
     is all subscribed folders in system.}
    function ListSearchSubscribed(FromFolder, Search: string; const FolderList: TStrings): Boolean;

    {:Create a new folder.}
    function CreateFolder(FolderName: string): Boolean;

    {:Delete a folder.}
    function DeleteFolder(FolderName: string): Boolean;

    {:Rename folder names.}
    function RenameFolder(FolderName, NewFolderName: string): Boolean;

    {:Subscribe folder.}
    function SubscribeFolder(FolderName: string): Boolean;

    {:Unsubscribe folder.}
    function UnsubscribeFolder(FolderName: string): Boolean;

    {:Select folder.}
    function SelectFolder(FolderName: string): Boolean;

    {:Select folder, but only for reading. Any changes are not allowed!}
    function SelectROFolder(FolderName: string): Boolean;

    {:Close a folder. (end of Selected state)}
    function CloseFolder: Boolean;

    {:Ask for given status of folder. I.e. if you specify as value 'UNSEEN',
     result is number of unseen messages in folder. For another status
     indentificator check IMAP documentation and documentation of your IMAP
     server (each IMAP server can have their own statuses.)}
    function StatusFolder(FolderName, Value: string): integer;

    {:Hardly delete all messages marked as 'deleted' in current selected folder.}
    function ExpungeFolder: Boolean;

    {:Touch to folder. (use as update status of folder, etc.)}
    function CheckFolder: Boolean;

    {:Append given message to specified folder.}
    function AppendMess(ToFolder: string; const Mess: TStrings): Boolean;

    {:'Delete' message from current selected folder. It mark message as Deleted.
     Real deleting will be done after sucessfull @link(CloseFolder) or
     @link(ExpungeFolder)}
    function DeleteMess(MessID: integer): boolean;

    {:Get full message from specified message in selected folder.}
    function FetchMess(MessID: integer; const Mess: TStrings): Boolean;

    {:Get message headers only from specified message in selected folder.}
    function FetchHeader(MessID: integer; const Headers: TStrings): Boolean;

    {:Return message size of specified message from current selected folder.}
    function MessageSize(MessID: integer): integer;

    {:Copy message from current selected folder to another folder.}
    function CopyMess(MessID: integer; ToFolder: string): Boolean;

    {:Return message numbers from currently selected folder as result
     of searching. Search criteria is very complex language (see to IMAP
     specification) similar to SQL (but not same syntax!).}
    function SearchMess(Criteria: string; const FoundMess: TStrings): Boolean;

    {:Sets flags of message from current selected folder.}
    function SetFlagsMess(MessID: integer; Flags: string): Boolean;

    {:Gets flags of message from current selected folder.}
    function GetFlagsMess(MessID: integer; var Flags: string): Boolean;

    {:Add flags to message's flags.}
    function AddFlagsMess(MessID: integer; Flags: string): Boolean;

    {:Remove flags from message's flags.}
    function DelFlagsMess(MessID: integer; Flags: string): Boolean;

    {:Call STARTTLS command for upgrade connection to SSL/TLS mode.}
    function StartTLS: Boolean;

    {:return UID of requested message ID.}
    function GetUID(MessID: integer; var UID : Integer): Boolean;

    {:Try to find given capabily in capabilty string returned from IMAP server.}
    function FindCap(const Value: string): string;
  published
    {:Status line with result of last operation.}
    property ResultString: string read FResultString;

    {:Full result of last IMAP operation.}
    property FullResult: TStringList read FFullResult;

    {:List of server capabilites.}
    property IMAPcap: TStringList read FIMAPcap;

    {:Authorization is successful done.}
    property AuthDone: Boolean read FAuthDone;

    {:Turn on or off usage of UID (unicate identificator) of messages instead
     only sequence numbers.}
    property UID: Boolean read FUID Write FUID;

    {:Name of currently selected folder.}
    property SelectedFolder: string read FSelectedFolder;

    {:Count of messages in currently selected folder.}
    property SelectedCount: integer read FSelectedCount;

    {:Count of not-visited messages in currently selected folder.}
    property SelectedRecent: integer read FSelectedRecent;

    {:This number with name of folder is unique indentificator of folder.
     (If someone delete folder and next create new folder with exactly same name
     of folder, this number is must be different!)}
    property SelectedUIDvalidity: integer read FSelectedUIDvalidity;

    {:If is set to true, then upgrade to SSL/TLS mode if remote server support it.}
    property AutoTLS: Boolean read FAutoTLS Write FAutoTLS;

    {:SSL/TLS mode is used from first contact to server. Servers with full
     SSL/TLS mode usualy using non-standard TCP port!}
    property FullSSL: Boolean read FFullSSL Write FFullSSL;

    {:Socket object used for TCP/IP operation. Good for seting OnStatus hook, etc.}
    property Sock: TTCPBlockSocket read FSock;
  end;

implementation

constructor TIMAPSend.Create;
begin
  inherited Create;
  FFullResult := TStringList.Create;
  FIMAPcap := TStringList.Create;
  FSock := TTCPBlockSocket.Create;
  FSock.Owner := self;
  FSock.ConvertLineEnd := True;
  FSock.SizeRecvBuffer := 32768;
  FSock.SizeSendBuffer := 32768;
  FTimeout := 60000;
  FTargetPort := cIMAPProtocol;
  FTagCommand := 0;
  FSelectedFolder := '';
  FSelectedCount := 0;
  FSelectedRecent := 0;
  FSelectedUIDvalidity := 0;
  FUID := False;
  FAutoTLS := False;
  FFullSSL := False;
end;

destructor TIMAPSend.Destroy;
begin
  FSock.Free;
  FIMAPcap.Free;
  FFullResult.Free;
  inherited Destroy;
end;


function TIMAPSend.ReadResult: string;
var
  s: string;
  x, l: integer;
begin
  Result := '';
  FFullResult.Clear;
  FResultString := '';
  repeat
    s := FSock.RecvString(FTimeout);
    if Pos('S' + IntToStr(FTagCommand) + ' ', s) = 1 then
    begin
      FResultString := s;
      break;
    end
    else
      FFullResult.Add(s);
    if (s <> '') and (s[Length(s)]='}') then
    begin
      s := Copy(s, 1, Length(s) - 1);
      x := RPos('{', s);
      s := Copy(s, x + 1, Length(s) - x);
      l := StrToIntDef(s, -1);
      if l <> -1 then
      begin
        s := FSock.RecvBufferStr(l, FTimeout);
        FFullResult.Add(s);
      end;
    end;
  until FSock.LastError <> 0;
  s := Trim(separateright(FResultString, ' '));
  Result:=uppercase(Trim(separateleft(s, ' ')));
end;

procedure TIMAPSend.ProcessLiterals;
var
  l: TStringList;
  n, x: integer;
  b: integer;
  s: string;
begin
  l := TStringList.Create;
  try
    l.Assign(FFullResult);
    FFullResult.Clear;
    b := 0;
    for n := 0 to l.Count - 1 do
    begin
      s := l[n];
      if b > 0 then
      begin
        FFullResult[FFullresult.Count - 1] :=
          FFullResult[FFullresult.Count - 1] + s;
        inc(b);
        if b > 2 then
          b := 0;
      end
      else
      begin
        if (s <> '') and (s[Length(s)]='}') then
        begin
          x := RPos('{', s);
          Delete(s, x, Length(s) - x + 1);
          b := 1;
        end
        else
          b := 0;
        FFullResult.Add(s);
      end;
    end;
  finally
    l.Free;
  end;
end;

function TIMAPSend.IMAPcommand(Value: string): string;
begin
  Inc(FTagCommand);
  FSock.SendString('S' + IntToStr(FTagCommand) + ' ' + Value + CRLF);
  Result := ReadResult;
end;

function TIMAPSend.IMAPuploadCommand(Value: string; const Data:TStrings): string;
var
  l: integer;
begin
  Inc(FTagCommand);
  l := Length(Data.Text);
  FSock.SendString('S' + IntToStr(FTagCommand) + ' ' + Value + ' {'+ IntToStr(l) + '}' + CRLF);
  FSock.RecvString(FTimeout);
  FSock.SendString(Data.Text + CRLF);
  Result := ReadResult;
end;

procedure TIMAPSend.ParseMess(Value:TStrings);
var
  n: integer;
begin
  Value.Clear;
  for n := 0 to FFullResult.Count - 2 do
    if (length(FFullResult[n]) > 0) and (FFullResult[n][Length(FFullResult[n])] = '}') then
    begin
      Value.Text := FFullResult[n + 1];
      Break;
    end;
end;

procedure TIMAPSend.ParseFolderList(Value:TStrings);
var
  n, x: integer;
  s: string;
begin
  ProcessLiterals;
  Value.Clear;
  for n := 0 to FFullResult.Count - 1 do
  begin
    s := FFullResult[n];
    if (s <> '') and (Pos('\NOSELECT', UpperCase(s)) = 0) then
    begin
      if s[Length(s)] = '"' then
      begin
        Delete(s, Length(s), 1);
        x := RPos('"', s);
      end
      else
        x := RPos(' ', s);
      if (x > 0) then
        Value.Add(Copy(s, x + 1, Length(s) - x));
    end;
  end;
end;

procedure TIMAPSend.ParseSelect;
var
  n: integer;
  s, t: string;
begin
  ProcessLiterals;
  FSelectedCount := 0;
  FSelectedRecent := 0;
  FSelectedUIDvalidity := 0;
  for n := 0 to FFullResult.Count - 1 do
  begin
    s := uppercase(FFullResult[n]);
    if Pos(' EXISTS', s) > 0 then
    begin
      t := Trim(separateleft(s, ' EXISTS'));
      t := Trim(separateright(t, '* '));
      FSelectedCount := StrToIntDef(t, 0);
    end;
    if Pos(' RECENT', s) > 0 then
    begin
      t := Trim(separateleft(s, ' RECENT'));
      t := Trim(separateright(t, '* '));
      FSelectedRecent := StrToIntDef(t, 0);
    end;
    if Pos('UIDVALIDITY', s) > 0 then
    begin
      t := Trim(separateright(s, 'UIDVALIDITY '));
      t := Trim(separateleft(t, ']'));
      FSelectedUIDvalidity := StrToIntDef(t, 0);
    end;
  end;
end;

procedure TIMAPSend.ParseSearch(Value:TStrings);
var
  n: integer;
  s: string;
begin
  ProcessLiterals;
  Value.Clear;
  for n := 0 to FFullResult.Count - 1 do
  begin
    s := uppercase(FFullResult[n]);
    if Pos('* SEARCH', s) = 1 then
    begin
      s := Trim(SeparateRight(s, '* SEARCH'));
      while s <> '' do
        Value.Add(Fetch(s, ' '));
    end;
  end;
end;

function TIMAPSend.FindCap(const Value: string): string;
var
  n: Integer;
  s: string;
begin
  s := UpperCase(Value);
  Result := '';
  for n := 0 to FIMAPcap.Count - 1 do
    if Pos(s, UpperCase(FIMAPcap[n])) = 1 then
    begin
      Result := FIMAPcap[n];
      Break;
    end;
end;

function TIMAPSend.AuthLogin: Boolean;
begin
  Result := IMAPcommand('LOGIN "' + FUsername + '" "' + FPassword + '"') = 'OK';
end;

function TIMAPSend.Connect: Boolean;
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

function TIMAPSend.Capability: Boolean;
var
  n: Integer;
  s, t: string;
begin
  Result := False;
  FIMAPcap.Clear;
  s := IMAPcommand('CAPABILITY');
  if s = 'OK' then
  begin
    ProcessLiterals;
    for n := 0 to FFullResult.Count - 1 do
      if Pos('* CAPABILITY ', FFullResult[n]) = 1 then
      begin
        s := Trim(SeparateRight(FFullResult[n], '* CAPABILITY '));
        while not (s = '') do
        begin
          t := Trim(separateleft(s, ' '));
          s := Trim(separateright(s, ' '));
          if s = t then
            s := '';
          FIMAPcap.Add(t);
        end;
      end;
    Result := True;
  end;
end;

function TIMAPSend.Login: Boolean;
var
  s: string;
begin
  FSelectedFolder := '';
  FSelectedCount := 0;
  FSelectedRecent := 0;
  FSelectedUIDvalidity := 0;
  Result := False;
  FAuthDone := False;
  if not Connect then
    Exit;
  s := FSock.RecvString(FTimeout);
  if Pos('* PREAUTH', s) = 1 then
    FAuthDone := True
  else
    if Pos('* OK', s) = 1 then
      FAuthDone := False
    else
      Exit;
  if Capability then
  begin
    if Findcap('IMAP4rev1') = '' then
      Exit;
    if FAutoTLS and (Findcap('STARTTLS') <> '') then
      if StartTLS then
        Capability;
  end;
  Result := AuthLogin;
end;

function TIMAPSend.Logout: Boolean;
begin
  Result := IMAPcommand('LOGOUT') = 'OK';
  FSelectedFolder := '';
  FSock.CloseSocket;
end;

function TIMAPSend.NoOp: Boolean;
begin
  Result := IMAPcommand('NOOP') = 'OK';
end;

function TIMAPSend.List(FromFolder: string; const FolderList: TStrings): Boolean;
begin
  Result := IMAPcommand('LIST "' + FromFolder + '" *') = 'OK';
  ParseFolderList(FolderList);
end;

function TIMAPSend.ListSearch(FromFolder, Search: string; const FolderList: TStrings): Boolean;
begin
  Result := IMAPcommand('LIST "' + FromFolder + '" "' + Search +'"') = 'OK';
  ParseFolderList(FolderList);
end;

function TIMAPSend.ListSubscribed(FromFolder: string; const FolderList: TStrings): Boolean;
begin
  Result := IMAPcommand('LSUB "' + FromFolder + '" *') = 'OK';
  ParseFolderList(FolderList);
end;

function TIMAPSend.ListSearchSubscribed(FromFolder, Search: string; const FolderList: TStrings): Boolean;
begin
  Result := IMAPcommand('LSUB "' + FromFolder + '" "' + Search +'"') = 'OK';
  ParseFolderList(FolderList);
end;

function TIMAPSend.CreateFolder(FolderName: string): Boolean;
begin
  Result := IMAPcommand('CREATE "' + FolderName + '"') = 'OK';
end;

function TIMAPSend.DeleteFolder(FolderName: string): Boolean;
begin
  Result := IMAPcommand('DELETE "' + FolderName + '"') = 'OK';
end;

function TIMAPSend.RenameFolder(FolderName, NewFolderName: string): Boolean;
begin
  Result := IMAPcommand('RENAME "' + FolderName + '" "' + NewFolderName + '"') = 'OK';
end;

function TIMAPSend.SubscribeFolder(FolderName: string): Boolean;
begin
  Result := IMAPcommand('SUBSCRIBE "' + FolderName + '"') = 'OK';
end;

function TIMAPSend.UnsubscribeFolder(FolderName: string): Boolean;
begin
  Result := IMAPcommand('UNSUBSCRIBE "' + FolderName + '"') = 'OK';
end;

function TIMAPSend.SelectFolder(FolderName: string): Boolean;
begin
  Result := IMAPcommand('SELECT "' + FolderName + '"') = 'OK';
  FSelectedFolder := FolderName;
  ParseSelect;
end;

function TIMAPSend.SelectROFolder(FolderName: string): Boolean;
begin
  Result := IMAPcommand('EXAMINE "' + FolderName + '"') = 'OK';
  FSelectedFolder := FolderName;
  ParseSelect;
end;

function TIMAPSend.CloseFolder: Boolean;
begin
  Result := IMAPcommand('CLOSE') = 'OK';
  FSelectedFolder := '';
end;

function TIMAPSend.StatusFolder(FolderName, Value: string): integer;
var
  n: integer;
  s, t: string;
begin
  Result := -1;
  Value := Uppercase(Value);
  if IMAPcommand('STATUS "' + FolderName + '" (' + Value + ')' ) = 'OK' then
  begin
    ProcessLiterals;
    for n := 0 to FFullResult.Count - 1 do
    begin
      s := FFullResult[n];
//      s := UpperCase(FFullResult[n]);
      if (Pos('* ', s) = 1) and (Pos(FolderName, s) >= 1) and (Pos(Value, s) > 0 ) then
      begin
        t := SeparateRight(s, Value);
        t := SeparateLeft(t, ')');
        t := trim(t);
        Result := StrToIntDef(t, -1);
        Break;
      end;
    end;
  end;
end;

function TIMAPSend.ExpungeFolder: Boolean;
begin
  Result := IMAPcommand('EXPUNGE') = 'OK';
end;

function TIMAPSend.CheckFolder: Boolean;
begin
  Result := IMAPcommand('CHECK') = 'OK';
end;

function TIMAPSend.AppendMess(ToFolder: string; const Mess: TStrings): Boolean;
begin
  Result := IMAPuploadCommand('APPEND "' + ToFolder + '"', Mess) = 'OK';
end;

function TIMAPSend.DeleteMess(MessID: integer): boolean;
var
  s: string;
begin
  s := 'STORE ' + IntToStr(MessID) + ' +FLAGS.SILENT (\Deleted)';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
end;

function TIMAPSend.FetchMess(MessID: integer; const Mess: TStrings): Boolean;
var
  s: string;
begin
  s := 'FETCH ' + IntToStr(MessID) + ' (RFC822)';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
  ParseMess(Mess);
end;

function TIMAPSend.FetchHeader(MessID: integer; const Headers: TStrings): Boolean;
var
  s: string;
begin
  s := 'FETCH ' + IntToStr(MessID) + ' (RFC822.HEADER)';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
  ParseMess(Headers);
end;

function TIMAPSend.MessageSize(MessID: integer): integer;
var
  n: integer;
  s, t: string;
begin
  Result := -1;
  s := 'FETCH ' + IntToStr(MessID) + ' (RFC822.SIZE)';
  if FUID then
    s := 'UID ' + s;
  if IMAPcommand(s) = 'OK' then
  begin
    ProcessLiterals;
    for n := 0 to FFullResult.Count - 1 do
    begin
      s := UpperCase(FFullResult[n]);
      if (Pos('* ', s) = 1) and (Pos('RFC822.SIZE', s) > 0 ) then
      begin
        t := SeparateRight(s, 'RFC822.SIZE ');
        t := Trim(SeparateLeft(t, ')'));
        t := Trim(SeparateLeft(t, ' '));
        Result := StrToIntDef(t, -1);
        Break;
      end;
    end;
  end;
end;

function TIMAPSend.CopyMess(MessID: integer; ToFolder: string): Boolean;
var
  s: string;
begin
  s := 'COPY ' + IntToStr(MessID) + ' "' + ToFolder + '"';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
end;

function TIMAPSend.SearchMess(Criteria: string; const FoundMess: TStrings): Boolean;
var
  s: string;
begin
  s := 'SEARCH ' + Criteria;
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
  ParseSearch(FoundMess);
end;

function TIMAPSend.SetFlagsMess(MessID: integer; Flags: string): Boolean;
var
  s: string;
begin
  s := 'STORE ' + IntToStr(MessID) + ' FLAGS.SILENT (' + Flags + ')';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
end;

function TIMAPSend.AddFlagsMess(MessID: integer; Flags: string): Boolean;
var
  s: string;
begin
  s := 'STORE ' + IntToStr(MessID) + ' +FLAGS.SILENT (' + Flags + ')';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
end;

function TIMAPSend.DelFlagsMess(MessID: integer; Flags: string): Boolean;
var
  s: string;
begin
  s := 'STORE ' + IntToStr(MessID) + ' -FLAGS.SILENT (' + Flags + ')';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
end;

function TIMAPSend.GetFlagsMess(MessID: integer; var Flags: string): Boolean;
var
  s: string;
  n: integer;
begin
  Flags := '';
  s := 'FETCH ' + IntToStr(MessID) + ' (FLAGS)';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
  ProcessLiterals;
  for n := 0 to FFullResult.Count - 1 do
  begin
    s := uppercase(FFullResult[n]);
    if (Pos('* ', s) = 1) and (Pos('FLAGS', s) > 0 ) then
    begin
      s := SeparateRight(s, 'FLAGS');
      s := Separateright(s, '(');
      Flags := Trim(SeparateLeft(s, ')'));
    end;
  end;
end;

function TIMAPSend.StartTLS: Boolean;
begin
  Result := False;
  if FindCap('STARTTLS') <> '' then
  begin
    if IMAPcommand('STARTTLS') = 'OK' then
    begin
      Fsock.SSLDoConnect;
      Result := FSock.LastError = 0;
    end;
  end;
end;

//Paul Buskermolen <p.buskermolen@pinkroccade.com>
function TIMAPSend.GetUID(MessID: integer; var UID : Integer): boolean;
var
  s, sUid: string;
  n: integer;
begin
  sUID := '';
  s := 'FETCH ' + IntToStr(MessID) + ' UID';
  Result := IMAPcommand(s) = 'OK';
  ProcessLiterals;
  for n := 0 to FFullResult.Count - 1 do
  begin
    s := uppercase(FFullResult[n]);
    if Pos('FETCH (UID', s) >= 1 then
    begin
      s := Separateright(s, '(UID ');
      sUID := Trim(SeparateLeft(s, ')'));
    end;
  end;
  UID := StrToIntDef(sUID, 0);
end;

{==============================================================================}

end.
