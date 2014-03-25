  (************************************************)
(*TPlinkremote  Version 1.0 (06.06.2006)        *)
(*                                              *)
(*Copyright ® 2006 by Uwe Zeh                   *)
(*E-Mail: daliuz@gmx.net                        *)
(*- - - - - - - - - - - - - - - - - - - - - - - *)
(*Ich übernehme keine Haftung für etwaige       *)
(*Schäden, die durch diese Komponente verursacht*)
(*werden.                                       *)
(*- - - - - - - - - - - - - - - - - - - - - - - *)
(*Diese Komponente ist FREEWARE.                *)
(*Alle Rechte vorbehalten                       *)
(*                                              *)
(*  Vielen Dank an Felix John für die Infos     *)
(*  auf http://www.felix-colibri.com            *)
(************************************************)
unit Plinkremote;

interface

uses
  Windows, StrUtils, SysUtils, Classes, Controls, Forms, Dialogs, UPipeThread, gnugettext;

const
  LINE_FEED = #10;
  CARRIAGE_RETURN = #13;
  NEW_LINE = CARRIAGE_RETURN + LINE_FEED;

type
  TTerminateStatus = (tsRunning, tsWaitForTerminate, tsTerminated);

  TPlinkRemote = class; //Class forward
  TPipe = record
    ReadHandle: THandle;
    WriteHandle: THandle;
  end;

  TExecuteWaitEvent = procedure(
    const ProcessInfo: TProcessInformation; var ATerminate: Boolean
  ) of object;
  TPlinkDataAvailableEvent = procedure(
    Sender: TPlinkRemote; const Buffer: string
  ) of object;
  TPlinkDataErrorEvent = procedure(
    Sender: TPlinkRemote; const Buffer: string
  ) of object;
  TPlinkCloseEvent = procedure(
    Sender: TPlinkRemote
  ) of object;
  TPlinkErrorEvent = procedure(
    Sender: TPlinkRemote; const Error: string
  ) of object;

  TPlinkRemote = class(TComponent)
  private
    FTerminate: TTerminateStatus;
    FInPipe: TPipe;
    FOutPipe: TPipe;
    FErrorPipe: TPipe;
    FFilename: string;
    FParameters: string;
    Fm_EscFLag: Boolean;
    Fm_EscBuffer: string[80];
    FClearEscSeq: Boolean;
    FLogFileName: string;
    FLogging: Boolean;
    FPipe: TPipeThread;
    FProcessInfo : TProcessInformation;
    function IsRunning(create_event: Boolean): Boolean;
    function CreatePipeEx(var pv_pipe: TPipe): boolean;
    procedure ClosePipe(p_pipe: TPipe);
    function CreatePipes: Boolean;
    procedure ClosePipes;
    procedure ReadOutput;
    procedure ReadError;
    function ReadPipe(const APipe : TPipe) : String;
    function ASCII2ANSI(AText:AnsiString):AnsiString;
    procedure ExecuteFile(const AFilename: string; AParameter,
        ACurrentDir: string; AWait: Boolean);
    function CleanEscSeq(const buffer: string): string;
    function f_check_Escapeseq(const chr: char): char;
    procedure WriteLog(p_text, log_file_name: string);
    function IsTrustedKey(const Buffer: String): Boolean;
    function GetExecuted: Boolean;
    procedure PipeReadPipeData;
  protected
    FOnDataAvailable: TPlinkDataAvailableEvent;
    FOnDataError: TPlinkDataErrorEvent;
    FOnPlinkclose: TPlinkCloseEvent;
    FOnError: TPlinkErrorEvent;
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
  published
    function Execute : Boolean;
    procedure Terminate;
    procedure SendText(AText : String);
    property OnDataError: TPlinkDataErrorEvent
      read FOnDataError write FOnDataError;
    property OnDataAvailable: TPlinkDataAvailableEvent
      read FOnDataAvailable write FOnDataAvailable;
    property OnPlinkClose: TPlinkCloseEvent
      read FOnPlinkclose write FOnPlinkclose;
    property OnError: TPlinkErrorEvent
      read FOnError write FOnError;
    property Filename: string
      read FFilename write FFilename;
    property Parameters: string
      read FParameters write FParameters;
    property ClearEsqSeq: Boolean
      read FClearEscSeq write FClearEscSeq default false;
    property LogFileName: string
      read FLogFileName write FLogFileName;
    property LogFile: Boolean
      read FLogging write FLogging default false;
    property Executed : Boolean
      read GetExecuted;
  end;

{procedure Register;}

implementation

uses helpers;

function TPlinkRemote.CreatePipeEx(var pv_pipe: TPipe): boolean;
const
  k_pipe_buffer_size = 8192;
begin
  with pv_pipe do
  begin
    // -- Create the pipe
    result:= CreatePipe(ReadHandle, WriteHandle, nil, k_pipe_buffer_size);

    if result then
      result:= DuplicateHandle(
        GetCurrentProcess, ReadHandle,
        GetCurrentProcess, @ReadHandle, 0, True,
        DUPLICATE_CLOSE_SOURCE OR DUPLICATE_SAME_ACCESS
      );

    if result then result:= DuplicateHandle(
      GetCurrentProcess, WriteHandle,
      GetCurrentProcess, @WriteHandle, 0, True,
      DUPLICATE_CLOSE_SOURCE OR DUPLICATE_SAME_ACCESS
    );
  end;
end; // CreatePipeEx

procedure TPlinkRemote.ClosePipe(p_pipe: TPipe);
begin
  with p_pipe do
  begin
    CloseHandle(ReadHandle);
    CloseHandle(WriteHandle);
  end;
end; // ClosePipe

function TPlinkRemote.CreatePipes: Boolean;
begin
  Result:=     CreatePipeEx(FInPipe)
           and CreatePipeEx(FOutPipe)
           and CreatePipeEx(FErrorPipe);
end; // CreatePipes

procedure TPlinkRemote.ClosePipes;
begin
  ClosePipe(FInPipe);
  ClosePipe(FOutPipe);
  ClosePipe(FErrorPipe);
end; // ClosePipes

procedure TPlinkRemote.ExecuteFile(const AFilename: string; AParameter,
  ACurrentDir: string; AWait: Boolean);
var
  StartupInfo: TStartupInfo;
  FileDirectory: pchar;
begin
  if not CreatePipes then
  begin
    if Assigned(OnError) then
      OnError(Self, 'Error creating I/O pipes!');
  end
  else begin
    if (ACurrentDir <> '') and (AnsiLastChar(ACurrentDir) = '\') then
      Delete(ACurrentDir, Length(ACurrentDir), 1);
    if ACurrentDir = '' then
      FileDirectory := nil
    else
      FileDirectory := PChar(ACurrentDir);

    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    with StartupInfo do begin
      cb := SizeOf(StartupInfo);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;              // show no window
      hStdInput:= FInPipe.ReadHandle;      // read input from input pipe
      hStdError:= FErrorPipe.WriteHandle;  // Write errors to the error pipe
      hStdOutput:= FOutPipe.WriteHandle;   // Write Ouput to output pipe
    end;

    FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
    AParameter := Format('%s %s', [AFilename, TrimRight(AParameter)]);

    if FTerminate = tsTerminated then
    begin
      if CreateProcess(
           nil, PChar(AParameter), nil, nil, true,
           CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
           nil, FileDirectory, StartupInfo, FProcessInfo) then
      begin
        FPipe.ProcessHandle := FProcessInfo.hProcess;
        FTerminate := tsRunning;
        FPipe.Resume;
      end;
    end else
    begin
      if Assigned(OnError) then
        OnError(Self, f_('Could not run "%s"' [AFilename]));
    end;
  end;
end;

procedure TPlinkRemote.ReadOutput;
var
  Output : string;
begin
  Output := ReadPipe(FOutPipe);
  if Output <> '' then
  begin
    if Assigned(FOnDataAvailable) then
      FOnDataAvailable(Self, Output);
  end; // received some bytes
end;

function TPlinkRemote.ASCII2ANSI(AText:AnsiString):AnsiString;
const cMaxLength = 255;
var PText : PAnsiChar;
begin
  Result:='';
  PText:=AnsiStrAlloc(cMaxLength);
  while AText <> '' do begin
    StrPCopy(PText, copy(AText, 1, cMaxLength-1));
    OemToAnsi(PText, PText);
    Result:=Result + StrPas(PText);
    delete(AText, 1, cMaxLength-1);
  end;
  StrDispose(PText);
end;

function TPlinkRemote.ReadPipe(const APipe: TPipe): String;
var
  BufferReadCount, OutLen: Cardinal;
  BytesRemaining : Cardinal;
  Buffer : array [0..1023] of AnsiChar;
  R:AnsiString;
begin
  Result := '';
  if APipe.ReadHandle = INVALID_HANDLE_VALUE then
  begin
    if Assigned(OnError) then
      OnError(Self, 'Error I/O pipes!');
  end
  else begin
    // -- check to see if there is any data to read from stdout
    PeekNamedPipe(
      APipe.ReadHandle, nil, 0, nil, @BufferReadCount, nil
    );

    if BufferReadCount <> 0 then
    begin
      FillChar(Buffer, sizeof(Buffer), 'z');
      // -- read by 1024 bytes chunks
      BytesRemaining := BufferReadCount;
      OutLen := 0;
      while BytesRemaining >= 1024 do
      begin
        // -- read the stdout pipe
        ReadFile(APipe.ReadHandle, Buffer, 1024, BufferReadCount, nil);
        Dec(BytesRemaining, BufferReadCount);

        SetLength(R, OutLen + BufferReadCount);
        Move(Buffer, R[OutLen + 1], BufferReadCount);
        Inc(OutLen, BufferReadCount);
      end;

      if BytesRemaining > 0 then
      begin
        ReadFile(APipe.ReadHandle, Buffer, BytesRemaining, BufferReadCount, nil);
        SetLength(R, OutLen + BufferReadCount);
        Move(Buffer, R[OutLen + 1], BufferReadCount);
      end;

      R:=ASCII2ANSI(R);
      {$WARNINGS OFF}
      Result:=AnsiToUtf8(R);
      {$WARNINGS ON}

      if FClearEscSeq then
        Result := CleanEscSeq(Result);
      if FLogging then
        WriteLog(Result, FLogFileName);
    end; // received some bytes
  end;
  Result:=StringReplace(
    Result,
    CARRIAGE_RETURN + CARRIAGE_RETURN + LINE_FEED, CARRIAGE_RETURN + LINE_FEED,
    [rfReplaceAll]
  );
end;

// ReadOutput

procedure TPlinkRemote.ReadError;
var
  Output : String;
begin
  Output := ReadPipe(FErrorPipe);
  if Output <> '' then
  begin
    if not IsTrustedKey(Output) and Assigned(OnDataError) then
      OnDataError(Self, Output);
  end;
end; // ReadError

procedure TPlinkRemote.SendText(AText: String);
var
  WrittenBytes: Cardinal;
  Text:AnsiString;
begin
  // -- check if the process is still active
  if IsRunning(true) then
  begin
    {$WARNINGS OFF}
    Text:=Utf8ToAnsi(AText);
    {$WARNINGS ON}
    if Text <> '' then
      WriteFile(FInPipe.WriteHandle, Text[1], Length(Text), WrittenBytes, nil);
  end;
end; // SendText

function TPlinkRemote.Execute: boolean;
begin
  // -- check if the process is still active
  Result := false;
  if (FTerminate = tsTerminated) and not IsRunning(false) then
  begin
    if not FileExists(Filename)then
    begin
      if Assigned(OnError) then
        OnError(Self, f_('File not found: %s', [Filename]));
    end
    else begin
      // --start program
      ExecuteFile(Filename, FParameters, ExtractFileDir(Filename), True);
      Result := true;
    end;
  end
  else if Assigned(OnError) then
    OnError(Self, f_('Process %s already started.', [FFilename]));
end;

procedure TPlinkRemote.Terminate;
begin
  if FTerminate <> tsTerminated then
    FTerminate := tsWaitForTerminate;
end;

destructor TPlinkRemote.Destroy;
begin
  // Initiere ein Beenden des Programms
  if FTerminate <> tsTerminated then
  begin
    Terminate;
    PipeReadPipeData;
  end;
  FPipe.Terminate;
  Application.ProcessMessages;
  if FPipe.Suspended then
    FPipe.Resume;
  Application.ProcessMessages;
  FPipe.Free;
  inherited;
end;

constructor TPlinkRemote.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FPipe := TPipeThread.Create(true);
  FPipe.ReadPipeData := PipeReadPipeData;
  FTerminate := tsTerminated;
  FOnDataAvailable:=nil;
  FOnDataError:=nil;
  FOnPlinkclose:=nil;
  FOnError:=nil;
end;

function TPlinkRemote.CleanEscSeq(const buffer: string): string;
var i : integer;
    chr: char;
begin
  result:='';
  Fm_EscFlag := FALSE;
  for I := 1 to Length(Buffer) do
  begin
    chr:= f_check_Escapeseq(buffer[I]);
    if chr <> #0 then
      result:=result + chr;
  end;
end;

function TPlinkRemote.f_check_Escapeseq(const chr: char): char;
var
    bProcess : Boolean;
begin
  Result := #0;
  if Fm_EscFLag then
  begin
    bProcess := False;
    if     (Length(Fm_EscBuffer) = 0)
       and CharInSet(Chr, ['D', 'M', 'E', 'H', '7', '8', '=', '>', '<']) then
    begin
      bProcess := True;
    end
    else if     (Length(Fm_EscBuffer) = 1)
            and (Fm_EscBuffer[1] in ['(', ')', '*', '+']) then
    begin
      bProcess := True;
    end
    else if    CharInSet(Chr, ['0'..'9', ';', '?', ' '])
            or (    (Length(Fm_EscBuffer) = 0)
                and CharInSet(chr, ['[', '(', ')', '*', '+'])) then
    begin
      {$WARNINGS OFF}
      Fm_EscBuffer := Fm_EscBuffer + Chr;
      {$WARNINGS ON}
      if Length(Fm_EscBuffer) >= High(Fm_EscBuffer) then
      begin
        MessageBeep(MB_ICONASTERISK);
        Fm_EscBuffer := '';
        Fm_EscFlag   := FALSE;
      end;
    end
    else
      bProcess := True;

    if bProcess then
    begin
      Fm_EscBuffer := '';
      Fm_EscFlag   := FALSE;
    end;
  end
  else if chr = #27 then
  begin
    Fm_EscBuffer := '';
    Fm_EscFlag   := TRUE;
  end
  else
    result:= chr;
end;

procedure TPlinkRemote.WriteLog(p_text: string; log_file_name: string);
var log_file: file;
begin
  AssignFile(log_file,log_file_name);
  if not FileExists(log_file_name)then
    Rewrite(log_file, 1)
  else
    reset(log_file, 1);
  Seek(log_file, FileSize(log_file));
  BlockWrite(log_file, p_text[1], Length(p_text));
  Close(log_file);
end; // write_string

function TPlinkRemote.IsRunning(create_event: Boolean): Boolean;
var
  l_process_exit_code: Cardinal;
begin
  Result := false;
  // -- check if the process is still active
  GetExitCodeProcess(FProcessInfo.hProcess, l_process_exit_code);
  if l_process_exit_code = STILL_ACTIVE then
    result:= true;
  if not result and create_event and Assigned(OnPlinkClose) then
    OnPlinkClose(Self);
end;

procedure Register;
begin
  RegisterComponents('PuttyComp', [TPlinkRemote]);
end;

function TPlinkRemote.IsTrustedKey(const Buffer: String): Boolean;
begin
  result:= false;
  if Pos('host key is not cached in the registry', buffer) > 0 then
  begin
    result:= true;
    if MessageDialog(_('Accept and cache host key?'), Buffer, mtConfirmation, [mbYes, mbNo]) = mrYes then
      SendText('y')
    else
      SendText('n');
  end;
  if Pos('host key does not match the one PuTTY', buffer) > 0 then
  begin
    result:= true;
    if MessageDialog(_('Accept changed host key?'), Buffer, mtConfirmation, [mbYes, mbNo]) = mrYes then
      SendText('y')
    else
      SendText('n');
  end;
end;

function TPlinkRemote.GetExecuted: Boolean;
begin
  Result := IsRunning(false);
end;

procedure TPlinkRemote.PipeReadPipeData;
begin
  if FTerminate <> tsTerminated then
  begin
    ReadOutput;
    ReadError;
    if FTerminate = tsWaitForTerminate then
    begin
      FPipe.WillSuspend := true;
      TerminateProcess(FProcessInfo.hProcess, 0); //Cardinal(-1));
      CloseHandle(FProcessInfo.hProcess); // close programm
      CloseHandle(FProcessInfo.hThread);
      ClosePipes; // close all pipes
      FTerminate := tsTerminated;
      IsRunning(true);      // create a on closeevent
    end;
  end;
end;

end.
