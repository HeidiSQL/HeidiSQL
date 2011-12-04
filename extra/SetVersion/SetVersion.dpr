program SetVersion;

{$APPTYPE CONSOLE}

uses
  SysUtils, Windows, SynRegExpr, Classes;

var
  FileName, FileContent, Line, Revision, AppVer, CurDir, SvnOutput, Cmd: String;
  FileHandle: TextFile;
  rx: TRegExpr;
  SvnOutputLines: TStringList;

{$I ..\..\source\const.inc}

function RunConsoleAppWaitAndCapture(const cConsoleApp, cParameters,
  cWorkingDir: string; aResults: TStringList): DWord;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  hStdOut, hAppProcess, hAppThread: THandle;
  cTemp, cTempFile: String;
  aBuffer: Array[0..255] of Char;
begin
  Result := 0;
  hAppProcess := 0;
  hAppThread := 0;
  aResults.Clear;

  GetTempPath( 255, aBuffer );
  cTemp := StrPas( aBuffer );
  cTempFile := cTemp + 'stdout.tmp';
  if FileExists(cTempFile) then
    SysUtils.DeleteFile( cTempFile );

  // Initialize output file security attributes
  FillChar(SA, SizeOf(SA), #0 );
  SA.nLength              := SizeOf(SA);
  SA.lpSecurityDescriptor := nil;
  SA.bInheritHandle       := true;

  // Create Output File
  hStdOut := CreateFile(PChar(cTempFile),
    GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE,
    @SA,
    CREATE_ALWAYS, // Always create it
    FILE_ATTRIBUTE_TEMPORARY or // Will cache in memory if possible
    FILE_FLAG_WRITE_THROUGH,
    0);
  if hStdOut = INVALID_HANDLE_VALUE then begin
    ExitCode := 103;
    Raise Exception.CreateFmt('Creating temporary stdout "'+cTempFile+'" file failed.', [cConsoleApp]);
  end;

  // Initialize Startup Info
  FillChar(SI, SizeOf(SI), #0);
  with SI do begin
    cb          := SizeOf(SI);
    dwFlags     := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    wShowWindow := SW_HIDE;
    hStdInput   := GetStdHandle(STD_INPUT_HANDLE);
    hStdError   := hStdOut;
    hStdOutput  := hStdOut;
  end;

  try
    // Create the process
    cTemp := cConsoleApp + ' ' + cParameters;
    if CreateProcess(nil,
      PChar( cTemp ),
      nil,
      nil,
      true,
      0,
      nil,
      PChar(cWorkingDir),
      SI,
      PI
     ) then begin
      WaitforSingleObject( PI.hProcess, INFINITE );
      hAppProcess := PI.hProcess;
      hAppThread  := PI.hThread;
      GetExitCodeProcess( hAppProcess, result );
    end else begin
      ExitCode := 104;
      Raise Exception.CreateFmt('CreateProcess() failed!'#10#13'Command line = %s', [cConsoleApp]);
    end;
    CloseHandle( hStdOut );

    if FileExists(cTempFile) then begin
      try
        aResults.LoadFromFile(cTempFile);
      except
        on e:Exception do
          WriteLn(e.Message);
      end;
      SysUtils.DeleteFile(cTempFile);
    end;

  finally
    if hAppProcess <> 0 then
      CloseHandle(hAppProcess);
    if hAppThread <> 0 then
      CloseHandle(hAppThread);
  end;
end;


begin
  try
    FileName := Paramstr(1);
    if not FileExists(FileName) then begin
      raise Exception.Create('File "'+FileName+'" is not a valid resource file.');
      ExitCode := 101;
    end;

    // Read resource file
    AssignFile(FileHandle, FileName);
    Reset(FileHandle);
    while not Eof(FileHandle) do begin
      ReadLn(FileHandle, Line);
      FileContent := FileContent + Line + #13#10;
    end;
    Delete(FileContent, Length(FileContent)-1, 2);
    CloseFile(FileHandle);

    // Regular expression object
    rx := TRegExpr.Create;
    rx.ModifierI := True;

    // Find Subversion revision number
    CurDir := ExtractFilePath(paramStr(0));
    SvnOutputLines := TStringList.Create;
    Cmd := 'svnversion.exe';
    RunConsoleAppWaitAndCapture(Cmd, ExtractFilePath(ExpandFileName(FileName)), CurDir, SvnOutputLines);
    SvnOutput := Trim(SvnOutputLines.Text);
    // 123:123M
    // 123M
    // 123
    rx.Expression := '^(\d+\:)?(\d+)M?$';
    if rx.Exec(SvnOutput) then
      Revision := rx.Match[2]
    else begin
      ExitCode := 102;
      raise Exception.Create('Could not find SVN revision');
    end;

    // Inject revision into file content
    rx.Expression := '((\bFILEVERSION\s)(\d+),(\d+),(\d+),)\d+(\b)';
    if rx.Exec(FileContent) then
      AppVer := rx.Match[3]+'.'+rx.Match[4]+'.'+rx.Match[5]+'.'+Revision
    else
      AppVer := '[Parse error in '+ExtractFileName(ParamStr(0))+']';
    FileContent := rx.Replace(FileContent, '${1}'+Revision+'${6}', True);
    rx.Free;

    FileContent := StringReplace(FileContent, '%APPNAME%', APPNAME, [rfReplaceAll]);
    FileContent := StringReplace(FileContent, '%APPVER%', AppVer, [rfReplaceAll]);

    // Save modified file
    Rewrite(FileHandle);
    WriteLn(FileHandle, FileContent);
    CloseFile(FileHandle);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.

