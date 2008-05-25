unit updatecheck;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  Dialogs, StdCtrls, ExtActns, IniFiles, Controls, Graphics, Registry;

type
  TfrmUpdateCheck = class(TForm)
    btnCancel: TButton;
    groupBuild: TGroupBox;
    btnBuild: TButton;
    groupRelease: TGroupBox;
    btnRelease: TButton;
    lblStatus: TLabel;
    memoRelease: TMemo;
    memoBuild: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure btnReleaseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    CheckfileDownload : TDownLoadURL;
    ReleaseURL, BuildURL : String;
    procedure Status(txt: String);
    procedure ReadCheckFile;
    procedure URLOnDownloadProgress(Sender: TDownLoadURL; Progress, ProgressMax: Cardinal;
      StatusCode: TURLDownloadStatus; StatusText: String; var Cancel: Boolean);
  public
    { Public declarations }
    AutoClose: Boolean; // Automatically close dialog after detecting no available downloads
    CurrentRevision: Integer;
    CheckForBuildsInAutoMode: Boolean;
  end;

implementation

uses helpers, main;

{$R *.dfm}

{$I const.inc}


{**
  Set defaults
}
procedure TfrmUpdateCheck.FormCreate(Sender: TObject);
begin
  // Should be false by default. Callers can set this to True after Create()
  AutoClose := False;
end;

{**
  Update status text
}
procedure TfrmUpdateCheck.Status(txt: String);
begin
  lblStatus.Caption := txt;
  Repaint;
end;


{**
  Download check file
}
procedure TfrmUpdateCheck.FormShow(Sender: TObject);
var
  reg : TRegistry;
begin
  Status('Initiating ... ');
  Caption := 'Check for '+APPNAME+' updates ...';
  CurrentRevision := StrToIntDef(AppRevision, 0);

  // Init GUI controls
  btnRelease.Enabled := False;
  btnBuild.Enabled := False;
  memoRelease.Clear;
  memoBuild.Clear;

  // Prepare download
  CheckfileDownload := TDownLoadURL.Create(Self);
  CheckfileDownload.URL := APPDOMAIN + 'updatecheck.php';
  CheckfileDownload.Filename := GetTempDir + APPNAME + '_updatecheck.ini';

  // Download the check file
  Screen.Cursor := crHourglass;
  try
    Status('Downloading check file ...');
    CheckfileDownload.ExecuteTarget(nil);
    Status('Reading check file ...');
    ReadCheckFile;
    if (not groupRelease.Enabled) and (not groupBuild.Enabled) then begin
      // Developer versions probably have "unknown" (0) as revision,
      // which makes it impossible to compare the revisions.
      if CurrentRevision = 0 then
        Status('Error: Cannot determine current revision. Using a developer version?')
      else
        Status('Your '+APPNAME+' is up-to-date (no update available).');
    end else
      Status('Updates available.');
    // Remember when we did the updatecheck to enable the automatic interval
    reg := TRegistry.Create;
    reg.OpenKey(REGPATH, true);
    reg.WriteString(REGNAME_LAST_UPDATECHECK, DateTimeToStr(Now));
    reg.CloseKey;
    FreeAndNil(reg);
  except
    // Do not popup errors, just display them in the status label
    On E:Exception do
      Status(E.Message);
  end;
  if FileExists(CheckfileDownload.Filename) then
    DeleteFile(CheckfileDownload.Filename);
  FreeAndNil(CheckfileDownload);
  Screen.Cursor := crDefault;

  // For automatic updatechecks this dialog should close if no updates are available.
  // Using PostMessage, as Self.Close or ModalResult := mrCancel does not work
  // as expected in FormShow
  if AutoClose
    and (not groupRelease.Enabled)
    and ((not CheckForBuildsInAutoMode) or (not groupBuild.Enabled)) then
    PostMessage(Self.Handle, WM_CLOSE, 0, 0);
end;


{**
  Parse check file for updated version + release
}
procedure TfrmUpdateCheck.ReadCheckFile;
var
  Ini : TIniFile;
  ReleaseVersion : String;
  ReleaseRevision, BuildRevision : Integer;
  Note : String;
  Compiled : TDateTime;
const
  INISECT_RELEASE = 'Release';
  INISECT_BUILD = 'Build';
begin
  // Read [Release] section of check file
  Ini := TIniFile.Create(CheckfileDownload.Filename);
  if Ini.SectionExists(INISECT_RELEASE) then begin
    ReleaseVersion := Ini.ReadString(INISECT_RELEASE, 'Version', 'unknown');
    ReleaseRevision := Ini.ReadInteger(INISECT_RELEASE, 'Revision', 0);
    ReleaseURL := Ini.ReadString(INISECT_RELEASE, 'URL', '');
    memoRelease.Lines.Add( 'Version ' + ReleaseVersion + ' (yours: '+AppVersion+')' );
    memoRelease.Lines.Add( 'Released: ' + Ini.ReadString(INISECT_RELEASE, 'Date', '') );
    Note := Ini.ReadString(INISECT_RELEASE, 'Note', '');
    if Note <> '' then
      memoRelease.Lines.Add( 'Note: ' + Note );
    btnRelease.Caption := 'Download version ' + ReleaseVersion;
    // Enable the download button if the current version is outdated
    groupRelease.Enabled := (CurrentRevision > 0) and (ReleaseRevision > CurrentRevision);
    btnRelease.Enabled := groupRelease.Enabled;
    memoRelease.Enabled := groupRelease.Enabled;
    if not memoRelease.Enabled then
      memoRelease.Font.Color := cl3DDkShadow
    else
      memoRelease.Font.Color := clWindowText;
  end;

  // Read [Build] section of check file
  if Ini.SectionExists(INISECT_BUILD) then begin
    BuildRevision := Ini.ReadInteger(INISECT_BUILD, 'Revision', 0);
    BuildURL := Ini.ReadString(INISECT_BUILD, 'URL', '');
    memoBuild.Lines.Add( 'Revision ' + IntToStr(BuildRevision) + ' (yours: '+AppRevision+')' );
    FileAge(ParamStr(0), Compiled);
    memoBuild.Lines.Add( 'Compiled: ' + Ini.ReadString(INISECT_BUILD, 'Date', '') + ' (yours: '+DateToStr(Compiled)+')' );
    Note := Ini.ReadString(INISECT_BUILD, 'Note', '');
    if Note <> '' then
      memoBuild.Lines.Add( 'Note: ' + Note );
    btnBuild.Caption := 'Download and install build ' + IntToStr(BuildRevision);
    // A new release should have priority over a new nightly build.
    // So the user should not be able to download a newer build here
    // before having installed the new release.
    groupBuild.Enabled := (CurrentRevision > 0) and (BuildRevision > CurrentRevision) and (not btnRelease.Enabled);
    btnBuild.Enabled := groupBuild.Enabled;
    memoBuild.Enabled := groupBuild.Enabled;
    if not memoBuild.Enabled then
      memoBuild.Font.Color := cl3DDkShadow
    else
      memoBuild.Font.Color := clWindowText;
  end;
end;


{**
  Download release installer via web browser
}
procedure TfrmUpdateCheck.btnReleaseClick(Sender: TObject);
begin
  ShellExec(ReleaseURL);
end;


{**
  Download latest build and replace running exe
}
procedure TfrmUpdateCheck.btnBuildClick(Sender: TObject);
var
  Download: TDownLoadURL;
  ScriptFile: Textfile;
  ExeName, ScriptFilename, ScriptContent: String;
begin
  Download := TDownLoadURL.Create(Self);
  Download.URL := BuildURL;
  ExeName := ExtractFileName(Application.ExeName);

  // Save the file in a temp directory
  Download.Filename := GetTempDir + ExeName;
  Download.OnDownloadProgress := URLOnDownloadProgress;

  // Delete probably previously downloaded file
  if FileExists(Download.Filename) then
    DeleteFile(Download.Filename);

  // Do the download
  Download.ExecuteTarget(nil);

  // Check if downloaded file exists
  if not FileExists(Download.Filename) then
    Raise Exception.Create('Downloaded file not found: '+Download.Filename);

  // The Visual Basic Script code which kills this exe and moves the
  // downloaded file to the application directory.
  // This file moving can fail due to several reasons. Especially in Vista
  // where users are normally not admins, they'll get a "Permission denied".
  // However, the script does several write attempts and quits with a clear
  // error message if it couldn't move the file.
  // TODO: move this code to a seperate file for easier debugging
  Status('Update in progress ...');
  ScriptContent := ''' This is a temporary script which shall update your ' + APPNAME + CRLF +
    ''' with a nightly build.' + CRLF +
    CRLF +
    'ExeName = "'+ExeName+'"' + CRLF +
    'DownloadFileName = "'+Download.Filename+'"' + CRLF +
    'TargetFileName = "'+Application.ExeName+'"' + CRLF +
    CRLF +
    'WScript.Echo "Terminating """&ExeName&""" ..."' + CRLF +
    'Set Shell = WScript.CreateObject("WScript.Shell")' + CRLF +
    'Shell.Run("taskkill /im """&ExeName&""" /f")' + CRLF +
    CRLF +
    'Set FileSystem = CreateObject("Scripting.FileSystemObject")' + CRLF +
    'Set DownloadFile = FileSystem.GetFile(DownloadFileName)' + CRLF +
    'Set TargetFile = FileSystem.GetFile(TargetFileName)' + CRLF +
    'On Error Resume Next' + CRLF +
    'MaxAttempts = 10' + CRLF +
    'for x = 1 to MaxAttempts' + CRLF +
    '  WScript.Echo "Deleting "&ExeName&" (attempt "&x&" of "&MaxAttempts&") ..."' + CRLF +
    '  TargetFile.Delete' + CRLF +
    '  If Err.Number = 0 Then' + CRLF +
    '    Err.Clear' + CRLF +
    '    Exit For' + CRLF +
    '  End If' + CRLF +
    '  Err.Clear' + CRLF +
    '  WScript.Sleep(2000)' + CRLF +
    'Next' + CRLF +
    'If Err.Number <> 0 Then' + CRLF +
    '  WScript.Echo "Error: Cannot delete file "&TargetFileName' + CRLF +
    '  WScript.Sleep(10000)' + CRLF +
    '  Wscript.Quit' + CRLF +
    'End If' + CRLF +
    'Err.Clear' + CRLF +
    CRLF +
    'WScript.Echo "Installing new build ..."' + CRLF +
    'DownloadFile.Move TargetFileName' + CRLF +
    CRLF +
    'WScript.Echo "Restarting ..."' + CRLF +
    'Shell.Run(""""&TargetFileName&"""")';
  // Write script file to disk
  ScriptFilename := GetTempDir + APPNAME + '_Update.vbs';
  AssignFile(ScriptFile, ScriptFilename);
  Rewrite(ScriptFile);
  Write(Scriptfile, ScriptContent);
  CloseFile(ScriptFile);
  // Calling the script will now terminate the running exe...
  WinExec(PAnsiChar('cscript.exe "'+ScriptFilename+'"'), 1);
end;


{**
  Download progress event
}
procedure TfrmUpdateCheck.URLOnDownloadProgress;
begin
  Status('Downloading: '+FormatByteNumber(Progress)+' / '+FormatByteNumber(ProgressMax) + ' ...');
end;


end.
