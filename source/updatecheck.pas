unit updatecheck;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, StdCtrls, IniFiles, Controls, Graphics,
  helpers, gnugettext;

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
    CheckfileDownload: THttpDownLoad;
    ReleaseURL, BuildURL : String;
    FLastStatusUpdate: Cardinal;
    FCheckFilename: String;
    procedure Status(txt: String);
    procedure ReadCheckFile;
    procedure DownloadProgress(Sender: TObject);
  public
    { Public declarations }
    AutoClose: Boolean; // Automatically close dialog after detecting no available downloads
    BuildRevision: Integer;
    CheckForBuildsInAutoMode: Boolean;
    BuildSize: Integer;
  end;

implementation

uses main;

{$R *.dfm}

{$I const.inc}



{**
  Set defaults
}
procedure TfrmUpdateCheck.FormCreate(Sender: TObject);
begin
  // Should be false by default. Callers can set this to True after Create()
  AutoClose := False;
  InheritFont(Font);
  TranslateComponent(Self);
end;

{**
  Update status text
}
procedure TfrmUpdateCheck.Status(txt: String);
begin
  lblStatus.Caption := txt;
  lblStatus.Repaint;
end;


{**
  Download check file
}
procedure TfrmUpdateCheck.FormShow(Sender: TObject);
begin
  Status('Initiating ... ');
  Caption := 'Check for '+APPNAME+' updates ...';

  // Init GUI controls
  btnRelease.Enabled := False;
  btnBuild.Enabled := False;
  memoRelease.Clear;
  memoBuild.Clear;

  // Prepare download
  CheckfileDownload := THttpDownload.Create(Self);
  CheckfileDownload.URL := APPDOMAIN+'updatecheck.php?r='+IntToStr(Mainform.AppVerRevision)+'&t='+DateTimeToStr(Now);
  FCheckFilename := GetTempDir + APPNAME + '_updatecheck.ini';

  // Download the check file
  Screen.Cursor := crHourglass;
  try
    Status('Downloading check file ...');
    CheckfileDownload.SendRequest(FCheckFilename);
    Status('Reading check file ...');
    ReadCheckFile;
    // Developer versions probably have "unknown" (0) as revision,
    // which makes it impossible to compare the revisions.
    if Mainform.AppVerRevision = 0 then
      Status('Error: Cannot determine current revision. Using a developer version?')
    else if Mainform.AppVerRevision = BuildRevision then
      Status('Your '+APPNAME+' is up-to-date (no update available).')
    else if groupRelease.Enabled or btnBuild.Enabled then
      Status('Updates available.');
    // Remember when we did the updatecheck to enable the automatic interval
    AppSettings.WriteString(asUpdatecheckLastrun, DateTimeToStr(Now));
  except
    // Do not popup errors, just display them in the status label
    on E:Exception do
      Status(E.Message);
  end;
  if FileExists(FCheckFilename) then
    DeleteFile(FCheckFilename);
  FreeAndNil(CheckfileDownload);
  Screen.Cursor := crDefault;

  // For automatic updatechecks this dialog should close if no updates are available.
  // Using PostMessage, as Self.Close or ModalResult := mrCancel does not work
  // as expected in FormShow
  if AutoClose
    and (not groupRelease.Enabled)
    and ((not CheckForBuildsInAutoMode) or (not btnBuild.Enabled)) then
    PostMessage(Self.Handle, WM_CLOSE, 0, 0);
end;


{**
  Parse check file for updated version + release
}
procedure TfrmUpdateCheck.ReadCheckFile;
var
  Ini : TIniFile;
  ReleaseVersion : String;
  ReleaseRevision: Integer;
  Note : String;
  Compiled : TDateTime;
const
  INISECT_RELEASE = 'Release';
  INISECT_BUILD = 'Build';
begin
  // Read [Release] section of check file
  Ini := TIniFile.Create(FCheckFilename);
  if Ini.SectionExists(INISECT_RELEASE) then begin
    ReleaseVersion := Ini.ReadString(INISECT_RELEASE, 'Version', 'unknown');
    ReleaseRevision := Ini.ReadInteger(INISECT_RELEASE, 'Revision', 0);
    ReleaseURL := Ini.ReadString(INISECT_RELEASE, 'URL', '');
    memoRelease.Lines.Add( 'Version ' + ReleaseVersion + ' (yours: '+Mainform.AppVersion+')' );
    memoRelease.Lines.Add( 'Released: ' + Ini.ReadString(INISECT_RELEASE, 'Date', '') );
    Note := Ini.ReadString(INISECT_RELEASE, 'Note', '');
    if Note <> '' then
      memoRelease.Lines.Add( 'Note: ' + Note );
    btnRelease.Caption := 'Download version ' + ReleaseVersion;
    // Enable the download button if the current version is outdated
    groupRelease.Enabled := ReleaseRevision > Mainform.AppVerRevision;
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
    BuildSize := Ini.ReadInteger(INISECT_BUILD, 'Size', 0);
    memoBuild.Lines.Add( 'Revision ' + IntToStr(BuildRevision) + ' (yours: '+IntToStr(Mainform.AppVerRevision)+')' );
    FileAge(ParamStr(0), Compiled);
    memoBuild.Lines.Add( 'Compiled: ' + Ini.ReadString(INISECT_BUILD, 'Date', '') + ' (yours: '+DateToStr(Compiled)+')' );
    Note := Ini.ReadString(INISECT_BUILD, 'Note', '');
    if Note <> '' then
      memoBuild.Lines.Add( 'Notes: * ' + StringReplace(Note, '%||%', CRLF+'* ', [rfReplaceAll] ) );
    btnBuild.Caption := 'Download and install build ' + IntToStr(BuildRevision);
    // A new release should have priority over a new nightly build.
    // So the user should not be able to download a newer build here
    // before having installed the new release.
    btnBuild.Enabled := (Mainform.AppVerRevision = 0) or ((BuildRevision > Mainform.AppVerRevision) and (not btnRelease.Enabled));
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
  Download: THttpDownLoad;
  ExeName, DownloadFilename, UpdaterFilename: String;
  ResInfoblockHandle: HRSRC;
  ResHandle: THandle;
  ResPointer: PChar;
  Stream: TMemoryStream;
begin
  Download := THttpDownload.Create(Self);
  Download.URL := BuildURL;
  ExeName := ExtractFileName(Application.ExeName);

  // Save the file in a temp directory
  DownloadFilename := GetTempDir + ExeName;
  Download.OnProgress := DownloadProgress;

  // Delete probably previously downloaded file
  if FileExists(DownloadFilename) then
    DeleteFile(DownloadFilename);

  try
    // Do the download
    Download.SendRequest(DownloadFilename);

    // Check if downloaded file exists
    if not FileExists(DownloadFilename) then
      Raise Exception.Create('Downloaded file not found: '+DownloadFilename);

    Status('Update in progress ...');
    ResInfoblockHandle := FindResource(HInstance, 'UPDATER', 'EXE');
    ResHandle := LoadResource(HInstance, ResInfoblockHandle);
    if ResHandle <> 0 then begin
      Stream := TMemoryStream.Create;
      try
        ResPointer := LockResource(ResHandle);
        Stream.WriteBuffer(ResPointer[0], SizeOfResource(HInstance, ResInfoblockHandle));
        Stream.Position := 0;
        UpdaterFilename := GetTempDir + AppName+'_updater.exe';
        if FileExists(UpdaterFilename) and (Stream.Size = _GetFileSize(UpdaterFilename)) then
          // Do not replace old updater if it's still valid. Avoids annoyance for cases in which
          // user has whitelisted this .exe in his antivirus or whatever software.
        else
          Stream.SaveToFile(UpdaterFilename);
        // Calling the script will now post a WM_CLOSE this running exe...
        ShellExec(UpdaterFilename, '', '"'+ParamStr(0)+'" "'+DownloadFilename+'"');
      finally
        UnlockResource(ResHandle);
        FreeResource(ResHandle);
        Stream.Free;
      end;
    end;
  except
    on E:Exception do
      ErrorDialog(E.Message);
  end;
end;


{**
  Download progress event
}
procedure TfrmUpdateCheck.DownloadProgress(Sender: TObject);
var
  Download: THttpDownload;
begin
  if FLastStatusUpdate > GetTickCount-200 then
    Exit;
  Download := Sender as THttpDownload;
  Status('Downloading: '+FormatByteNumber(Download.BytesRead)+' / '+FormatByteNumber(Download.ContentLength) + ' ...');
  FLastStatusUpdate := GetTickCount;
end;


end.
