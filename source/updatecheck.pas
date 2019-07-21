unit updatecheck;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, StdCtrls, IniFiles, Controls, Graphics,
  apphelpers, gnugettext, ExtCtrls, extra_controls;

type
  TfrmUpdateCheck = class(TExtForm)
    btnCancel: TButton;
    groupBuild: TGroupBox;
    btnBuild: TButton;
    groupRelease: TGroupBox;
    btnRelease: TButton;
    lblStatus: TLabel;
    memoRelease: TMemo;
    memoBuild: TMemo;
    imgDonate: TImage;
    btnChangelog: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure btnReleaseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnChangelogClick(Sender: TObject);
  private
    { Private declarations }
    ReleaseURL, BuildURL : String;
    FLastStatusUpdate: Cardinal;
    procedure Status(txt: String);
    procedure DownloadProgress(Sender: TObject);
  public
    { Public declarations }
    BuildRevision: Integer;
    procedure ReadCheckFile;
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
  imgDonate.OnClick := MainForm.DonateClick;
  imgDonate.Visible := MainForm.HasDonated(False) = nbFalse;
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
  Caption := f_('Check for %s updates', [APPNAME]) + ' ...';
  Screen.Cursor := crHourglass;
  try
    Status(_('Downloading check file')+' ...');
    ReadCheckFile;
    // Developer versions probably have "unknown" (0) as revision,
    // which makes it impossible to compare the revisions.
    if Mainform.AppVerRevision = 0 then
      Status(_('Error: Cannot determine current revision. Using a developer version?'))
    else if Mainform.AppVerRevision = BuildRevision then
      Status(f_('Your %s is up-to-date (no update available).', [APPNAME]))
    else if groupRelease.Enabled or btnBuild.Enabled then
      Status(_('Updates available.'));
  except
    // Do not popup errors, just display them in the status label
    on E:Exception do
      Status(E.Message);
  end;
  Screen.Cursor := crDefault;
end;


{**
  Parse check file for updated version + release
}
procedure TfrmUpdateCheck.ReadCheckFile;
var
  CheckfileDownload: THttpDownLoad;
  CheckFilename: String;
  Ini: TIniFile;
  ReleaseVersion: String;
  ReleaseRevision: Integer;
  Note: String;
  Compiled: TDateTime;
const
  INISECT_RELEASE = 'Release';
  INISECT_BUILD = 'Build';
begin
  // Init GUI controls
  btnRelease.Enabled := False;
  btnBuild.Enabled := False;
  memoRelease.Clear;
  memoBuild.Clear;

  if RunningAsUwp then begin
    raise Exception.Create(
      f_('Please update %s through the Microsoft Store.', [APPNAME])
      );
  end;

  // Prepare download
  CheckfileDownload := THttpDownload.Create(Self);
  CheckfileDownload.TimeOut := 5;
  CheckfileDownload.URL := APPDOMAIN+'updatecheck.php?r='+IntToStr(Mainform.AppVerRevision)+'&bits='+IntToStr(GetExecutableBits)+'&t='+DateTimeToStr(Now);
  CheckFilename := GetTempDir + APPNAME + '_updatecheck.ini';

  // Download the check file
  CheckfileDownload.SendRequest(CheckFilename);
  // Remember when we did the updatecheck to enable the automatic interval
  AppSettings.WriteString(asUpdatecheckLastrun, DateTimeToStr(Now));

  // Read [Release] section of check file
  Ini := TIniFile.Create(CheckFilename);
  if Ini.SectionExists(INISECT_RELEASE) then begin
    ReleaseVersion := Ini.ReadString(INISECT_RELEASE, 'Version', 'unknown');
    ReleaseRevision := Ini.ReadInteger(INISECT_RELEASE, 'Revision', 0);
    ReleaseURL := Ini.ReadString(INISECT_RELEASE, 'URL', '');
    memoRelease.Lines.Add(f_('Version %s (yours: %s)', [ReleaseVersion, Mainform.AppVersion]));
    memoRelease.Lines.Add(f_('Released: %s', [Ini.ReadString(INISECT_RELEASE, 'Date', '')]));
    Note := Ini.ReadString(INISECT_RELEASE, 'Note', '');
    if Note <> '' then
      memoRelease.Lines.Add(_('Notes') + ': ' + Note);
    btnRelease.Caption := f_('Download version %s', [ReleaseVersion]);
    // Enable the download button if the current version is outdated
    groupRelease.Enabled := ReleaseRevision > Mainform.AppVerRevision;
    btnRelease.Enabled := groupRelease.Enabled;
    memoRelease.Enabled := groupRelease.Enabled;
    if not memoRelease.Enabled then
      memoRelease.Font.Color := GetThemeColor(cl3DDkShadow)
    else
      memoRelease.Font.Color := GetThemeColor(clWindowText);
  end;

  // Read [Build] section of check file
  if Ini.SectionExists(INISECT_BUILD) then begin
    BuildRevision := Ini.ReadInteger(INISECT_BUILD, 'Revision', 0);
    BuildURL := Ini.ReadString(INISECT_BUILD, 'URL', '');
    memoBuild.Lines.Add(f_('Revision %d (yours: %d)', [BuildRevision, Mainform.AppVerRevision]));
    FileAge(ParamStr(0), Compiled);
    memoBuild.Lines.Add(f_('Compiled: %s (yours: %s)', [Ini.ReadString(INISECT_BUILD, 'Date', ''), DateToStr(Compiled)]));
    Note := Ini.ReadString(INISECT_BUILD, 'Note', '');
    if Note <> '' then
      memoBuild.Lines.Add(_('Notes') + ': * ' + StringReplace(Note, '%||%', CRLF+'* ', [rfReplaceAll] ) );
    btnBuild.Caption := f_('Download and install build %d', [BuildRevision]);
    // A new release should have priority over a new nightly build.
    // So the user should not be able to download a newer build here
    // before having installed the new release.
    btnBuild.Enabled := (Mainform.AppVerRevision = 0) or ((BuildRevision > Mainform.AppVerRevision) and (not btnRelease.Enabled));
  end;

  if FileExists(CheckFilename) then
    DeleteFile(CheckFilename);
  FreeAndNil(CheckfileDownload);
end;


{**
  Download release installer via web browser
}
procedure TfrmUpdateCheck.btnReleaseClick(Sender: TObject);
begin
  ShellExec(APPDOMAIN+'download.php?download=installer');
end;


procedure TfrmUpdateCheck.btnChangelogClick(Sender: TObject);
begin
  ShellExec(APPDOMAIN+'download.php?place='+EncodeURLParam(TButton(Sender).Name)+'#nightlybuilds');
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
  BuildSizeDownloaded: Int64;
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
      Raise Exception.CreateFmt(_('Downloaded file not found: %s'), [DownloadFilename]);
    BuildSizeDownloaded := _GetFileSize(DownloadFilename);
    if (Download.ContentLength > 0) and (BuildSizeDownloaded < Download.ContentLength) then
      Raise Exception.CreateFmt(_('Downloaded file corrupted: %s (Size is %d and should be %d)'), [DownloadFilename, BuildSizeDownloaded, Download.ContentLength]);

    Status(_('Update in progress')+' ...');
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
  Status(f_('Downloading: %s / %s', [FormatByteNumber(Download.BytesRead), FormatByteNumber(Download.ContentLength)]) + ' ...');
  FLastStatusUpdate := GetTickCount;
end;


end.
