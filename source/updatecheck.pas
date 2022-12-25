unit updatecheck;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, StdCtrls, IniFiles, Controls, Graphics,
  apphelpers, gnugettext, ExtCtrls, extra_controls, System.StrUtils, Vcl.Dialogs,
  Vcl.Menus, Vcl.Clipbrd, generic_types, System.DateUtils, System.IOUtils;

type
  TfrmUpdateCheck = class(TExtForm)
    btnCancel: TButton;
    groupBuild: TGroupBox;
    btnBuild: TButton;
    groupRelease: TGroupBox;
    LinkLabelRelease: TLinkLabel;
    lblStatus: TLabel;
    memoRelease: TMemo;
    memoBuild: TMemo;
    imgDonate: TImage;
    btnChangelog: TButton;
    popupDownloadRelease: TPopupMenu;
    CopydownloadURL1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure LinkLabelReleaseLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure FormShow(Sender: TObject);
    procedure btnChangelogClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CopydownloadURL1Click(Sender: TObject);
  const
    SLinkDownloadRelease= 'download-release';
    SLinkInstructionsPortable = 'instructions-portable';
    SLinkChangelog = 'changelog';
  private
    { Private declarations }
    BuildURL: String;
    FLastStatusUpdate: Cardinal;
    FRestartTaskName: String;
    procedure Status(txt: String);
    procedure DownloadProgress(Sender: TObject);
    function GetLinkUrl(Sender: TObject; LinkType: String): String;
    function GetTaskXmlFileContents: String;
  public
    { Public declarations }
    BuildRevision: Integer;
    procedure ReadCheckFile;
  end;

procedure DeleteRestartTask;


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
  HasSizeGrip := True;
  FRestartTaskName := 'yet_invalid';
end;

procedure TfrmUpdateCheck.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AppSettings.WriteIntDpiAware(asUpdateCheckWindowWidth, Self, Width);
  AppSettings.WriteIntDpiAware(asUpdateCheckWindowHeight, Self, Height);
  if ModalResult <> btnBuild.ModalResult then begin
    DeleteRestartTask;
  end;
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
  Width := AppSettings.ReadIntDpiAware(asUpdateCheckWindowWidth, Self);
  Height := AppSettings.ReadIntDpiAware(asUpdateCheckWindowHeight, Self);
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
  CheckFilename, TaskXmlFile: String;
  Ini: TIniFile;
  ReleaseVersion, ReleasePackage: String;
  ReleaseRevision: Integer;
  Note: String;
  Compiled: TDateTime;
const
  INISECT_RELEASE = 'Release';
  INISECT_BUILD = 'Build';
begin
  // Init GUI controls
  LinkLabelRelease.Enabled := False;
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
    ReleasePackage := IfThen(AppSettings.PortableMode, 'portable', 'installer');
    memoRelease.Lines.Add(f_('Version %s (yours: %s)', [ReleaseVersion, Mainform.AppVersion]));
    memoRelease.Lines.Add(f_('Released: %s', [Ini.ReadString(INISECT_RELEASE, 'Date', '')]));
    Note := Ini.ReadString(INISECT_RELEASE, 'Note', '');
    if Note <> '' then
      memoRelease.Lines.Add(_('Notes') + ': ' + Note);

    LinkLabelRelease.Caption := f_('Download version %s (%s)', [ReleaseVersion, ReleasePackage]);
    LinkLabelRelease.Caption := '<a id="'+SLinkDownloadRelease+'">' + LinkLabelRelease.Caption + '</a>';
    if AppSettings.PortableMode then begin
      LinkLabelRelease.Caption := LinkLabelRelease.Caption + '   <a id="'+SLinkInstructionsPortable+'">'+_('Update instructions')+'</a>';
    end;

    // Enable the download button if the current version is outdated
    groupRelease.Enabled := ReleaseRevision > Mainform.AppVerRevision;
    LinkLabelRelease.Enabled := groupRelease.Enabled;
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
    if GetExecutableBits = 64 then begin
      btnBuild.Caption := f_('Download and install build %d', [BuildRevision]);
      // A new release should have priority over a new nightly build.
      // So the user should not be able to download a newer build here
      // before having installed the new release.
      btnBuild.Enabled := (Mainform.AppVerRevision = 0) or ((BuildRevision > Mainform.AppVerRevision) and (not LinkLabelRelease.Enabled));
    end
    else begin
      btnBuild.Caption := _('No build updates for 32 bit version');
    end;

    if btnBuild.Enabled then begin
      TaskXmlFile := GetTempDir + APPNAME + '_task_restart.xml';
      SaveUnicodeFile(TaskXmlFile, GetTaskXmlFileContents);
      FRestartTaskName := ValidFilename(ParamStr(0));
      ShellExec('schtasks', '', '/Create /TN "'+FRestartTaskName+'" /xml '+TaskXmlFile, True);
    end;

  end;

  if FileExists(CheckFilename) then
    DeleteFile(CheckFilename);
  FreeAndNil(CheckfileDownload);
end;


{**
  Download release package via web browser
}
procedure TfrmUpdateCheck.LinkLabelReleaseLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  case LinkType of

    sltURL: ShellExec(Link);

    sltID: begin
      if Link = SLinkDownloadRelease then begin
        ShellExec(GetLinkUrl(Sender, Link));
        Close;
      end
      else if Link = SLinkInstructionsPortable then begin
        MessageDialog(f_('Download the portable package and extract it in %s', [ExtractFilePath(Application.ExeName)]), mtInformation, [mbOK]);
      end;
    end;

  end;
end;


procedure TfrmUpdateCheck.btnChangelogClick(Sender: TObject);
begin
  ShellExec(GetLinkUrl(Sender, SLinkChangelog));
end;


procedure TfrmUpdateCheck.CopydownloadURL1Click(Sender: TObject);
begin
  Clipboard.AsText := GetLinkUrl(LinkLabelRelease, SLinkDownloadRelease);
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
  DoOverwrite: Boolean;
  UpdaterAge: TDateTime;
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

        DoOverwrite := True;
        if FileExists(UpdaterFilename) and (Stream.Size = _GetFileSize(UpdaterFilename)) then begin
          // Do not replace old updater if it's still valid. Avoids annoyance for cases in which
          // user has whitelisted this .exe in his antivirus or whatever software.
          FileAge(UpdaterFilename, UpdaterAge);
          if Abs(DaysBetween(Now, UpdaterAge)) < 30 then
            DoOverwrite := False;
        end;

        if DoOverwrite then begin
          Stream.SaveToFile(UpdaterFilename);
        end;

        // Calling the script will now post a WM_CLOSE this running exe...
        ShellExec(UpdaterFilename, '', '"'+ParamStr(0)+'" "'+DownloadFilename+'" "'+FRestartTaskName+'"');
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


function TfrmUpdateCheck.GetLinkUrl(Sender: TObject; LinkType: String): String;
var
  DownloadParam, PlaceParam: String;
begin
  PlaceParam := 'place='+EncodeURLParam(TWinControl(Sender).Name);

  if LinkType = SLinkDownloadRelease then begin
    if AppSettings.PortableMode then begin
      if GetExecutableBits = 64 then
        DownloadParam := 'download=portable-64'
      else
        DownloadParam := 'download=portable';
    end else begin
      DownloadParam := 'download=installer';
    end;
    Result := 'download.php?'+DownloadParam+'&'+PlaceParam;
  end

  else if LinkType = SLinkChangelog then begin
    Result := 'download.php?'+PlaceParam+'#nightlybuilds';
  end;

  Result := APPDOMAIN + Result;
end;


function TfrmUpdateCheck.GetTaskXmlFileContents: String;
begin
  Result := '<?xml version="1.0" encoding="UTF-16"?>' + sLineBreak +
    '<Task version="1.2" xmlns="http://schemas.microsoft.com/windows/2004/02/mit/task">' + sLineBreak +
    '  <RegistrationInfo>' + sLineBreak +
    '    <Date>2022-12-24T12:39:17.5068755</Date>' + sLineBreak +
    '    <Author>' + APPNAME + ' ' + MainForm.AppVersion + '</Author>' + sLineBreak +
    '    <URI>\' + APPNAME + '_restart</URI>' + sLineBreak +
    '  </RegistrationInfo>' + sLineBreak +
    '  <Triggers>' + sLineBreak +
    '    <TimeTrigger>' + sLineBreak +
    '      <StartBoundary>2022-12-24T12:42:36</StartBoundary>' + sLineBreak +
    '      <Enabled>true</Enabled>' + sLineBreak +
    '    </TimeTrigger>' + sLineBreak +
    '  </Triggers>' + sLineBreak +
    '  <Principals>' + sLineBreak +
    '    <Principal id="Author">' + sLineBreak +
    // Note: no <UserId> with the current users SID
    '      <LogonType>InteractiveToken</LogonType>' + sLineBreak +
    '      <RunLevel>LeastPrivilege</RunLevel>' + sLineBreak +
    '    </Principal>' + sLineBreak +
    '  </Principals>' + sLineBreak +
    '  <Settings>' + sLineBreak +
    '    <MultipleInstancesPolicy>IgnoreNew</MultipleInstancesPolicy>' + sLineBreak +
    '    <DisallowStartIfOnBatteries>true</DisallowStartIfOnBatteries>' + sLineBreak +
    '    <StopIfGoingOnBatteries>true</StopIfGoingOnBatteries>' + sLineBreak +
    '    <AllowHardTerminate>true</AllowHardTerminate>' + sLineBreak +
    '    <StartWhenAvailable>false</StartWhenAvailable>' + sLineBreak +
    '    <RunOnlyIfNetworkAvailable>false</RunOnlyIfNetworkAvailable>' + sLineBreak +
    '    <IdleSettings>' + sLineBreak +
    '      <StopOnIdleEnd>true</StopOnIdleEnd>' + sLineBreak +
    '      <RestartOnIdle>false</RestartOnIdle>' + sLineBreak +
    '    </IdleSettings>' + sLineBreak +
    '    <AllowStartOnDemand>true</AllowStartOnDemand>' + sLineBreak +
    '    <Enabled>true</Enabled>' + sLineBreak +
    '    <Hidden>false</Hidden>' + sLineBreak +
    '    <RunOnlyIfIdle>false</RunOnlyIfIdle>' + sLineBreak +
    '    <WakeToRun>false</WakeToRun>' + sLineBreak +
    '    <ExecutionTimeLimit>PT72H</ExecutionTimeLimit>' + sLineBreak +
    '    <Priority>7</Priority>' + sLineBreak +
    '  </Settings>' + sLineBreak +
    '  <Actions Context="Author">' + sLineBreak +
    '    <Exec>' + sLineBreak +
    '      <Command>' + ParamStr(0) + '</Command>' + sLineBreak +
    '      <Arguments>--runfrom=scheduler</Arguments>' + sLineBreak +
    '    </Exec>' + sLineBreak +
    '  </Actions>' + sLineBreak +
    '</Task>';
end;


procedure DeleteRestartTask;
begin
  // TN = Task Name
  // F = Force, suppress prompt
  ShellExec('schtasks', '', '/Delete /TN "'+ValidFilename(ParamStr(0))+'" /F', True);
end;

end.
