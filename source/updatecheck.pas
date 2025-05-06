unit updatecheck;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, Forms, StdCtrls, IniFiles, Controls, Graphics,
  apphelpers, ExtCtrls, extra_controls, StrUtils, Dialogs,
  Menus, Clipbrd, generic_types, DateUtils;

type

  { TfrmUpdateCheck }

  TfrmUpdateCheck = class(TExtForm)
    btnCancel: TButton;
    groupBuild: TGroupBox;
    btnBuild: TButton;
    groupRelease: TGroupBox;
    LinkLabelRelease: TLabel;
    lblStatus: TLabel;
    memoRelease: TMemo;
    memoBuild: TMemo;
    btnChangelog: TButton;
    popupDownloadRelease: TPopupMenu;
    CopydownloadURL1: TMenuItem;
    btnDonate: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LinkLabelReleaseLinkClick(Sender: TObject);
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
    FRestartTaskName: String;
    procedure Status(txt: String);
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

{$R *.lfm}

{$I const.inc}



{**
  Set defaults
}
procedure TfrmUpdateCheck.FormCreate(Sender: TObject);
begin
  // Should be false by default. Callers can set this to True after Create()
  btnDonate.OnClick := MainForm.DonateClick;
  btnDonate.Visible := MainForm.HasDonated(False) = nbFalse;
  btnDonate.Caption := f_('Donate to the %s project', [APPNAME]);
  HasSizeGrip := True;
  Width := AppSettings.ReadInt(asUpdateCheckWindowWidth);
  Height := AppSettings.ReadInt(asUpdateCheckWindowHeight);
  FRestartTaskName := 'yet_invalid';
end;

procedure TfrmUpdateCheck.FormClose(Sender: TObject; var Action: TCloseAction);
begin
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
  btnCancel.TrySetFocus;
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

  // Prepare download
  CheckfileDownload := THttpDownload.Create(Self);
  CheckfileDownload.TimeOut := 5;
  CheckfileDownload.URL := APPDOMAIN+'updatecheck.php?r='+IntToStr(Mainform.AppVerRevision)+'&bits='+IntToStr(GetExecutableBits)+'&os='+EncodeURLParam(GetOS.ToLower)+'&t='+EncodeURLParam(DateTimeToStr(Now));
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
    //LinkLabelRelease.Caption := '<a id="'+SLinkDownloadRelease+'">' + LinkLabelRelease.Caption + '</a>';
    //if AppSettings.PortableMode then begin
    //  LinkLabelRelease.Caption := LinkLabelRelease.Caption + '   <a id="'+SLinkInstructionsPortable+'">'+_('Update instructions')+'</a>';
    //end;

    // Enable the download button if the current version is outdated
    groupRelease.Enabled := ReleaseRevision > Mainform.AppVerRevision;
    LinkLabelRelease.Enabled := groupRelease.Enabled;
    LinkLabelRelease.Font.Style := LinkLabelRelease.Font.Style + [fsUnderline];
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
      //btnBuild.Enabled := (Mainform.AppVerRevision = 0) or ((BuildRevision > Mainform.AppVerRevision) and (not LinkLabelRelease.Enabled));
    end
    else begin
      btnBuild.Caption := _('No build updates for 32 bit version');
    end;

    if btnBuild.Enabled then begin
      TaskXmlFile := GetTempDir + APPNAME + '_task_restart.xml';
      SaveUnicodeFile(TaskXmlFile, GetTaskXmlFileContents, UTF8NoBOMEncoding);
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
procedure TfrmUpdateCheck.LinkLabelReleaseLinkClick(Sender: TObject);
begin
  ShellExec(GetLinkUrl(LinkLabelRelease, SLinkDownloadRelease));
end;


procedure TfrmUpdateCheck.btnChangelogClick(Sender: TObject);
begin
  ShellExec(GetLinkUrl(Sender, SLinkChangelog));
end;


procedure TfrmUpdateCheck.CopydownloadURL1Click(Sender: TObject);
begin
  Clipboard.TryAsText := GetLinkUrl(LinkLabelRelease, SLinkDownloadRelease);
end;

procedure TfrmUpdateCheck.btnBuildClick(Sender: TObject);
begin
  // No auto-update
end;

procedure TfrmUpdateCheck.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteInt(asUpdateCheckWindowWidth, ScaleFormToDesign(Width));
  AppSettings.WriteInt(asUpdateCheckWindowHeight, ScaleFormToDesign(Height));
end;


function TfrmUpdateCheck.GetLinkUrl(Sender: TObject; LinkType: String): String;
var
  DownloadParam, PlaceParam, OsParam: String;
begin
  PlaceParam := 'place='+EncodeURLParam(TWinControl(Sender).Name);
  OsParam := 'os='+EncodeURLParam(GetOS.ToLower);

  if LinkType = SLinkDownloadRelease then begin
    if AppSettings.PortableMode then begin
      if GetExecutableBits = 64 then
        DownloadParam := 'download=portable-64'
      else
        DownloadParam := 'download=portable';
    end else begin
      DownloadParam := 'download=installer';
    end;
    Result := 'download.php?'+DownloadParam+'&'+PlaceParam+'&'+OsParam;
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
    '      <Command>"' + ParamStr(0) + '"</Command>' + sLineBreak +
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
