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
    groupRelease: TGroupBox;
    LinkLabelRelease: TLabel;
    lblStatus: TLabel;
    memoRelease: TMemo;
    popupDownloadRelease: TPopupMenu;
    CopydownloadURL1: TMenuItem;
    btnDonate: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LinkLabelReleaseLinkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CopydownloadURL1Click(Sender: TObject);
  const
    SLinkDownloadRelease= 'download-release';
    SLinkInstructionsPortable = 'instructions-portable';
    SLinkChangelog = 'changelog';
  private
    { Private declarations }
    procedure Status(txt: String);
    function GetLinkUrl(Sender: TObject; LinkType: String): String;
  public
    { Public declarations }
    procedure ReadCheckFile;
  end;


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
  Width := AppSettings.ReadInt(asUpdateCheckWindowWidth);
  Height := AppSettings.ReadInt(asUpdateCheckWindowHeight);
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
    else if groupRelease.Enabled then
      Status(_('Updates available.'))
    else
      Status(f_('Your %s is up-to-date (no update available).', [APPNAME]));
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
begin
  // Init GUI controls
  LinkLabelRelease.Enabled := False;
  memoRelease.Clear;

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


procedure TfrmUpdateCheck.CopydownloadURL1Click(Sender: TObject);
begin
  Clipboard.TryAsText := GetLinkUrl(LinkLabelRelease, SLinkDownloadRelease);
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

  else if LinkType = SLinkChangelog then
    Result := 'changes-lazarus'

  else
    Result := '';

  Result := APPDOMAIN + Result;
end;


end.
