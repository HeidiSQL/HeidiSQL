; HeidiSQL setup script for Innosetup

; Set commonly used constants for preprocessor
#define ProgName "HeidiSQL"
#define ProgNameLower LowerCase(ProgName)
#define ProgExeName ProgNameLower + ".exe"
#define ProgVersion GetFileVersion(AddBackslash(SourcePath) + ProgExeName)
#define ProgShortVersion Copy(ProgVersion, 1, 3)
#define WebSite "http://www." + ProgNameLower + ".com/"
#define OutDir "."
#define ResourceDir OutDir + "\..\res\"


[Setup]
AppId={#ProgName}
AppName={#ProgName}
AppVerName={#ProgName} {#ProgShortVersion}
VersionInfoVersion={#ProgVersion}

; Displayed on the "Support" dialog of the Add/Remove Programs Control Panel applet:
AppVersion={#ProgShortVersion}
AppPublisher=Ansgar Becker
AppPublisherURL={#WebSite}
AppSupportURL={#WebSite}forum.php
AppUpdatesURL={#WebSite}download.php
AppContact={#ProgNameLower}@anse.de
AppReadmeFile={app}\readme.txt

DefaultDirName={pf}\{#ProgName}
DefaultGroupName={#ProgName}
LicenseFile=license.txt
ChangesAssociations=yes
WizardImageFile={#ResourceDir}installer-logo.bmp
WizardImageBackColor=$ffffff
WizardSmallImageFile={#ResourceDir}installer-small-logo.bmp
OutputDir={#OutDir}
OutputBaseFilename={#ProgName}_{#ProgShortVersion}_Setup
UninstallDisplayIcon={app}\{#ProgExeName}
SetupIconFile={#ResourceDir}mainicon.ico
UsePreviousAppDir=yes
DirExistsWarning=auto
PrivilegesRequired=none

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"; MinVersion: 4,4
Name: "associatesqlfiles"; Description: "Associate .&SQL files with {#ProgName}"; GroupDescription: "Options:";
Name: "activate_updatechecks"; Description: "Automatically check {#WebSite} for updates"; GroupDescription: "Options:";
Name: "activate_statistics"; Description: "Automatically report client and server versions on {#WebSite}"; GroupDescription: "Options:";

[InstallDelete]
Type: files; Name: "{app}\libmysql40.dll"
Type: files; Name: "{app}\libmysql41.dll"
Type: files; Name: "{app}\{#ProgExeName}.manifest"
Type: files; Name: "{app}\{#ProgNameLower}.url"
Type: files; Name: "{app}\{#ProgNameLower}_forum.url"
Type: files; Name: "{app}\donate.url"
Type: files; Name: "{app}\function.txt"

[Files]
Source: "{#ProgExeName}"; DestDir: "{app}"; Flags: ignoreversion
Source: "readme.txt"; DestDir: "{app}"; Flags: ignoreversion isreadme
Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "gpl.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "libmysql.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "Snippets\*.sql"; DestDir: "{commonappdata}\{#ProgName}\Snippets";

[Icons]
Name: "{group}\{#ProgName}"; Filename: "{app}\{#ProgExeName}"
Name: "{group}\Readme file"; Filename: "{app}\readme.txt"
Name: "{userdesktop}\{#ProgName}"; Filename: "{app}\{#ProgExeName}"; MinVersion: 4,4; Tasks: desktopicon

[Registry]
Root: HKCR; Subkey: ".sql"; ValueType: string; ValueName: ""; ValueData: "SQLScriptFile"; Flags: uninsdeletevalue; Tasks: associatesqlfiles
Root: HKCR; Subkey: "SQLScriptFile"; ValueType: string; ValueName: ""; ValueData: "SQL-Script"; Flags: uninsdeletekey; Tasks: associatesqlfiles
Root: HKCR; Subkey: "SQLScriptFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#ProgExeName},0"; Tasks: associatesqlfiles
Root: HKCR; Subkey: "SQLScriptFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#ProgExeName}"" ""%1"""; Tasks: associatesqlfiles
; Enable auto-updatechecks if this option was checked. Only save the value when it's checked, as the default in preferences is False (see const.inc)
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "Updatecheck"; ValueData: 1; Tasks: activate_updatechecks
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "DoUsageStatistics"; ValueData: 1; Tasks: activate_statistics

[Run]
Filename: "{app}\{#ProgExeName}"; Description: "Launch {#ProgName}"; Flags: nowait postinstall skipifsilent

[Code]
procedure DonateClick(Sender: TObject);
var
  ErrorCode: Integer;
begin
  ShellExec('open', '{#WebSite}donate.php', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

procedure InitializeWizard();
var
  txt: TNewStaticText;
  btn: TButton;
begin
  txt := TNewStaticText.Create(WizardForm);
  txt.Parent := WizardForm.FinishedPage;
  txt.Caption := '{#ProgName} is free software. You may make a donation:';
  txt.Left := WizardForm.FinishedLabel.Left;
  txt.Top := WizardForm.FinishedLabel.Top + 130;
  txt.AutoSize := True;

  btn := TButton.Create(WizardForm);
  btn.Parent := WizardForm.FinishedPage;
  btn.Left := txt.Left;
  btn.Top := txt.Top + txt.Height + 10;
  btn.Width := 120;
  btn.Height := WizardForm.CancelButton.Height;
  btn.Caption := 'Donate';
  btn.OnClick := @DonateClick;
end;
