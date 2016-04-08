; HeidiSQL setup script for Innosetup
; 32bit mode only, for users who want 32bit installed on a 64bit Windows
; Always keep this file synchronized with the universal installer (heidisql.iss)!!

; Set commonly used constants for preprocessor
#define ProgName "HeidiSQL"
#define ProgNameLower LowerCase(ProgName)
#define ProgExeName ProgNameLower + ".exe"
#define ProgVersion GetFileVersion(AddBackslash(SourcePath) + ProgExeName)
#define ProgShortVersion Copy(ProgVersion, 1, 3)
#define WebSite "http://www." + ProgNameLower + ".com/"
#define OutDir "."
#define ResourceDir OutDir + "\..\res\"

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "pt_BR"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "ca"; MessagesFile: "compiler:Languages\Catalan.isl"
Name: "co"; MessagesFile: "compiler:Languages\Corsican.isl"
Name: "cs"; MessagesFile: "compiler:Languages\Czech.isl"
Name: "da"; MessagesFile: "compiler:Languages\Danish.isl"
Name: "nl"; MessagesFile: "compiler:Languages\Dutch.isl"
Name: "fi"; MessagesFile: "compiler:Languages\Finnish.isl"
Name: "fr"; MessagesFile: "compiler:Languages\French.isl"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"
Name: "el"; MessagesFile: "compiler:Languages\Greek.isl"
Name: "he"; MessagesFile: "compiler:Languages\Hebrew.isl"
Name: "hu"; MessagesFile: "compiler:Languages\Hungarian.isl"
Name: "it"; MessagesFile: "compiler:Languages\Italian.isl"
Name: "ja"; MessagesFile: "compiler:Languages\Japanese.isl"
Name: "no"; MessagesFile: "compiler:Languages\Norwegian.isl"
Name: "pl"; MessagesFile: "compiler:Languages\Polish.isl"
Name: "pt"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "ru"; MessagesFile: "compiler:Languages\Russian.isl"
Name: "sr"; MessagesFile: "compiler:Languages\SerbianLatin.isl"
Name: "sl"; MessagesFile: "compiler:Languages\Slovenian.isl"
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"
Name: "uk"; MessagesFile: "compiler:Languages\Ukrainian.isl"

[Setup]
AppId={#ProgName}
AppName={#ProgName}
AppVerName={#ProgName} {#ProgVersion}
VersionInfoVersion={#ProgVersion}

; Displayed on the "Support" dialog of the Add/Remove Programs Control Panel applet:
AppVersion={#ProgShortVersion}
AppPublisher=Ansgar Becker
AppPublisherURL={#WebSite}
AppSupportURL={#WebSite}forum.php
AppUpdatesURL={#WebSite}download.php
AppContact={#ProgNameLower}@anse.de
AppReadmeFile=http://www.heidisql.com/help.php?place=installer

CloseApplications=yes
ShowLanguageDialog=auto
DefaultDirName={pf}\{#ProgName}
DefaultGroupName={#ProgName}
AllowNoIcons=yes
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
Source: "{#ProgNameLower}32.exe"; DestDir: "{app}"; DestName: "{#ProgExeName}"; Flags: ignoreversion
Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "gpl.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "libmysql32.dll"; DestDir: "{app}"; DestName: "libmysql.dll"; Flags: ignoreversion
Source: "libpq32.dll"; DestDir: "{app}"; DestName: "libpq.dll"; Flags: ignoreversion
Source: "libintl-832.dll"; DestDir: "{app}"; DestName: "libintl-8.dll"; Flags: ignoreversion
Source: "ssleay32-32.dll"; DestDir: "{app}"; DestName: "ssleay32.dll"; Flags: ignoreversion
Source: "libeay32-32.dll"; DestDir: "{app}"; DestName: "libeay32.dll"; Flags: ignoreversion
Source: "libiconv-232.dll"; DestDir: "{app}"; DestName: "libiconv-2.dll"; Flags: ignoreversion
Source: "plugins32\*.dll"; DestDir: "{app}\plugins"; Flags: ignoreversion
Source: "Snippets\*.sql"; DestDir: "{commonappdata}\{#ProgName}\Snippets";

[Icons]
Name: "{group}\{#ProgName}"; Filename: "{app}\{#ProgExeName}"
Name: "{group}\General help"; Filename: "http://www.heidisql.com/help.php?place=startmenu"
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
  ShellExec('open', '{#WebSite}donatebutton.php?place=installer', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
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
