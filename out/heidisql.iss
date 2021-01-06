; HeidiSQL setup script for Innosetup

; Set commonly used constants for preprocessor
#define ProgName "HeidiSQL"
#define ProgNameLower LowerCase(ProgName)
#define ProgExeName ProgNameLower + ".exe"
#define ProgVersion GetFileVersion(AddBackslash(SourcePath) + ProgNameLower + "32.exe")
; Take care: this takes the first 4(!) chars of the exe's version string, eg "10.0"
#define ProgShortVersion Copy(ProgVersion, 1, 4)
#define WebSite "https://www." + ProgNameLower + ".com/"
#define OutDir "."
#define ResourceDir OutDir + "\..\res\"

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "hy"; MessagesFile: "compiler:Languages\Armenian.isl"
Name: "ca"; MessagesFile: "compiler:Languages\Catalan.isl"
Name: "co"; MessagesFile: "compiler:Languages\Corsican.isl"
Name: "cs"; MessagesFile: "compiler:Languages\Czech.isl"
Name: "da"; MessagesFile: "compiler:Languages\Danish.isl"
Name: "nl"; MessagesFile: "compiler:Languages\Dutch.isl"
Name: "fi"; MessagesFile: "compiler:Languages\Finnish.isl"
Name: "fr"; MessagesFile: "compiler:Languages\French.isl"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"
Name: "he"; MessagesFile: "compiler:Languages\Hebrew.isl"
Name: "is"; MessagesFile: "compiler:Languages\Icelandic.isl"
Name: "it"; MessagesFile: "compiler:Languages\Italian.isl"
Name: "ja"; MessagesFile: "compiler:Languages\Japanese.isl"
Name: "no"; MessagesFile: "compiler:Languages\Norwegian.isl"
Name: "pl"; MessagesFile: "compiler:Languages\Polish.isl"
Name: "pt"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "pt_BR"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "ru"; MessagesFile: "compiler:Languages\Russian.isl"
Name: "sl"; MessagesFile: "compiler:Languages\Slovenian.isl"
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"
Name: "tr"; MessagesFile: "compiler:Languages\Turkish.isl"
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
AppReadmeFile={#WebSite}help.php?place=installer

CloseApplications=yes
ShowLanguageDialog=auto
DefaultDirName={commonpf}\{#ProgName}
DefaultGroupName={#ProgName}
AllowNoIcons=yes
LicenseFile=license.txt
ChangesAssociations=yes
WizardImageFile={#ResourceDir}installer-logo.bmp
WizardSmallImageFile={#ResourceDir}installer-small-logo.bmp
OutputDir={#OutDir}
OutputBaseFilename={#ProgName}_{#ProgShortVersion}_Setup
UninstallDisplayIcon={app}\{#ProgExeName}
SetupIconFile={#ResourceDir}mainicon.ico
ArchitecturesInstallIn64BitMode=x64
UsePreviousAppDir=yes
DirExistsWarning=auto
PrivilegesRequired=none

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Options:"; MinVersion: 4,4
Name: "associatesqlfiles"; Description: "Associate .&SQL files with {#ProgName}"; GroupDescription: "Options:";
Name: "activate_updatechecks"; Description: "Automatically check {#WebSite} for updates"; GroupDescription: "Options:";
Name: "activate_statistics"; Description: "Automatically report client and server versions on {#WebSite}"; GroupDescription: "Options:";

[InstallDelete]
Type: files; Name: "{app}\heidisql32.exe"
Type: files; Name: "{app}\ssleay32.dll"
Type: files; Name: "{app}\libeay32.dll"
Type: files; Name: "{app}\libpq.dll"

[Files]
Source: "{#ProgNameLower}64.exe"; DestDir: "{app}"; DestName: "{#ProgExeName}"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "{#ProgNameLower}32.exe"; DestDir: "{app}"; DestName: "{#ProgExeName}"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "gpl.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "plugins64\*.dll"; DestDir: "{app}\plugins"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "plugins32\*.dll"; DestDir: "{app}\plugins"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "Snippets\*.sql"; DestDir: "{userdocs}\{#ProgName}\Snippets";
Source: "plink-64.exe"; DestDir: "{app}"; DestName: "plink.exe"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "plink-32.exe"; DestDir: "{app}"; DestName: "plink.exe"; Check: not Is64BitInstallMode; Flags: ignoreversion
; MySQL + MariaDB:
Source: "libmariadb64.dll"; DestDir: "{app}"; DestName: "libmariadb.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libmariadb32.dll"; DestDir: "{app}"; DestName: "libmariadb.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "libmysql64.dll"; DestDir: "{app}"; DestName: "libmysql.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libmysql32.dll"; DestDir: "{app}"; DestName: "libmysql.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "libmysql-6.1-64.dll"; DestDir: "{app}"; DestName: "libmysql-6.1.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libmysql-6.1-32.dll"; DestDir: "{app}"; DestName: "libmysql-6.1.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
; PostgreSQL:
Source: "libpq-10-64.dll"; DestDir: "{app}"; DestName: "libpq-10.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libpq-10-32.dll"; DestDir: "{app}"; DestName: "libpq-10.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "libpq-12-64.dll"; DestDir: "{app}"; DestName: "libpq-12.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libintl-8-64.dll"; DestDir: "{app}"; DestName: "libintl-8.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libintl-8-32.dll"; DestDir: "{app}"; DestName: "libintl-8.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "libssl-1_1-x64.dll"; DestDir: "{app}"; DestName: "libssl-1_1-x64.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libssl-1_1-32.dll"; DestDir: "{app}"; DestName: "libssl-1_1.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "libcrypto-1_1-x64.dll"; DestDir: "{app}"; DestName: "libcrypto-1_1-x64.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libcrypto-1_1-32.dll"; DestDir: "{app}"; DestName: "libcrypto-1_1.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "libiconv-2-64.dll"; DestDir: "{app}"; DestName: "libiconv-2.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libiconv-2-32.dll"; DestDir: "{app}"; DestName: "libiconv-2.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
; SQLite:
Source: "sqlite3-64.dll"; DestDir: "{app}"; DestName: "sqlite3.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "sqlite3-32.dll"; DestDir: "{app}"; DestName: "sqlite3.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion


[Icons]
Name: "{group}\{#ProgName}"; Filename: "{app}\{#ProgExeName}"
Name: "{group}\General help"; Filename: "http://www.heidisql.com/help.php?place=startmenu"
Name: "{userdesktop}\{#ProgName}"; Filename: "{app}\{#ProgExeName}"; MinVersion: 4,4; Tasks: desktopicon

[Registry]
Root: HKCR; Subkey: ".sql"; ValueType: string; ValueName: ""; ValueData: "SQLScriptFile"; Flags: noerror uninsdeletevalue; Tasks: associatesqlfiles
Root: HKCR; Subkey: "SQLScriptFile"; ValueType: string; ValueName: ""; ValueData: "SQL-Script"; Flags: noerror uninsdeletekey; Tasks: associatesqlfiles
Root: HKCR; Subkey: "SQLScriptFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#ProgExeName},0"; Flags: noerror; Tasks: associatesqlfiles
Root: HKCR; Subkey: "SQLScriptFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#ProgExeName}"" ""%1"""; Flags: noerror; Tasks: associatesqlfiles
; Enable auto-updatechecks if this option was checked. Only save the value when it's checked, as the default in preferences is False (see const.inc)
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "Updatecheck"; ValueData: 1; Tasks: activate_updatechecks
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "DoUsageStatistics"; ValueData: 1; Tasks: activate_statistics

[Run]
Filename: "{app}\{#ProgExeName}"; Description: "Launch {#ProgName}"; Flags: nowait postinstall skipifsilent

[Code]
var
  txt: TNewStaticText;
  btn: TButton;

procedure DonateClick(Sender: TObject);
var
  ErrorCode: Integer;
begin
  ShellExec('open', '{#WebSite}donatebutton.php?place=installer', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

procedure InitializeWizard();
begin
  txt := TNewStaticText.Create(WizardForm);
  txt.Parent := WizardForm.FinishedPage;
  txt.Caption := '{#ProgName} is free software for database workers.'+#13#10+'Keep it alive with a donation:';
  txt.Left := WizardForm.FinishedLabel.Left;
  txt.Top := WizardForm.FinishedLabel.Top + WizardForm.FinishedLabel.Height + 80;

  btn := TButton.Create(WizardForm);
  btn.Parent := WizardForm.FinishedPage;
  btn.Left := txt.Left;
  btn.Top := txt.Top + txt.Height + 10;
  btn.Width := WizardForm.Width div 2;
  btn.Height := WizardForm.CancelButton.Height + 10;
  btn.Caption := 'Donate via Paypal';
  btn.OnClick := @DonateClick;
end;


procedure CurPageChanged(CurPageID: Integer);
begin
  if CurPageID = wpFinished then
    WizardForm.ActiveControl := btn;
end;
