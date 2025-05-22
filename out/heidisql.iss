; HeidiSQL setup script for Innosetup

; Set commonly used constants for preprocessor
#define ProgName "HeidiSQL"
#define ProgNameLower LowerCase(ProgName)
#define ProgExeName ProgNameLower + ".exe"
#define WebSite "https://www." + ProgNameLower + ".com/"
#define OutDir "."
#define ResourceDir OutDir + "\..\res\"
#define SnippetsDir "{autodocs}\" + ProgName + "\Snippets"
; Some effort to get the major.minor program version: "11.23"
#define ProgVerMajor
#define ProgVerMinor
#define ProgVerRevision
#define ProgVerBuild
#define ProgVersion GetVersionComponents(AddBackslash(SourcePath) + ProgNameLower + "64.exe", ProgVerMajor, ProgVerMinor, ProgVerRevision, ProgVerBuild)
#define ProgShortVersion Str(ProgVerMajor) + "." + Str(ProgVerMinor)

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "hy"; MessagesFile: "compiler:Languages\Armenian.isl"
Name: "bg"; MessagesFile: "compiler:Languages\Bulgarian.isl"
Name: "ca"; MessagesFile: "compiler:Languages\Catalan.isl"
Name: "co"; MessagesFile: "compiler:Languages\Corsican.isl"
Name: "cs"; MessagesFile: "compiler:Languages\Czech.isl"
Name: "da"; MessagesFile: "compiler:Languages\Danish.isl"
Name: "nl"; MessagesFile: "compiler:Languages\Dutch.isl"
Name: "fi"; MessagesFile: "compiler:Languages\Finnish.isl"
Name: "fr"; MessagesFile: "compiler:Languages\French.isl"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"
Name: "he"; MessagesFile: "compiler:Languages\Hebrew.isl"
Name: "hu"; MessagesFile: "compiler:Languages\Hungarian.isl"
Name: "is"; MessagesFile: "compiler:Languages\Icelandic.isl"
Name: "it"; MessagesFile: "compiler:Languages\Italian.isl"
Name: "ja"; MessagesFile: "compiler:Languages\Japanese.isl"
Name: "kr"; MessagesFile: "compiler:Languages\Korean.isl"
Name: "no"; MessagesFile: "compiler:Languages\Norwegian.isl"
Name: "pl"; MessagesFile: "compiler:Languages\Polish.isl"
Name: "pt"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "pt_BR"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "ru"; MessagesFile: "compiler:Languages\Russian.isl"
Name: "sk"; MessagesFile: "compiler:Languages\Slovak.isl"
Name: "sl"; MessagesFile: "compiler:Languages\Slovenian.isl"
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"
Name: "se"; MessagesFile: "compiler:Languages\Swedish.isl"
Name: "ta"; MessagesFile: "compiler:Languages\Tamil.isl"
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
AppContact=anse@heidisql.com
AppReadmeFile={#WebSite}help.php?place=installer
Compression=lzma2/ultra64
SolidCompression=yes
CloseApplications=yes
ShowLanguageDialog=auto
DefaultDirName={autopf}\{#ProgName}
DefaultGroupName={#ProgName}
AllowNoIcons=yes
LicenseFile=license.txt
ChangesAssociations=yes
WizardStyle=modern
WizardImageFile={#ResourceDir}installer-logo.bmp
WizardSmallImageFile={#ResourceDir}installer-small-logo.bmp
OutputDir={#OutDir}
OutputBaseFilename={#ProgName}_{#ProgShortVersion}_Setup
UninstallDisplayIcon={app}\{#ProgExeName}
SetupIconFile={#ResourceDir}mainicon.ico
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
UsePreviousAppDir=yes
DirExistsWarning=auto
PrivilegesRequired=admin
PrivilegesRequiredOverridesAllowed=commandline dialog
;SignedUninstaller=yes
;SignTool=signtool $f

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Local options:"; MinVersion: 4,4
Name: "install_snippets"; Description: "Create example SQL snippet files in {#SnippetsDir}"; GroupDescription: "Local options:";
Name: "associatesqlfiles"; Description: "Associate .&SQL files with {#ProgName}"; GroupDescription: "Local options:";
Name: "activate_updatechecks"; Description: "Automatically check {#WebSite} for updates"; GroupDescription: "Telemetry:";
Name: "activate_statistics"; Description: "Automatically report client and server versions on {#WebSite}"; GroupDescription: "Telemetry:";

[Files]
Source: "{#ProgNameLower}64.exe"; DestDir: "{app}"; DestName: "{#ProgExeName}"; Flags: ignoreversion
Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "gpl.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "plugins64\*.dll"; DestDir: "{app}\plugins"; Flags: ignoreversion
Source: "Snippets\*.sql"; DestDir: "{#SnippetsDir}"; Tasks: install_snippets
Source: "plink-64.exe"; DestDir: "{app}"; DestName: "plink.exe"; Flags: ignoreversion
; OpenSSL libraries, used by Indy HTTP:
Source: "libeay32-64.dll"; DestDir: "{app}"; DestName: "libeay32.dll"; Flags: ignoreversion
Source: "ssleay32-64.dll"; DestDir: "{app}"; DestName: "ssleay32.dll"; Flags: ignoreversion
; MySQL + MariaDB:
Source: "libmariadb64.dll"; DestDir: "{app}"; DestName: "libmariadb.dll"; Flags: ignoreversion
Source: "libmysql64.dll"; DestDir: "{app}"; DestName: "libmysql.dll"; Flags: ignoreversion
Source: "libmysql-6.1-64.dll"; DestDir: "{app}"; DestName: "libmysql-6.1.dll"; Flags: ignoreversion
Source: "libmysql-8.4.0-64.dll"; DestDir: "{app}"; DestName: "libmysql-8.4.0.dll"; Flags: ignoreversion
; PostgreSQL:
Source: "libpq-10-64.dll"; DestDir: "{app}"; DestName: "libpq-10.dll"; Flags: ignoreversion
Source: "libpq-12-64.dll"; DestDir: "{app}"; DestName: "libpq-12.dll"; Flags: ignoreversion
Source: "libpq-15-64.dll"; DestDir: "{app}"; DestName: "libpq-15.dll"; Flags: ignoreversion
Source: "libpq-17-64.dll"; DestDir: "{app}"; DestName: "libpq-17.dll"; Flags: ignoreversion
Source: "libintl-8-64.dll"; DestDir: "{app}"; DestName: "libintl-8.dll"; Flags: ignoreversion
Source: "libintl-9-64.dll"; DestDir: "{app}"; DestName: "libintl-9.dll"; Flags: ignoreversion
Source: "libssl-1_1-x64.dll"; DestDir: "{app}"; DestName: "libssl-1_1-x64.dll"; Flags: ignoreversion
Source: "libssl-3-x64.dll"; DestDir: "{app}"; DestName: "libssl-3-x64.dll"; Flags: ignoreversion
Source: "libcrypto-1_1-x64.dll"; DestDir: "{app}"; DestName: "libcrypto-1_1-x64.dll"; Flags: ignoreversion
Source: "libcrypto-3-x64.dll"; DestDir: "{app}"; DestName: "libcrypto-3-x64.dll"; Flags: ignoreversion
Source: "LICENSE-openssl"; DestDir: "{app}"; Flags: ignoreversion
Source: "libiconv-2-64.dll"; DestDir: "{app}"; DestName: "libiconv-2.dll"; Flags: ignoreversion
Source: "libwinpthread-1-64.dll"; DestDir: "{app}"; DestName: "libwinpthread-1.dll"; Flags: ignoreversion
; SQLite:
Source: "sqlite3-64.dll"; DestDir: "{app}"; DestName: "sqlite3.dll"; Flags: ignoreversion
Source: "sqlite3mc-64.dll"; DestDir: "{app}"; DestName: "sqlite3mc.dll"; Flags: ignoreversion
; Interbase/Firebird:
Source: "ibclient64-14.1.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "fbclient-4.0-64.dll"; DestDir: "{app}"; DestName: "fbclient-4.0.dll"; Flags: ignoreversion
; SQL function definitions
Source: "functions-*.ini"; DestDir: "{app}"; Flags: ignoreversion


[Icons]
Name: "{group}\{#ProgName}"; Filename: "{app}\{#ProgExeName}"
Name: "{group}\Website"; Filename: "{#Website}"
Name: "{group}\General help"; Filename: "{#Website}help.php?place=startmenu"
Name: "{autodesktop}\{#ProgName}"; Filename: "{app}\{#ProgExeName}"; MinVersion: 4,4; Tasks: desktopicon

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
