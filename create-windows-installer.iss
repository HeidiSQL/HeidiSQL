; HeidiSQL setup script for Innosetup

; Set commonly used constants for preprocessor
#define ProgName "HeidiSQL"
#define ProgNameLower LowerCase(ProgName)
#define ProgExeName ProgNameLower + ".exe"
#define WebSite "https://www." + ProgNameLower + ".com/"
#define OutDir ".\out\"
#define ResourceDir ".\res\"
#define SnippetsDir "{autodocs}\" + ProgName + "\Snippets"
; Some effort to get the major.minor program version: "11.23"
#define ProgVerMajor
#define ProgVerMinor
#define ProgVerRevision
#define ProgVerBuild
#define ProgVersion GetVersionComponents(OutDir + ProgNameLower + ".exe", ProgVerMajor, ProgVerMinor, ProgVerRevision, ProgVerBuild)
#define ProgVersionStr Str(ProgVerMajor) + "." + Str(ProgVerMinor) + "." + Str(ProgVerRevision) + "." + Str(ProgVerBuild)

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
AppVersion={#ProgVersionStr}
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
OutputBaseFilename={#ProgName}_{#ProgVersionStr}_Setup
UninstallDisplayIcon={app}\{#ProgExeName}
SetupIconFile={#ResourceDir}heidisql.ico
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
Name: "install_snippets"; Description: "Create example SQL snippet files in {#SnippetsDir}"; GroupDescription: "Local options:"; Flags: unchecked
Name: "associatesqlfiles"; Description: "Associate .&SQL files with {#ProgName}"; GroupDescription: "Local options:";

[Files]
Source: "{#OutDir}{#ProgNameLower}.exe"; DestDir: "{app}"; DestName: "{#ProgExeName}"; Flags: ignoreversion
Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "LICENSE"; DestDir: "{app}"; Flags: ignoreversion
Source: "extra\dll\plugins\*.dll"; DestDir: "{app}\plugins"; Flags: ignoreversion
Source: "extra\Snippets\*.sql"; DestDir: "{#SnippetsDir}"; Tasks: install_snippets
Source: "extra\dll\plink-64.exe"; DestDir: "{app}"; DestName: "plink.exe"; Flags: ignoreversion
Source: "extra\dll\plink-0.81-64.exe"; DestDir: "{app}"; DestName: "plink-0.81.exe"; Flags: ignoreversion
; MySQL + MariaDB:
Source: "extra\dll\libmariadb.dll"; DestDir: "{app}"; DestName: "libmariadb.dll"; Flags: ignoreversion
Source: "extra\dll\libmysql.dll"; DestDir: "{app}"; DestName: "libmysql.dll"; Flags: ignoreversion
Source: "extra\dll\libmysql-6.1.dll"; DestDir: "{app}"; DestName: "libmysql-6.1.dll"; Flags: ignoreversion
Source: "extra\dll\libmysql-8.4.0.dll"; DestDir: "{app}"; DestName: "libmysql-8.4.0.dll"; Flags: ignoreversion
Source: "extra\dll\libmysql-9.4.0.dll"; DestDir: "{app}"; DestName: "libmysql-9.4.0.dll"; Flags: ignoreversion
; PostgreSQL:
Source: "extra\dll\libpq-15.dll"; DestDir: "{app}"; DestName: "libpq-15.dll"; Flags: ignoreversion
Source: "extra\dll\libpq-17.dll"; DestDir: "{app}"; DestName: "libpq-17.dll"; Flags: ignoreversion
Source: "extra\dll\libintl-9.dll"; DestDir: "{app}"; DestName: "libintl-9.dll"; Flags: ignoreversion
Source: "extra\dll\libssl-3-x64.dll"; DestDir: "{app}"; DestName: "libssl-3-x64.dll"; Flags: ignoreversion
Source: "extra\dll\libcrypto-3-x64.dll"; DestDir: "{app}"; DestName: "libcrypto-3-x64.dll"; Flags: ignoreversion
Source: "extra\dll\LICENSE-openssl"; DestDir: "{app}"; Flags: ignoreversion
Source: "extra\dll\libiconv-2.dll"; DestDir: "{app}"; DestName: "libiconv-2.dll"; Flags: ignoreversion
Source: "extra\dll\libwinpthread-1.dll"; DestDir: "{app}"; DestName: "libwinpthread-1.dll"; Flags: ignoreversion
; SQLite:
Source: "extra\dll\sqlite3.dll"; DestDir: "{app}"; DestName: "sqlite3.dll"; Flags: ignoreversion
Source: "extra\dll\sqlite3mc.dll"; DestDir: "{app}"; DestName: "sqlite3mc.dll"; Flags: ignoreversion
; Interbase/Firebird:
Source: "extra\dll\ibclient64-14.1.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "extra\dll\fbclient-4.0.dll"; DestDir: "{app}"; DestName: "fbclient-4.0.dll"; Flags: ignoreversion
; SQL function definitions
Source: "extra\ini\functions-*.ini"; DestDir: "{app}"; Flags: ignoreversion


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
