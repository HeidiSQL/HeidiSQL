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
Name: "theme_windows"; Description: "Use default Windows theme"; GroupDescription: "Select theme:"; Flags: exclusive
Name: "theme_material"; Description: "Use dark Material theme"; GroupDescription: "Select theme:"; Flags: exclusive unchecked

[InstallDelete]
Type: files; Name: "{app}\heidisql32.exe"

[Files]
Source: "{#ProgNameLower}64.exe"; DestDir: "{app}"; DestName: "{#ProgExeName}"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "{#ProgNameLower}32.exe"; DestDir: "{app}"; DestName: "{#ProgExeName}"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "gpl.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "libmariadb64.dll"; DestDir: "{app}"; DestName: "libmariadb.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libmariadb32.dll"; DestDir: "{app}"; DestName: "libmariadb.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "libmysql64.dll"; DestDir: "{app}"; DestName: "libmysql.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libmysql32.dll"; DestDir: "{app}"; DestName: "libmysql.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "libmysql-6.1-64.dll"; DestDir: "{app}"; DestName: "libmysql-6.1.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libmysql-6.1-32.dll"; DestDir: "{app}"; DestName: "libmysql-6.1.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "libpq64.dll"; DestDir: "{app}"; DestName: "libpq.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libpq32.dll"; DestDir: "{app}"; DestName: "libpq.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "libintl-864.dll"; DestDir: "{app}"; DestName: "libintl-8.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libintl-832.dll"; DestDir: "{app}"; DestName: "libintl-8.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "ssleay32-64.dll"; DestDir: "{app}"; DestName: "ssleay32.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "ssleay32-32.dll"; DestDir: "{app}"; DestName: "ssleay32.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "libeay32-64.dll"; DestDir: "{app}"; DestName: "libeay32.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libeay32-32.dll"; DestDir: "{app}"; DestName: "libeay32.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "libiconv-264.dll"; DestDir: "{app}"; DestName: "libiconv-2.dll"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "libiconv-232.dll"; DestDir: "{app}"; DestName: "libiconv-2.dll"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "plugins64\*.dll"; DestDir: "{app}\plugins"; Check: Is64BitInstallMode; Flags: ignoreversion
Source: "plugins32\*.dll"; DestDir: "{app}\plugins"; Check: not Is64BitInstallMode; Flags: ignoreversion
Source: "Snippets\*.sql"; DestDir: "{userdocs}\{#ProgName}\Snippets";

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

; Store theme selection: "Windows"
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: string; ValueName: "Theme"; ValueData: "Windows"; Tasks: theme_windows
; SQL colors from "Light" preset
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Comment Foreground"; ValueData: "8421504"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr ConditionalComment Foreground"; ValueData: "8421504"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr DataType Foreground"; ValueData: "128"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr DelimitedIdentifier Foreground"; ValueData: "32896"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Function Foreground"; ValueData: "8388608"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Identifier Foreground"; ValueData: "32896"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Key Foreground"; ValueData: "16711680"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Number Foreground"; ValueData: "8388736"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr String Foreground"; ValueData: "32768"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Symbol Foreground"; ValueData: "16711680"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr TableName Foreground"; ValueData: "16711935"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Variable Foreground"; ValueData: "8388736"; Tasks: theme_windows
; Data type colors from "Light" preset
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Binary"; ValueData: "8388736"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Datetime"; ValueData: "128"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Numeric"; ValueData: "16711680"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Other"; ValueData: "32896"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Real"; ValueData: "16711752"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Spatial"; ValueData: "8421376"; Tasks: theme_windows
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Text"; ValueData: "32768"; Tasks: theme_windows

; Store theme selection: "Material"
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: string; ValueName: "Theme"; ValueData: "Material"; Tasks: theme_material
; SQL colors from "Material" preset
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Comment Foreground"; ValueData: "8023636"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr ConditionalComment Foreground"; ValueData: "12108397"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr DataType Foreground"; ValueData: "15372999"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr DelimitedIdentifier Foreground"; ValueData: "16757122"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Function Foreground"; ValueData: "14929603"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Identifier Foreground"; ValueData: "16757122"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Key Foreground"; ValueData: "14929603"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Number Foreground"; ValueData: "7361535"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr String Foreground"; ValueData: "8906947"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Symbol Foreground"; ValueData: "12897152"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr TableName Foreground"; ValueData: "6911735"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "SQL Attr Variable Foreground"; ValueData: "7064575"; Tasks: theme_material
; Data type colors from "Dark" preset
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Binary"; ValueData: "13203071"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Datetime"; ValueData: "7566281"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Numeric"; ValueData: "16750469"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Other"; ValueData: "7586241"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Real"; ValueData: "13663613"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Spatial"; ValueData: "13553267"; Tasks: theme_material
Root: HKCU; Subkey: "Software\{#ProgName}"; ValueType: dword; ValueName: "FieldColor_Text"; ValueData: "7591283"; Tasks: theme_material

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
