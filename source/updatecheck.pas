unit updatecheck;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  Dialogs, StdCtrls, ExtActns, IniFiles, Controls, Graphics;

type
  TfrmUpdateCheck = class(TForm)
    btnCancel: TButton;
    groupBuild: TGroupBox;
    btnBuild: TButton;
    groupRelease: TGroupBox;
    btnRelease: TButton;
    lblStatus: TLabel;
    memoRelease: TMemo;
    memoBuild: TMemo;
    procedure btnBuildClick(Sender: TObject);
    procedure btnReleaseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    CheckfileDownload : TDownLoadURL;
    ReleaseURL, BuildURL : String;
    procedure Status(txt: String);
    procedure ReadCheckFile;
  public
    { Public declarations }
  end;

implementation

uses helpers, main;

{$R *.dfm}

{$I const.inc}



{**
  Update status text
}
procedure TfrmUpdateCheck.Status(txt: String);
begin
  lblStatus.Caption := txt;
  Repaint;
end;


{**
  Download check file
}
procedure TfrmUpdateCheck.FormShow(Sender: TObject);
var
  TempPath: array[0..MAX_PATH] of Char;
begin
  Status('Initiating ... ');
  Caption := 'Check for '+APPNAME+' updates ...';

  // Init GUI controls
  btnRelease.Enabled := False;
  btnBuild.Enabled := False;
  btnCancel.Enabled := False;
  memoRelease.Clear;
  memoBuild.Clear;

  // Detect temp directory
  GetTempPath(MAX_PATH, @TempPath);

  // Prepare download
  CheckfileDownload := TDownLoadURL.Create(Self);
  CheckfileDownload.URL := APPDOMAIN + 'updatecheck.php';
  CheckfileDownload.Filename := StrPas(TempPath) +  APPNAME + '_updatecheck.ini';

  // Download the check file
  Screen.Cursor := crHourglass;
  Status('Downloading check file ...');
  try
    CheckfileDownload.ExecuteTarget(nil);
    ReadCheckFile;
    DeleteFile(CheckfileDownload.Filename);
  finally
    FreeAndNil(CheckfileDownload);
    Screen.Cursor := crDefault;
    btnCancel.Enabled := True;
  end;
end;


{**
  Parse check file for updated version + release
}
procedure TfrmUpdateCheck.ReadCheckFile;
var
  Ini : TIniFile;
  ReleaseVersion : String;
  ReleaseRevision, BuildRevision : Integer;
  Note : String;
  Compiled : TDateTime;
const
  INISECT_RELEASE = 'Release';
  INISECT_BUILD = 'Build';
begin
  Status('Reading check file ...');

  // Read [Release] section of check file
  Ini := TIniFile.Create(CheckfileDownload.Filename);
  if Ini.SectionExists(INISECT_RELEASE) then begin
    ReleaseVersion := Ini.ReadString(INISECT_RELEASE, 'Version', 'unknown');
    ReleaseRevision := Ini.ReadInteger(INISECT_RELEASE, 'Revision', 0);
    ReleaseURL := Ini.ReadString(INISECT_RELEASE, 'URL', '');
    memoRelease.Lines.Add( 'Version ' + ReleaseVersion + ' (yours: '+AppVersion+')' );
    memoRelease.Lines.Add( 'Released: ' + Ini.ReadString(INISECT_RELEASE, 'Date', '') );
    Note := Ini.ReadString(INISECT_RELEASE, 'Note', '');
    if Note <> '' then
      memoRelease.Lines.Add( 'Note: ' + Note );
    btnRelease.Caption := 'Download version ' + ReleaseVersion;
    // Enable the download button if the current version is outdated
    groupRelease.Enabled := ReleaseRevision > StrToIntDef(AppRevision, 0);
    btnRelease.Enabled := groupRelease.Enabled;
    memoRelease.Enabled := groupBuild.Enabled;
    if not memoRelease.Enabled then
      memoRelease.Font.Color := cl3DDkShadow
    else
      memoRelease.Font.Color := clWindowText;
  end;

  // Read [Build] section of check file
  if Ini.SectionExists(INISECT_BUILD) then begin
    BuildRevision := Ini.ReadInteger(INISECT_BUILD, 'Revision', 0);
    BuildURL := Ini.ReadString(INISECT_BUILD, 'URL', '');
    memoBuild.Lines.Add( 'Revision ' + IntToStr(BuildRevision) + ' (yours: '+AppRevision+')' );
    FileAge(ParamStr(0), Compiled);
    memoBuild.Lines.Add( 'Compiled: ' + Ini.ReadString(INISECT_BUILD, 'Date', '') + ' (yours: '+DateToStr(Compiled)+')' );
    Note := Ini.ReadString(INISECT_BUILD, 'Note', '');
    if Note <> '' then
      memoBuild.Lines.Add( 'Note: ' + Note );
    btnBuild.Caption := 'Download build ' + IntToStr(BuildRevision);
    // A new release should have priority over a new nightly build.
    // So the user should not be able to download a newer build here
    // before having installed the new release.
    groupBuild.Enabled := (BuildRevision > StrToIntDef(AppRevision, 0)) and (not btnRelease.Enabled);
    btnBuild.Enabled := groupBuild.Enabled;
    memoBuild.Enabled := groupBuild.Enabled;
    if not memoBuild.Enabled then
      memoBuild.Font.Color := cl3DDkShadow
    else
      memoBuild.Font.Color := clWindowText;
  end;

  // Clear status label
  Status('');
end;


{**
  Download release installer via web browser
}
procedure TfrmUpdateCheck.btnReleaseClick(Sender: TObject);
begin
  ShellExec(ReleaseURL);
end;


{**
  Download latest build via web browser
  TODO: internally replace heidisql.exe:
    1. create a batch file which replaces heidisql.exe
    2. quit Heidi
    3. start the batch
    4. start Heidi
}
procedure TfrmUpdateCheck.btnBuildClick(Sender: TObject);
begin
  ShellExec(BuildURL);
end;

end.
