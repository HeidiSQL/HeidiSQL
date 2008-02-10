unit updatecheck;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  Dialogs, StdCtrls, ExtActns, IniFiles, Controls, ExtCtrls, GIFImg;

type
  TfrmUpdateCheck = class(TForm)
    btnRelease: TButton;
    btnBuild: TButton;
    lblStatus: TLabel;
    btnCancel: TButton;
    Image1: TImage;
    procedure btnBuildClick(Sender: TObject);
    procedure btnReleaseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    d : TDownLoadURL;
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

  // Detect temp directory
  GetTempPath(MAX_PATH, @TempPath);

  // Initially hide download buttons
  btnRelease.Enabled := False;
  btnBuild.Enabled := False;
  btnCancel.Enabled := False;

  d := TDownLoadURL.Create(Self);
  d.URL := APPDOMAIN + 'updatecheck.php';
  // Todo: find a better temp directory
  d.Filename := StrPas(TempPath) +  APPNAME + '_updatecheck.ini';
  Screen.Cursor := crHourglass;
  Status('Downloading check file '+d.URL+' ... ');
  try
    // Download the check file
    d.ExecuteTarget(nil);
    ReadCheckFile;
    DeleteFile(d.Filename);
  finally
    FreeAndNil(d);
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
  ReleaseVersion, BuildRevision, statustxt : String;
const
  INISECT_RELEASE = 'Release';
  INISECT_BUILD = 'Build';
begin
  Status('Reading check file ...');
  statustxt := 'No update available.';

  // Read [Release] section of check file
  Ini := TIniFile.Create(d.Filename);
  if Ini.SectionExists(INISECT_RELEASE) then begin
    ReleaseVersion := Ini.ReadString(INISECT_RELEASE, 'Version', 'unknown');
    ReleaseURL := Ini.ReadString(INISECT_RELEASE, 'URL', '');
    statustxt := 'Release available: Version ' + ReleaseVersion + ' (yours: '+AppVersion+')' + CRLF;
    btnRelease.Caption := 'Download version ' + ReleaseVersion;
    // Enable the download button if the current version is outdated
    btnRelease.Enabled := ReleaseVersion <> AppVersion;
  end;

  // Read [Build] section of check file
  if Ini.SectionExists(INISECT_BUILD) then begin
    BuildRevision := Ini.ReadString(INISECT_BUILD, 'Revision', 'unknown');
    BuildURL := Ini.ReadString(INISECT_BUILD, 'URL', '');
    statustxt := statustxt + 'Build available: Revision ' + BuildRevision + ' (yours: '+AppRevision+')' + CRLF;
    btnBuild.Caption := 'Download build ' + BuildRevision;
    // A new release should have priority over a new nightly build.
    // So the user should not be able to download a newer build here
    // before having installed the new release.
    btnBuild.Enabled := (BuildRevision <> AppRevision) and (not btnRelease.Enabled);
  end;

  // Display summary
  Status(statustxt);
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
  ShellExec(ReleaseURL);
end;

end.
