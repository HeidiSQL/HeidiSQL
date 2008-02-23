unit updatedownload;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtActns, Helpers;

type
  TfrmUpdateDownload = class(TForm)
    progressDownload: TProgressBar;
    lblStatus: TLabel;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
    Download: TDownLoadURL;
    DoCancel: Boolean;
    FilePath: String;
    procedure DoDownload;
    procedure URLOnDownloadProgress(Sender: TDownLoadURL; Progress, ProgressMax: Cardinal;
      StatusCode: TURLDownloadStatus; StatusText: String; var Cancel: Boolean);
  public
    { Public declarations }
  end;

function DoUpdateDownload(AOwner: TComponent; URL, FilePath: String): Boolean;

implementation

{$R *.dfm}


{**
  Initiate the download + update process
}
function DoUpdateDownload(AOwner: TComponent; URL, FilePath: String): Boolean;
var
  frm: TfrmUpdateDownload;
begin
  Result := False;
  frm := TfrmUpdateDownload.Create(AOwner);
  frm.Download := TDownLoadURL.Create(frm);
  frm.Download.URL := URL;
  frm.FilePath := FilePath;
  frm.DoDownload;
end;


{**
  Cancel clicked
}
procedure TfrmUpdateDownload.btnCancelClick(Sender: TObject);
begin
  DoCancel := True;
end;


{**
  Start the download + update process
}
procedure TfrmUpdateDownload.DoDownload;
var
  BatchFile: Textfile;
  BatchFilename, ExeName: String;
begin
  Show;

  // Save the file in a temp directory
  Download.Filename := GetTempDir + ExtractFileName(Filepath);
  Download.OnDownloadProgress := URLOnDownloadProgress;

  // Delete probably previously downloaded file
  if FileExists(Download.Filename) then
    DeleteFile(Download.Filename);

  // Do the download
  Download.ExecuteTarget(nil);

  // Check if downloaded file exists
  if not FileExists(Download.Filename) then begin
    Raise Exception.Create('Downloaded file not found: '+Download.Filename);
  end;

  if FilePath = Application.ExeName then begin
    ExeName := ExtractFilename(FilePath);
    BatchFilename := ExtractFilepath(Application.ExeName) + 'Update_' + ExeName + '.cmd';
    AssignFile(BatchFile, BatchFilename);
    Rewrite(BatchFile);
    WriteLn(BatchFile, 'rem Killing task '+ExeName);
    WriteLn(BatchFile, 'taskkill /im "'+ExeName+'" /f');
    // TODO: find some sleep command, as we sometimes get "access denied"
    // while trying to overwrite the just terminated exe. Seems like taskkill
    // doesn't close all handles itself
    WriteLn(BatchFile, 'rem Moving downloaded file to '+ExtractFilepath(FilePath));
    WriteLn(BatchFile, 'move /Y "'+Download.Filename+'" "'+FilePath+'"');
    WriteLn(BatchFile, 'rem Restarting '+APPNAME);
    WriteLn(BatchFile, 'start /D"'+ExtractFilepath(FilePath)+'" '+ExeName);
    WriteLn(Batchfile);
    CloseFile(BatchFile);
    // Calling the batchfile will now terminate the running exe...
    WinExec(PAnsiChar(BatchFilename), 1);
  end else begin
    // We're not replacing the running exe, so just move file to destination
    MoveFile(PChar(Download.Filename), PChar(FilePath));
  end;

  // Close form
  ModalResult := mrOK;
end;


{**
  Download progress event
}
procedure TfrmUpdateDownload.URLOnDownloadProgress(Sender: TDownLoadURL; Progress, ProgressMax: Cardinal;
  StatusCode: TURLDownloadStatus; StatusText: String; var Cancel: Boolean);
begin
  progressDownload.Max := ProgressMax;
  progressDownload.Position := Progress;
  lblStatus.Caption := StatusText;
  // Notification if cancel button was pressed
  Cancel := DoCancel;
  Application.ProcessMessages;
end;




end.
