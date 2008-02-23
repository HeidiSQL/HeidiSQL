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
  ScriptFile: Textfile;
  ScriptFilename, ScriptContent: String;
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
  if not FileExists(Download.Filename) then
    Raise Exception.Create('Downloaded file not found: '+Download.Filename);

  if FilePath = Application.ExeName then begin
    // The Visual Basic Script code which kills this exe and moves the
    // downloaded file to the application directory.
    // This file moving can fail due to several reasons. Especially in Vista
    // where users are normally not admins, they'll get a "Permission denied".
    // However, the script does several write attempts and quits with a clear
    // error message if it couldn't move the file.
    // TODO: move this code to a seperate file for easier debugging
    ScriptContent := ''' This is a temporary script which shall update your ' + APPNAME + CRLF +
      ''' with a nightly build.' + CRLF +
      CRLF +
      'ExeName = "'+ExtractFilename(FilePath)+'"' + CRLF +
      'DownloadFileName = "'+Download.Filename+'"' + CRLF +
      'TargetFileName = "'+FilePath+'"' + CRLF +
      CRLF +
      'WScript.Echo "Terminating """&ExeName&""" ..."' + CRLF +
      'Set Shell = WScript.CreateObject("WScript.Shell")' + CRLF +
      'Shell.Run("taskkill /im """&ExeName&""" /f")' + CRLF +
      CRLF +
      'Set FileSystem = CreateObject("Scripting.FileSystemObject")' + CRLF +
      'Set DownloadFile = FileSystem.GetFile(DownloadFileName)' + CRLF +
      'Set TargetFile = FileSystem.GetFile(TargetFileName)' + CRLF +
      'On Error Resume Next' + CRLF +
      'MaxAttempts = 10' + CRLF +
      'for x = 1 to MaxAttempts' + CRLF +
      '  WScript.Echo "Deleting "&ExeName&" (attempt "&x&" of "&MaxAttempts&") ..."' + CRLF +
      '  TargetFile.Delete' + CRLF +
      '  If Err.Number = 0 Then' + CRLF +
      '    Err.Clear' + CRLF +
      '    Exit For' + CRLF +
      '  End If' + CRLF +
      '  Err.Clear' + CRLF +
      '  WScript.Sleep(2000)' + CRLF +
      'Next' + CRLF +
      'If Err.Number <> 0 Then' + CRLF +
      '  WScript.Echo "Error: Cannot delete file "&TargetFileName' + CRLF +
      '  WScript.Sleep(10000)' + CRLF +
      '  Wscript.Quit' + CRLF +
      'End If' + CRLF +
      'Err.Clear' + CRLF +
      CRLF +
      'WScript.Echo "Installing new build ..."' + CRLF +
      'DownloadFile.Move TargetFileName' + CRLF +
      CRLF +
      'WScript.Echo "Restarting ..."' + CRLF +
      'Shell.Run(""""&TargetFileName&"""")';
    // Write script file to disk
    ScriptFilename := GetTempDir + APPNAME + '_Update.vbs';
    AssignFile(ScriptFile, ScriptFilename);
    Rewrite(ScriptFile);
    Write(Scriptfile, ScriptContent);
    CloseFile(ScriptFile);
    // Calling the script will now terminate the running exe...
    WinExec(PAnsiChar('cscript.exe "'+ScriptFilename+'"'), 1);
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
