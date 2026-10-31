unit crashdialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons;

type

  { TfrmCrashDialog }

  TfrmCrashDialog = class(TForm)
    btnIgnore: TBitBtn;
    btnAbort: TBitBtn;
    btnCopy: TBitBtn;
    btnReport: TBitBtn;
    lblHeader: TLabel;
    memoDetails: TMemo;
    tmrReport: TTimer;
    procedure btnCopyClick(Sender: TObject);
    procedure btnReportClick(Sender: TObject);
    procedure tmrReportTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    procedure SetDetails(AValue: String);
  end;


implementation

uses
  apphelpers;

{$R *.lfm}

const
  // Monospace font candidates; the first one installed on the target system wins
  MONO_FONTS: array[0..3] of string = ('Consolas', 'Menlo', 'Courier New', 'Monospace');

{ TfrmCrashDialog }

procedure TfrmCrashDialog.btnCopyClick(Sender: TObject);
begin
  memoDetails.CopyToClipboard;
  btnCopy.Caption := btnCopy.Caption + ' ' + '✓';
  // enable timer which resets the button caption?
end;

procedure TfrmCrashDialog.btnReportClick(Sender: TObject);
var
  HttpReq: THttpDownload;
  Params, Response: TStringList;
  OldCaption: string;
begin
  OldCaption := btnReport.Caption;
  btnReport.Caption := OldCaption + '...';
  btnReport.Enabled := False;
  tmrReport.Enabled := True; // re-enable the button after 30s as flood protection
  Application.ProcessMessages;
  HttpReq := THttpDownload.Create(Self);
  Params := TStringList.Create;
  Response := TStringList.Create;
  try
    HttpReq.TimeOut := 30;
    Params.AddPair('report', memoDetails.Text);
    HttpReq.FormPost(APPDOMAIN + 'bugreport.php', Params, Response);
    if (HttpReq.ResponseStatusCode >= 200) and (HttpReq.ResponseStatusCode < 300) then
      MessageDialog(_('Report sent'), f_('The bug report was sent to %s. Thank you!', [APPDOMAIN]), mtInformation, [mbOK])
    else
      ErrorDialog(_('Sending the report failed'), f_('Got HTTP status %d from %s', [HttpReq.ResponseStatusCode, APPDOMAIN]) + LineEnding + Response.Text);
  finally
    Response.Free;
    Params.Free;
    HttpReq.Free;
    btnReport.Caption := OldCaption;
    Application.ProcessMessages;
  end;
end;

procedure TfrmCrashDialog.tmrReportTimer(Sender: TObject);
begin
  tmrReport.Enabled := False;
  btnReport.Enabled := True;
end;

procedure TfrmCrashDialog.FormShow(Sender: TObject);
var
  I: Integer;
begin
  // Monospace font for aligned addresses and callstack lines
  for I := Low(MONO_FONTS) to High(MONO_FONTS) do
    if Screen.Fonts.IndexOf(MONO_FONTS[I]) >= 0 then
    begin
      memoDetails.Font.Name := MONO_FONTS[I];
      Break;
    end;
  memoDetails.Font.Pitch := fpFixed;
  btnCopy.SetFocus;
end;

procedure TfrmCrashDialog.SetDetails(AValue: String);
begin
  memoDetails.Text := AValue;
end;


end.

