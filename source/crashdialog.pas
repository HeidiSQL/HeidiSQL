unit crashdialog;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LMessages;

type

  { TfrmCrashDialog }

  TfrmCrashDialog = class(TForm)
    btnIgnore: TButton;
    btnAbort: TButton;
    btnCopy: TButton;
    lblHeader: TLabel;
    memoDetails: TMemo;
    procedure btnCopyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    procedure SetDetails(AValue: String);
  end;


implementation

{$R *.lfm}

{ TfrmCrashDialog }

procedure TfrmCrashDialog.btnCopyClick(Sender: TObject);
begin
  memoDetails.CopyToClipboard;
  btnCopy.Caption := btnCopy.Caption + ' ' + #10003;
  // enable timer which resets the button caption?
end;

procedure TfrmCrashDialog.FormShow(Sender: TObject);
begin
  btnCopy.SetFocus;
end;

procedure TfrmCrashDialog.SetDetails(AValue: String);
begin
  memoDetails.Text := AValue;
end;

end.

