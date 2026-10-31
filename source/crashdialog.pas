unit crashdialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

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

