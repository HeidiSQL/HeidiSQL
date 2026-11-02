unit change_password;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Menus, Clipbrd, ComCtrls, extra_controls, Buttons, LCLIntf, LCLType;

type
  TfrmPasswordChange = class(TExtForm)
    lblHeading: TLabel;
    lblPassword: TLabel;
    lblRepeatPassword: TLabel;
    editPassword: TEdit;
    editRepeatPassword: TEdit;
    btnCancel: TButton;
    btnOK: TButton;
    lblStatus: TLabel;
    popupPassword: TPopupMenu;
    N6characters1: TMenuItem;
    N8characters1: TMenuItem;
    N10characters1: TMenuItem;
    N12characters1: TMenuItem;
    N30characters1: TMenuItem;
    menuDummy1: TMenuItem;
    menuDummy2: TMenuItem;
    menuDummy3: TMenuItem;
    menuDummy4: TMenuItem;
    menuDummy5: TMenuItem;
    btnCopyToClipboard: TSpeedButton;
    progressbarPasswordStrength: TProgressBar;
    procedure editPasswordChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure editPasswordKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure menuPasswordClick(Sender: TObject);
    procedure menuPasswordInsert(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure CheckPasswordStrength;
    procedure SetStrengthColor(State: TProgressBarState);
  public
    { Public-Deklarationen }
  end;

var
  frmPasswordChange: TfrmPasswordChange;

implementation

uses main, apphelpers;

{$R *.lfm}


procedure TfrmPasswordChange.FormShow(Sender: TObject);
begin
  // Manually trigger change event on password box
  editPassword.OnChange(Sender);
end;


procedure TfrmPasswordChange.menuPasswordClick(Sender: TObject);
var
  Parent, Item: TMenuItem;
  PasswordLen, i: Integer;
begin
  // Create menu items with random passwords
  Parent := Sender as TMenuItem;
  PasswordLen := MakeInt(Parent.Caption);
  for i:=0 to 19 do begin
    if Parent.Count > i then
      Item := Parent[i]
    else begin
      Item := TMenuItem.Create(Parent);
      Parent.Add(Item);
    end;
    Item.OnClick := menuPasswordInsert;
    Item.Caption := GeneratePassword(PasswordLen);
  end;
end;


procedure TfrmPasswordChange.menuPasswordInsert(Sender: TObject);
var
  Item: TMenuItem;
begin
  // Insert password from menu item
  Item := Sender as TMenuItem;
  editPassword.Text := Item.Caption;
  editRepeatPassword.Text := editPassword.Text;
end;


procedure TfrmPasswordChange.btnCopyToClipboardClick(Sender: TObject);
var
  OldImageIndex: Integer;
begin
  // Copy new password to clipboard
  Clipboard.TryAsText := editPassword.Text;
  OldImageIndex := btnCopyToClipboard.ImageIndex;
  btnCopyToClipboard.ImageIndex := 55;
  btnCopyToClipboard.Repaint;
  Sleep(500);
  btnCopyToClipboard.ImageIndex := OldImageIndex;
  AppBeep(MB_OK);
end;


procedure TfrmPasswordChange.editPasswordChange(Sender: TObject);
var
  PasswordsMatch: Boolean;
begin
  // User has entered something on one or both password fields
  btnOK.Enabled := False;
  btnCopyToClipboard.Enabled := False;
  progressbarPasswordStrength.Visible := False;
  if Sender = editPassword then
    editRepeatPassword.Modified := False;

  if editPassword.Text = '' then begin
    editPassword.PasswordChar := #0;
    lblStatus.Caption := _('Please change your password')
  end else begin
    editPassword.PasswordChar := '*';
    PasswordsMatch := editPassword.Text = editRepeatPassword.Text;
    if editRepeatPassword.Modified and (not PasswordsMatch) then
      lblStatus.Caption := _('Error: Passwords do not match!')
    else begin
      lblStatus.Caption := _('Password strength:');
      btnOK.Enabled := PasswordsMatch;
      btnCopyToClipboard.Enabled := True;
      progressbarPasswordStrength.Visible := True;
      CheckPasswordStrength;
    end;
  end;
end;


procedure TfrmPasswordChange.editPasswordKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  editPassword.OnChange(Sender);
end;


procedure TfrmPasswordChange.CheckPasswordStrength;
var
  i, Distance, LastOrd: Integer;
  p: String;
begin
  // Simple password-strength checker
  // Calculates the distance between all character codes, and give it a bonus for longer passwords
  p := editPassword.Text;
  Distance := 0;
  LastOrd := -1;
  for i:=1 to Length(p) do begin
    if LastOrd > -1 then
      Inc(Distance, Abs(LastOrd - Ord(p[i])));
    LastOrd := Ord(p[i]);
  end;
  Inc(Distance, Length(p)*10);
  progressbarPasswordStrength.Position := Round(progressbarPasswordStrength.Max / 500 * Distance);
  // Colorize by strength. The LCL TProgressBar has no State property (unlike VCL's
  // pbsError/pbsPaused/pbsNormal used in trunk), so use our helper.
  case progressbarPasswordStrength.Position of
    0..20: SetStrengthColor(pbsError);
    21..50: SetStrengthColor(pbsPaused);
    51..100: SetStrengthColor(pbsNormal);
  end;
end;


procedure TfrmPasswordChange.SetStrengthColor(State: TProgressBarState);
{$IFDEF WINDOWS}
const
  // Win32 common control message (Vista+), paints the bar itself green/yellow/red
  PBM_SETSTATE = $0400 + 16; // WM_USER + 16
  PBST: array[TProgressBarState] of PtrUInt = (1, 2, 3); // PBST_NORMAL, PBST_ERROR, PBST_PAUSED
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // The native win32 progress bar ignores TProgressBar.Color for the bar itself,
  // so send PBM_SETSTATE like VCL's State property does.
  LCLIntf.SendMessage(progressbarPasswordStrength.Handle, PBM_SETSTATE, PBST[State], 0);
  {$ENDIF}
  // Fallback for other widgetsets: set the control's background color.
  case State of
    pbsError: progressbarPasswordStrength.Color := clRed;
    pbsPaused: progressbarPasswordStrength.Color := clYellow;
    pbsNormal: progressbarPasswordStrength.Color := clGreen;
  end;
end;


end.
