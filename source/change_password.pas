unit change_password;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, extra_controls, gnugettext,
  Vcl.Menus, Clipbrd, Vcl.ComCtrls, System.Math;

type
  TfrmPasswordChange = class(TExtForm)
    lblHeading: TLabel;
    lblPassword: TLabel;
    lblRepeatPassword: TLabel;
    editPassword: TButtonedEdit;
    editRepeatPassword: TButtonedEdit;
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
    btnCopyToClipboard: TButton;
    progressbarPasswordStrength: TProgressBar;
    procedure editPasswordChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure editPasswordKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure menuPasswordClick(Sender: TObject);
    procedure menuPasswordInsert(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure CheckPasswordStrength;
  public
    { Public-Deklarationen }
  end;

var
  frmPasswordChange: TfrmPasswordChange;

implementation

uses main, apphelpers;

{$R *.dfm}


procedure TfrmPasswordChange.FormCreate(Sender: TObject);
begin
  HasSizeGrip := True;
end;


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
  Clipboard.AsText := editPassword.Text;
  OldImageIndex := btnCopyToClipboard.ImageIndex;
  btnCopyToClipboard.ImageIndex := 55;
  btnCopyToClipboard.Repaint;
  Sleep(500);
  btnCopyToClipboard.ImageIndex := OldImageIndex;
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
  case progressbarPasswordStrength.Position of
    0..20: progressbarPasswordStrength.State := pbsError;
    21..50: progressbarPasswordStrength.State := pbsPaused;
    51..100: progressbarPasswordStrength.State := pbsNormal;
  end;
end;


end.
