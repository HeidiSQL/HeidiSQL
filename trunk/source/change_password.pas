unit change_password;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, extra_controls, gnugettext,
  Vcl.Menus, Clipbrd;

type
  TfrmPasswordChange = class(TFormWithSizeGrip)
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
    procedure editPasswordChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure editPasswordKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure menuPasswordClick(Sender: TObject);
    procedure menuPasswordInsert(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmPasswordChange: TfrmPasswordChange;

implementation

uses main, helpers;

{$R *.dfm}


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
begin
  // User has entered something on one or both password fields
  btnOK.Enabled := False;
  btnCopyToClipboard.Enabled := False;

  if editPassword.Text = '' then begin
    editPassword.PasswordChar := #0;
    lblStatus.Caption := _('Please change your password')
  end else begin
    editPassword.PasswordChar := '*';
    if editPassword.Text <> editRepeatPassword.Text then
      lblStatus.Caption := _('Error: Passwords do not match!')
    else begin
      lblStatus.Caption := '';
      btnOK.Enabled := True;
      btnCopyToClipboard.Enabled := True;
    end;
  end;
end;


procedure TfrmPasswordChange.editPasswordKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  editPassword.OnChange(Sender);
end;


end.
