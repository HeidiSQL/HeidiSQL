unit loginform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmLogin = class(TForm)
    btnOK: TButton;
    editPassword: TEdit;
    lblPrompt: TLabel;
    editUsername: TEdit;
    lblUsername: TLabel;
    lblPassword: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure LoginPrompt(ACaption: String; var AUsername, APassword: String; UsernameEnabled: Boolean=True; PasswordEnabled: Boolean=True);

implementation

uses helpers;

{$R *.dfm}
{$I const.inc}

procedure LoginPrompt(ACaption: String; var AUsername, APassword: String; UsernameEnabled: Boolean=True; PasswordEnabled: Boolean=True);
var
  frm: TfrmLogin;
begin
  // Create login box and pass back user + pass
  frm := TfrmLogin.Create(nil);
  frm.lblPrompt.Caption := ACaption;
  frm.editUsername.Text := AUsername;
  frm.editPassword.Text := APassword;
  frm.editUsername.Enabled := UsernameEnabled;
  frm.lblUsername.Enabled := UsernameEnabled;
  frm.editPassword.Enabled := PasswordEnabled;
  frm.lblPassword.Enabled := PasswordEnabled;
  frm.ShowModal;
  AUsername := frm.editUsername.Text;
  APassword := frm.editPassword.Text;
  frm.Free;
end;


procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
  Caption := APPNAME + ' - Login';
end;

procedure TfrmLogin.FormShow(Sender: TObject);
begin
  if editPassword.CanFocus and (editUsername.GetTextLen > 0) and (editPassword.GetTextLen = 0) then
    editPassword.SetFocus
  else if editUsername.CanFocus then
    editUsername.SetFocus;
end;

end.
