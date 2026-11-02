unit loginform;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, dbstructures.mysql;

type
  TfrmLogin = class(TForm)
    btnOK: TButton;
    pnlBackground: TPanel;
    lblPrompt: TLabel;
    lblUsername: TLabel;
    lblPassword: TLabel;
    editPassword: TEdit;
    editUsername: TEdit;
    imgIcon: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


// Callback for libmysqlclient's "dialog" authentication plugin, exported from
// heidisql.lpr. Shows this login form when the server asks for user input.
function mysql_authentication_dialog_ask(
    Handle: PMYSQL;
    _type: Integer;
    prompt: PAnsiChar;
    buf: PAnsiChar;
    buf_len: Integer
    ): PAnsiChar; cdecl;


implementation

uses apphelpers, main, dbstructures;

{$R *.lfm}
{$I const.inc}



procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  Caption := APPNAME + ' - Login';
  lblPrompt.Font.Size := 10;
  lblPrompt.Font.Color := GetThemeColor(clHotlight);
  lblPrompt.Font.Style := lblPrompt.Font.Style + [fsBold];
  editUsername.Text := '';
  editPassword.Text := '';
end;

procedure TfrmLogin.FormShow(Sender: TObject);
begin
  if editPassword.CanFocus and (editUsername.GetTextLen > 0) and (editPassword.GetTextLen = 0) then
    editPassword.TrySetFocus
  else if editUsername.CanFocus then
    editUsername.TrySetFocus;
end;


function mysql_authentication_dialog_ask;
var
  Dialog: TfrmLogin;
begin
  {
  From client_plugin.h:
    The C function with the name "mysql_authentication_dialog_ask", if exists,
    will be used by the "dialog" client authentication plugin when user
    input is needed. This function should be of mysql_authentication_dialog_ask_t
    type. If the function does not exists, a built-in implementation will be
    used.
    @param mysql          mysql
    @param type           type of the input
                          1 - normal string input
                          2 - password string
    @param prompt         prompt
    @param buf            a buffer to store the use input
    @param buf_len        the length of the buffer
    @retval               a pointer to the user input string.
                          It may be equal to 'buf' or to 'mysql->password'.
                          In all other cases it is assumed to be an allocated
                          string, and the "dialog" plugin will free() it.
  Test suite:
    INSTALL PLUGIN three_attempts SONAME 'dialog.dll';
    CREATE USER test_dialog IDENTIFIED VIA three_attempts USING 'SECRET';
  }
  Dialog := TfrmLogin.Create(nil);
  Dialog.lblPrompt.Caption := String(prompt);
  Dialog.editUsername.Width := Dialog.editUsername.Width + (Dialog.editUsername.Left - Dialog.lblUsername.Left);
  Dialog.editPassword.Width := Dialog.editUsername.Width;
  Dialog.lblUsername.Visible := False;
  Dialog.lblPassword.Visible := False;
  Dialog.editUsername.Left := Dialog.lblUsername.Left;
  Dialog.editPassword.Left := Dialog.lblPassword.Left;
  Dialog.editUsername.Top := Dialog.lblPrompt.Top + Dialog.lblPrompt.Height + 15;
  Dialog.editPassword.Top := Dialog.editUsername.Top;
  Dialog.editUsername.Visible := _type=1;
  Dialog.editPassword.Visible := _type=2;
  Dialog.ShowModal;
  case _type of
    1: Result := PAnsiChar(AnsiString(Dialog.editUsername.Text));
    2: Result := PAnsiChar(AnsiString(Dialog.editPassword.Text));
    else raise EDbError.CreateFmt(_('Unsupported type (%d) in %s.'), [_type, 'mysql_authentication_dialog_ask']);
  end;
  Dialog.Free;
end;

end.
