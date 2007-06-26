unit edituser;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormEditUser = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EditUsername: TEdit;
    EditHost: TEdit;
    EditPassword1: TEdit;
    EditPassword2: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEditUser: TFormEditUser;

implementation

uses usermanager, childwin, MAIN;

{$I const.inc}
{$R *.DFM}

procedure TFormEditUser.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TFormEditUser.FormShow(Sender: TObject);
begin
  EditUsername.Text := UserManagerForm.user;
  EditHost.Text := UserManagerForm.host;
  EditPassword1.Text := '';
  EditPassword2.Text := '';
  EditUsername.SetFocus;
end;

procedure TFormEditUser.Button2Click(Sender: TObject);
var passwdset : String;
begin
  // Save credentials
  Screen.Cursor := crHourglass;
  if EditPassword1.Text <> EditPassword2.Text then begin
    Screen.Cursor := crDefault;
    MessageDlg('Retyped password doesn''t match with first password.', mtError, [mbOK], 0);
    EditPassword2.SetFocus;
    abort;
  end;
  passwdset := ', Password=password('''+Self.EditPassword1.Text+''')';
  if EditPassword1.Text = '' then begin
    // No password?
    Screen.Cursor := crDefault;

    if MessageDlg('Set empty password?' + CRLF + 'Press no to leave the old password.',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      passwdset := '';
  end;
  Mainform.ChildWin.ExecUpdateQuery('UPDATE mysql.user SET Host='''+EditHost.Text+''', User='''+EditUsername.Text+''' '+passwdset+' WHERE Host='''+UserManagerForm.host+''' AND User='''+UserManagerForm.user+'''');
  Mainform.ChildWin.ExecUpdateQuery('UPDATE mysql.db SET Host='''+EditHost.Text+''', User='''+EditUsername.Text+''' WHERE Host='''+UserManagerForm.host+''' AND User='''+UserManagerForm.user+'''');
  Mainform.ChildWin.ExecUpdateQuery('UPDATE mysql.tables_priv SET Host='''+EditHost.Text+''', User='''+EditUsername.Text+''' WHERE Host='''+UserManagerForm.host+''' AND User='''+UserManagerForm.user+'''');
  Mainform.ChildWin.ExecUpdateQuery('UPDATE mysql.columns_priv SET Host='''+EditHost.Text+''', User='''+EditUsername.Text+''' WHERE Host='''+UserManagerForm.host+''' AND User='''+UserManagerForm.user+'''');
  Mainform.ChildWin.ExecUpdateQuery('FLUSH PRIVILEGES');

  UserManagerForm.ShowPrivilegesControls(false, true, false);

  // Clear and refill user-list
  UserManagerForm.ZQueryDBs.Close;
  UserManagerForm.ZQueryTables.Close;
  UserManagerForm.ZQueryColumns.Close;
  UserManagerForm.ZQueryUsers.Close;
  UserManagerForm.ZQueryColumnNames.Close;
  UserManagerForm.TreeViewUsers.Items.Clear;
  UserManagerForm.PageControl1.OnChange(self);

  Screen.Cursor := crdefault;
  Close;
end;

end.
