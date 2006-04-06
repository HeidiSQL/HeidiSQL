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

uses usermanager, childwin, mysql;

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

    if MessageDlg('Set empty password?' + #13#10 + 'Press no to leave the old password.',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      passwdset := '';
  end;
  with TMDIChild(Application.Mainform.ActiveMDIChild) do begin
    q('UPDATE mysql.user SET Host='''+self.EditHost.Text+''', User='''+Self.EditUsername.Text+''' '+passwdset+' WHERE Host='''+UserManagerForm.host+''' AND User='''+UserManagerForm.user+'''');
    q('UPDATE mysql.db SET Host='''+self.EditHost.Text+''', User='''+Self.EditUsername.Text+''' WHERE Host='''+UserManagerForm.host+''' AND User='''+UserManagerForm.user+'''');
    q('UPDATE mysql.tables_priv SET Host='''+self.EditHost.Text+''', User='''+Self.EditUsername.Text+''' WHERE Host='''+UserManagerForm.host+''' AND User='''+UserManagerForm.user+'''');
    q('UPDATE mysql.columns_priv SET Host='''+self.EditHost.Text+''', User='''+Self.EditUsername.Text+''' WHERE Host='''+UserManagerForm.host+''' AND User='''+UserManagerForm.user+'''');
    q('FLUSH PRIVILEGES');
  end;
  // Clear and refill user-list
  with UserManagerForm do begin
    if UsersResult <> nil then begin
      mysql_free_result(UsersResult);
      UsersResult := nil;
    end;
    if DBsResult <> nil then begin
      mysql_free_result(DBsResult);
      dbsresult := nil;
    end;
    if TablesResult <> nil then begin
      mysql_free_result(TablesResult);
      tablesresult := nil;
    end;
    if ColumnsResult <> nil then begin
      mysql_free_result(ColumnsResult);
      columnsresult := nil;
    end;
  end;

  UserManagerForm.ShowPrivilegesControls(false, true, false);
  UserManagerForm.TreeViewUsers.Items.Clear;
  UserManagerForm.PageControl1.OnChange(self);
  Screen.Cursor := crdefault;
  Close;
end;

end.
