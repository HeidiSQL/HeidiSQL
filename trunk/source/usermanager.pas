unit usermanager;


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, CheckLst, ExtCtrls, Buttons, DB,
  Registry, ToolWin;

{$I const.inc}


type
  TPrivilege = class(TObject)
    private
      FDBOType: Byte;
      // Internal Flags
      FDeleted: Boolean;
      FAdded: Boolean;
      FModified: Boolean;
      function GetDBOKey: String;
      function GetDBOPrettyKey: String;
      function GetPrettyPrivNames: TStringList;
    public
      DBONames: TStringList;
      PrivNames: TStringList;
      SelectedPrivNames: TStringList;
      constructor Create(Fields: TFields; FieldDefs: TDataset = nil; AvoidFieldDefs: TDataSet = nil; CropFieldDefs: TDataSet = nil; SimulateDbField: Boolean = False);
      procedure Merge(Fields: TFields);
      property DBOType: Byte read FDBOType;
      property DBOKey: String read GetDBOKey;
      property DBOPrettyKey: String read GetDBOPrettyKey;
      property PrettyPrivNames: TStringList read GetPrettyPrivNames;
      property Added: Boolean read FAdded write FAdded;
      property Modified: Boolean read FModified write FModified;
      property Deleted: Boolean read FDeleted write FDeleted;
  end;

  TUser = class; // Forward declaration

  TPrivileges = class(TObject)
    private
      FPrivilegeItems: Array of TPrivilege;
      FOwner: TUser;
      function GetPrivilege(Index: Integer): TPrivilege;
      function GetCount: Integer;
    public
      constructor Create(AOwner: TUser);
      function AddPrivilege(Fields: TFields; FieldDefs: TDataset = nil; AvoidFieldDefs: TDataSet = nil; CropFieldDefs: TDataSet = nil; SimulateDbField: Boolean = False): TPrivilege;
      property Items[Index: Integer]: TPrivilege read GetPrivilege; default;
      property Count: Integer read GetCount;
      procedure DeletePrivilege(Index: Integer);
      function FindPrivilege(Fields: TFields; SimulateDbField: Boolean): TPrivilege;
  end;

  TUser = class(TObject)
    private
      FOldName: String;
      FNewName: String;
      FNameChanged: Boolean;
      FPassword: String;
      FOldPasswordHashed: String;
      FOldHost: String;
      FNewHost: String;
      FOldMaxQuestions: Cardinal;
      FOldMaxUpdates: Cardinal;
      FOldMaxConnections: Cardinal;
      FOldMaxUserConnections: Cardinal;
      FMaxQuestions: Cardinal;
      FMaxUpdates: Cardinal;
      FMaxConnections: Cardinal;
      FMaxUserConnections: Cardinal;
      // Internal Flags
      FDeleted: Boolean;
      FDisabled: Boolean;
      FAdded: Boolean;
      FPasswordModified: Boolean;
      FOtherModified: Boolean;
      FPrivileges: TPrivileges;
      function GetModified: Boolean;
      function GetName: String;
      procedure SetName(str: String);
      function GetHost: String;
      procedure SetHost(str: String);
      procedure SetPassword(str: String);
    public
      constructor Create(Fields: TFields = nil); overload;
      constructor Create(Name: String; Host: String); overload;
      property Name: String read GetName write SetName;
      property Host: String read GetHost write SetHost;
      property Password: String read FPassword write SetPassword;
      property OldPasswordHashed: String read FOldPasswordHashed write FOldPasswordHashed;
      property Privileges: TPrivileges read FPrivileges;
      property MaxQuestions: Cardinal read FMaxQuestions write FMaxQuestions;
      property MaxUpdates: Cardinal read FMaxUpdates write FMaxUpdates;
      property MaxConnections: Cardinal read FMaxConnections write FMaxConnections;
      property MaxUserConnections: Cardinal read FMaxUserConnections write FMaxUserConnections;
      property Deleted: Boolean read FDeleted write FDeleted;
      property Disabled: Boolean read FDisabled write FDisabled;
      property Added: Boolean read FAdded;
      property Modified: Boolean read GetModified write FOtherModified;
      property PasswordModified: Boolean read FPasswordModified write FPasswordModified;
  end;

  TUsers = class(TObject)
    constructor Create;
    private
      FUserItems: Array of TUser;
      function GetUser(Index: Integer): TUser;
      function GetCount: Integer;
    public
      // The default property, can be access via Object[nr]
      property Items[Index: Integer]: TUser read GetUser; default;
      property Count: Integer read GetCount;
      procedure AddUser(User: TUser);
      procedure DeleteUser(Index: Integer);
      function FindUser(Name: String; Host: String): TUser;
  end;

  TUserManagerForm = class(TForm)
    lblUser: TLabel;
    comboUsers: TComboBoxEx;
    btnCancel: TButton;
    btnOK: TButton;
    comboObjects: TComboBoxEx;
    boxPrivs: TCheckListBox;
    tlbObjects: TToolBar;
    btnAddObject: TToolButton;
    btnDeleteObject: TToolButton;
    tlbUsers: TToolBar;
    btnAddUser: TToolButton;
    btnDeleteUser: TToolButton;
    chkTogglePrivs: TCheckBox;
    PageControlUser: TPageControl;
    tabSettings: TTabSheet;
    lblFromHost: TLabel;
    lblPassword: TLabel;
    editPassword: TEdit;
    editFromHost: TEdit;
    tabLimitations: TTabSheet;
    lblMaxQuestions: TLabel;
    lblMaxUpdates: TLabel;
    lblMaxConnections: TLabel;
    lblMaxUserConnections: TLabel;
    editMaxUserConnections: TEdit;
    editMaxConnections: TEdit;
    editMaxUpdates: TEdit;
    editMaxQuestions: TEdit;
    udMaxQuestions: TUpDown;
    udMaxUpdates: TUpDown;
    udMaxConnections: TUpDown;
    udMaxUserConnections: TUpDown;
    tabUserInfo: TTabSheet;
    lblFullName: TLabel;
    editFullName: TEdit;
    chkDisabled: TCheckBox;
    lblDescription: TLabel;
    editDescription: TEdit;
    lblEmail: TLabel;
    editEmail: TEdit;
    lblContactInfo: TLabel;
    memoContactInfo: TMemo;
    lblUsername: TLabel;
    editUsername: TEdit;
    lblLimitHint: TLabel;
    lblHostHint: TLabel;
    lblWarning: TLabel;
    procedure boxPrivsClickCheck(Sender: TObject);
    procedure btnAddObjectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddUserClick(Sender: TObject);
    procedure btnDeleteObjectClick(Sender: TObject);
    procedure btnDeleteUserClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure chkDisabledClick(Sender: TObject);
    procedure chkTogglePrivsClick(Sender: TObject);
    procedure comboObjectsChange(Sender: TObject);
    procedure comboUsersChange(Sender: TObject);
    procedure editFromHostChange(Sender: TObject);
    procedure editLimitations(Sender: TObject);
    procedure editPasswordChange(Sender: TObject);
    procedure editPasswordEnter(Sender: TObject);
    procedure editPasswordExit(Sender: TObject);
    procedure editUsernameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    Users: TUsers;
    procedure RefreshGUI;
    procedure RefreshUserPulldown;
    procedure SetChkToggleStatus;
  public
    { Public declarations }
  end;

procedure GetPrivilegeRowKey(Fields: TFields; SimulateDbField: Boolean; out DBOType: Byte; out DBONames: TStringList);

implementation


uses
  main, childwin, helpers, selectdbobject;


var
  CWin: TMDIChild;
  db: String;
  // Results from SELECT * FROM user/db/...
  dsUser, dsDb, dsTables, dsColumns,
  // Results from SHOW FIELDS FROM user/db/...
  dsTablesFields, dsColumnsFields : TDataset;


{$R *.DFM}



{**
  FormCreate: Restore GUI setup
}
procedure TUserManagerForm.FormCreate(Sender: TObject);
begin
  Width := Mainform.GetRegValue(REGNAME_USERMNGR_WINWIDTH, Width);
  Height := Mainform.GetRegValue(REGNAME_USERMNGR_WINHEIGHT, Height);
  db := Mainform.Mask(DBNAME_MYSQL);
end;


{**
  FormDestroy: Save GUI setup
}
procedure TUserManagerForm.FormDestroy(Sender: TObject);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  if reg.OpenKey(REGPATH, False) then begin
    reg.WriteInteger( REGNAME_USERMNGR_WINWIDTH, Width );
    reg.WriteInteger( REGNAME_USERMNGR_WINHEIGHT, Height );
    reg.CloseKey;
  end;
  reg.Free;
end;


{**
  FormResize: Adjust columns of boxPriv
}
procedure TUserManagerForm.FormResize(Sender: TObject);
begin
  boxPrivs.Columns := Trunc(Width / 110);
end;


{**
  FormShow: Load users and privileges into memory
}
procedure TUserManagerForm.FormShow(Sender: TObject);
var
  test_result: String;
begin
  // Test if we can access the privileges database and tables by
  // A. Using the mysql-DB
  CWin := Mainform.Childwin;
  try
    CWin.ExecuteNonQuery('USE ' + db);
  except
    MessageDlg('You have no access to the privileges database.', mtError, [mbOK], 0);
    ModalResult := mrCancel;
    Exit;
  end;
  // B. retrieving a count of all users.
  test_result := cwin.GetVar( 'SELECT COUNT(*) FROM '+db+'.'+Mainform.Mask(PRIVTABLE_USERS), 0, true, false );
  if test_result = '' then begin
    MessageDlg('You have no access to the privileges tables.', mtError, [mbOK], 0);
    ModalResult := mrCancel;
    Exit;
  end;

  // Load users into memory
  Users := TUsers.Create;
  // Enable limitations editors only if relevant fields exist
  lblMaxQuestions.Enabled := dsUser.FindField('max_questions') <> nil;
  editMaxQuestions.Enabled := lblMaxQuestions.Enabled;
  udMaxQuestions.Enabled := lblMaxQuestions.Enabled;

  lblMaxUpdates.Enabled := dsUser.FindField('max_updates') <> nil;
  editMaxQuestions.Enabled := lblMaxUpdates.Enabled;
  udMaxUpdates.Enabled := lblMaxUpdates.Enabled;

  lblMaxConnections.Enabled := dsUser.FindField('max_connections') <> nil;
  editMaxConnections.Enabled := lblMaxConnections.Enabled;
  udMaxConnections.Enabled := lblMaxConnections.Enabled;

  lblMaxUserConnections.Enabled := dsUser.FindField('max_user_connections') <> nil;
  editMaxUserConnections.Enabled := lblMaxUserConnections.Enabled;
  udMaxUserConnections.Enabled := lblMaxUserConnections.Enabled;

  RefreshGUI;
end;


{**
  FormClose: Free memory from privileges
}
procedure TUserManagerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Users.Free;
end;


{**
  Load all users into GUI and select one in the pulldown
}
procedure TUserManagerForm.RefreshGUI;
begin
  RefreshUserPulldown;
  // Manually invoke change-event of pulldown to verify valid state of GUI
  comboUsersChange(Self);
end;


{**
  Set correct image index of users in pulldown, corresponding
  to their modified / added / deleted state
}
procedure TUserManagerForm.RefreshUserPulldown;
var
  i, Selected, IconIndex: Integer;
  Username: String;
  NoUser: Boolean;
begin
  Selected := comboUsers.ItemIndex;
  comboUsers.ItemsEx.Clear;
  for i:=0 to Users.Count-1 do begin
    NoUser := Users[i].Name = '';

    // Compose displayed username
    Username := Users[i].Name;
    if NoUser then Username := 'Everybody';
    if (Users[i].Host <> '%') then
      Username := Username + '@' + Users[i].Host;

    // Detect modified status of the user object itself
    if NoUser then begin
      if Users[i].Deleted then IconIndex := 86
      else if Users[i].Added then IconIndex := 85
      else if Users[i].Modified then IconIndex := 84
      else IconIndex := 11;
    end else begin
      if Users[i].Deleted then IconIndex := 83
      else if Users[i].Added then IconIndex := 21
      else if Users[i].Modified then IconIndex := 12
      else IconIndex := 43;
    end;

    comboUsers.ItemsEx.AddItem(Username, IconIndex, IconIndex, -1, 0, nil);
  end;
  if Users.Count > 0 then begin
    if (Selected > -1) and (Selected < Users.Count) then
      comboUsers.ItemIndex := Selected
    else
      comboUsers.ItemIndex := 0; // Relevant at first time loading
  end;
end;

{**
  A user was selected from the pulldown
}
procedure TUserManagerForm.comboUsersChange(Sender: TObject);
var
  uid, i, Icon: Integer;
  u: TUser;
  OneSelected, Enable: Boolean;
  t: TNotifyEvent;
begin
  lblWarning.Visible := False;
  uid := comboUsers.ItemIndex;
  comboObjects.ItemsEx.Clear;
  OneSelected := uid > -1;
  Enable := OneSelected;
  if OneSelected then begin
    u := Users[uid];
    Enable := not u.Deleted;

    t := editUsername.OnChange;
    editUsername.OnChange := nil;
    editUsername.Text := u.Name;
    if u.Name = '' then editUsername.Text := '%';
    editUsername.OnChange := t;

    editPasswordExit(Sender);

    t := editFromHost.OnChange;
    editFromHost.OnChange := nil;
    editFromHost.Text := u.Host;
    editFromHost.OnChange := t;

    t := chkDisabled.OnClick;
    chkDisabled.OnClick := nil;
    chkDisabled.Checked := u.Disabled;
    chkDisabled.OnClick := t;

    // Activate "deleted" warning
    if u.Deleted then begin
      lblWarning.Visible := True;
      lblWarning.Caption := 'User is marked for deletion.';
    end;

    // Limitations
    t := editMaxQuestions.OnChange;
    editMaxQuestions.OnChange := nil;
    editMaxUpdates.OnChange := nil;
    editMaxConnections.OnChange := nil;
    editMaxUserConnections.OnChange := nil;
    udMaxQuestions.Position := u.MaxQuestions;
    udMaxUpdates.Position := u.MaxUpdates;
    udMaxConnections.Position := u.MaxConnections;
    udMaxUserConnections.Position := u.MaxUserConnections;
    editMaxQuestions.OnChange := t;
    editMaxUpdates.OnChange := t;
    editMaxConnections.OnChange := t;
    editMaxUserConnections.OnChange := t;

    // Display priv objects
    for i := 0 to u.Privileges.Count - 1 do begin
      // Hide deleted priv objects
      if u.Privileges[i].Deleted then
        Continue;
      Icon := -1;
      case u.Privileges[i].DBOType of
        NODETYPE_DEFAULT:   Icon := ICONINDEX_SERVER;
        NODETYPE_DB:        Icon := ICONINDEX_DB;
        NODETYPE_BASETABLE: Icon := ICONINDEX_TABLE;
        NODETYPE_VIEW:      Icon := ICONINDEX_VIEW;
        NODETYPE_COLUMN:    Icon := ICONINDEX_FIELD;
      end;
      comboObjects.ItemsEx.AddItem(u.Privileges[i].DBOPrettyKey, Icon, Icon, -1, 0, nil);
    end;
    if comboObjects.ItemsEx.Count > 0 then
      comboObjects.ItemIndex := 0;
    comboObjectsChange(Sender);
  end else begin
    editUsername.Text := '';
    editPassword.Text := '';
    editFromHost.Text := '';
    udMaxQuestions.Position := 0;
    udMaxUpdates.Position := 0;
    udMaxConnections.Position := 0;
    udMaxUserConnections.Position := 0;
  end;
  // Update top buttons
  btnDeleteUser.Enabled := Enable;
  // Update control in Settings tab
  lblUsername.Enabled := Enable;
  editUsername.Enabled := Enable;
  lblPassword.Enabled := Enable;
  editPassword.Enabled := Enable;
  lblFromHost.Enabled := Enable;
  editFromHost.Enabled := Enable;
  lblHostHint.Enabled := Enable;
  chkDisabled.Enabled := Enable;
  // Update controls in Limitations tab
  lblMaxQuestions.Enabled := Enable;
  editMaxQuestions.Enabled := Enable;
  udMaxQuestions.Enabled := Enable;
  lblMaxUpdates.Enabled := Enable;
  editMaxUpdates.Enabled := Enable;
  udMaxUpdates.Enabled := Enable;
  lblMaxConnections.Enabled := Enable;
  editMaxConnections.Enabled := Enable;
  udMaxConnections.Enabled := Enable;
  lblMaxUserConnections.Enabled := Enable;
  editMaxUserConnections.Enabled := Enable;
  udMaxUserConnections.Enabled := Enable;
  // Update controls for privileges
  comboObjects.Enabled := Enable;
  btnAddObject.Enabled := Enable;
  boxPrivs.Enabled := Enable;
  chkTogglePrivs.Enabled := Enable;
end;


{**
  Create new user
}
procedure TUserManagerForm.btnAddUserClick(Sender: TObject);
const
  name: String = 'New User';
var
  u: TUser;
begin
  // Avoid duplicates.
  u := Users.FindUser(name, '%');
  if u <> nil then begin
    comboUsers.ItemIndex := comboUsers.Items.IndexOf(name);
    comboUsersChange(Self);
    editUserName.SetFocus;
    MessageDlg('User/host combination "'+name+'@%" already exists.'+CRLF+CRLF+'Please chose a different username.', mtError, [mbOK], 0);
    Exit;
  end;
  Users.AddUser(TUser.Create(name, '%'));
  RefreshGUI;
  // Select newly added item.
  comboUsers.ItemIndex := comboUsers.ItemsEx.Count - 1;
  comboUsersChange(Self);
  // Focus the user name entry box.
  editUserName.SetFocus;
end;


{**
  Delete user
}
procedure TUserManagerForm.btnDeleteUserClick(Sender: TObject);
begin
  if MessageDlg('Delete user "'+comboUsers.ItemsEx[comboUsers.ItemIndex].Caption+'"?', mtConfirmation, [mbYes, mbCancel], 0 ) <> mrYes then
    Exit;
  Users.DeleteUser(comboUsers.ItemIndex);
  RefreshGUI;
end;


{**
  Disable a user
}
procedure TUserManagerForm.chkDisabledClick(Sender: TObject);
var
  disabled: Boolean;
  u: TUser;
begin
  disabled := TCheckbox(Sender).Checked;
  u := Users[comboUsers.ItemIndex];
  u.Disabled := disabled;
  // Reset password from "!" to empty string. Avoids again disabling it
  // by just leaving and saving the exclamation mark
  if u.Password = '!' then u.Password := '';
  u.Modified := True;
  comboUsersChange(self);
end;


{**
  Database object selected
}
procedure TUserManagerForm.comboObjectsChange(Sender: TObject);
var
  priv: TPrivilege;
  EnableDelete: Boolean;
  i: Integer;
begin
  boxPrivs.OnClickCheck := nil;
  boxPrivs.Items.BeginUpdate;
  boxPrivs.Items.Clear;
  EnableDelete := False;
  if comboObjects.ItemIndex > -1 then begin
    priv := Users[comboUsers.ItemIndex].Privileges[comboObjects.ItemIndex];
    boxPrivs.Items := priv.PrettyPrivNames;
    // Check selected privs
    for i := 0 to priv.PrivNames.Count - 1 do begin
      boxPrivs.Checked[i] := priv.SelectedPrivNames.IndexOf(priv.PrivNames[i]) > -1;
    end;
    EnableDelete := (priv.DBOType <> NODETYPE_DEFAULT) and
      (priv.DBOKey <> '%');
  end;
  if comboUsers.ItemIndex > -1 then
    EnableDelete := EnableDelete and (not Users[comboUsers.ItemIndex].Deleted);
  boxPrivs.Items.EndUpdate;
  btnDeleteObject.Enabled := EnableDelete;
  SetChkToggleStatus;
  boxPrivs.OnClickCheck := boxPrivsClickCheck;
end;


procedure TUserManagerForm.btnAddObjectClick(Sender: TObject);
var
  NewObj: TStringList;
  ds, FieldDefs: TDataset;
  NewPriv: TPrivilege;
  u: TUser;
  i: Integer;
begin
  NewObj := SelectDBO;
  if NewObj <> nil then begin
    u := Users[comboUsers.ItemIndex];
    for i := 0 to u.Privileges.Count - 1 do begin
      NewObj.Delimiter := u.Privileges[i].DBONames.Delimiter;
      if u.Privileges[i].DBOKey = NewObj.DelimitedText then begin
        MessageDlg(NewObj.DelimitedText+' already exists.', mtError, [mbOK], 0);
        comboObjects.ItemIndex := i;
        comboObjectsChange(Sender);
        Exit;
      end;
    end;
    ds := nil;
    FieldDefs := nil;
    case NewObj.Count of
      1: ds := dsDb;
      2: begin ds := dsTables; FieldDefs := dsTablesFields; end;
      3: begin ds := dsColumns; FieldDefs := dsColumnsFields; end;
      else
        Exception.Create('Added privilege object has an invalid number of segments ('+IntToStr(NewObj.Count)+')');
    end;
    NewPriv := u.Privileges.AddPrivilege(ds.Fields, FieldDefs);
    NewPriv.Added := True;
    NewPriv.DBONames := NewObj;
    u.Modified := True;
    RefreshUserPulldown;
    // Display new priv:
    comboUsersChange(Sender);
    comboObjects.ItemIndex := comboObjects.ItemsEx.Count-1;
    comboObjectsChange(Sender);
  end;
end;


{**
  Revoke access to an object
}
procedure TUserManagerForm.btnDeleteObjectClick(Sender: TObject);
begin
  Users[comboUsers.ItemIndex].Privileges.DeletePrivilege(comboObjects.ItemIndex);
  RefreshGUI;
end;


{**
  Manual click within boxPrivs
}
procedure TUserManagerForm.boxPrivsClickCheck(Sender: TObject);
var
  p: TPrivilege;
  i: Integer;
begin
  p := Users[comboUsers.ItemIndex].Privileges[comboObjects.ItemIndex];
  p.SelectedPrivNames.Clear;
  for i := 0 to boxPrivs.Count - 1 do begin
    if boxPrivs.Checked[i] then
      p.SelectedPrivNames.Add(p.PrivNames[i]);
  end;
  p.Modified := True;
  Users[comboUsers.ItemIndex].Modified := True;
  // Update users icons in pulldown
  RefreshUserPulldown;
  SetChkToggleStatus;
end;


{**
  Set the correct state of the tristate checkbox "Select / Deselect all"
}
procedure TUserManagerForm.SetChkToggleStatus;
var
  i : Integer;
  allSelected, noneSelected : Boolean;
begin
  allselected := True;
  noneSelected := True;
  for i := 0 to boxPrivs.Items.Count - 1 do begin
    if boxPrivs.Checked[i] then
      noneSelected := False
    else
      allSelected := False;
  end;
  // Disable clickevent handling
  chkTogglePrivs.OnClick := nil;
  if noneSelected then
    chkTogglePrivs.State := cbUnchecked
  else if allSelected then
    chkTogglePrivs.State := cbChecked
  else
    chkTogglePrivs.State := cbGrayed;
  // Enable clickevent handling
  chkTogglePrivs.OnClick := chkTogglePrivsClick;
end;


{**
  Username edited
}
procedure TUserManagerForm.editUsernameChange(Sender: TObject);
var
  u, f: TUser;
  t: TNotifyEvent;
begin
  u := Users[comboUsers.ItemIndex];
  // Check if user/host combination already exists
  f := Users.FindUser(editUsername.Text, u.Host);
  if (f = nil) or (f = u) then
    u.Name := editUsername.Text
  else
    MessageDlg('User/host combination "'+editUsername.Text+'@'+u.Host+'" already exists.'+CRLF+CRLF+'Please chose a different username.', mtError, [mbOK], 0);
  // User edit probably has to be reset to the previous value
  t := editUsername.OnChange;
  editUsername.OnChange := nil;
  // Blank user field means:
  // - this user is used for authentication failures
  // - privileges for this user applies to everyone
  // In effect, User='' means the same as User='%', except
  // that the latter syntax is not allowed (in the database).
  if u.Name = '' then editUserName.Text := '%'
  else editUsername.Text := u.Name;
  editUsername.OnChange := t;
  RefreshUserPulldown;
end;


{**
  "From Host" edited
}
procedure TUserManagerForm.editFromHostChange(Sender: TObject);
var
  u, f: TUser;
  t: TNotifyEvent;
begin
  u := Users[comboUsers.ItemIndex];
  // Check if user/host combination already exists
  f := Users.FindUser(u.Name, editFromHost.Text);
  if (f = nil) or (f = u) then
    u.Host := editFromHost.Text
  else
    MessageDlg('User/host combination "'+u.Name+'@'+editFromHost.Text+'" already exists.'+CRLF+CRLF+'Please choose a different hostname.', mtError, [mbOK], 0);
  // Host edit probably has to be reset to the previous value
  t := editFromHost.OnChange;
  editFromHost.OnChange := nil;
  editFromHost.Text := u.Host;
  editFromHost.OnChange := t;
  RefreshUserPulldown;
end;


{**
  Password field focused: Clear password if it's hashed
}
procedure TUserManagerForm.editPasswordEnter(Sender: TObject);
var
  e: TNotifyEvent;
begin
  if Users[comboUsers.ItemIndex].Password = '' then begin
    e := editPassword.OnChange;
    editPassword.OnChange := nil;
    editPassword.Clear;
    editPassword.PasswordChar := '*';
    editPassword.OnChange := e;
  end;
end;


{**
  Password edited
}
procedure TUserManagerForm.editPasswordChange(Sender: TObject);
var
  u: TUser;
  t: TNotifyEvent;
begin
  u := Users[comboUsers.ItemIndex];
  u.Password := editPassword.Text;
  if u.PasswordModified then begin
    u.Disabled := False;
    t := chkDisabled.OnClick;
    chkDisabled.OnClick := nil;
    chkDisabled.Checked := False;
    chkDisabled.OnClick := t;
  end;
  RefreshUserPulldown;
end;


{**
  Password field is unfocused: Apply change pw to user object
}
procedure TUserManagerForm.editPasswordExit(Sender: TObject);
var
  e: TNotifyEvent;
  u: TUser;
begin
  u := Users[comboUsers.ItemIndex];
  e := editPassword.OnChange;
  editPassword.OnChange := nil;
  editPassword.PasswordChar := #0;
  if u.Disabled then
    editPassword.Text := '!'
  else if not u.PasswordModified then
    editPassword.Text := u.OldPasswordHashed
  else begin
    editPassword.PasswordChar := '*';
    editPassword.Text := u.Password;
  end;
  editPassword.OnChange := e;
  // Show security warning for empty password if user is not disabled
  lblWarning.Visible := (not u.Disabled) and (
    (u.PasswordModified and (u.Password = '')) or
    ((not u.PasswordModified) and (u.OldPasswordHashed = ''))
  );
  lblWarning.Caption := 'This user has a blank password.';
end;



{**
  Select/Deselect all privileges
}
procedure TUserManagerForm.chkTogglePrivsClick(Sender: TObject);
begin
  ToggleCheckListBox(boxPrivs, (Sender as TCheckbox).Checked);
  // Ensure user gets marked as modified
  boxPrivsClickCheck(Sender);
end;


procedure TUserManagerForm.editLimitations(Sender: TObject);
var
  u: TUser;
begin
  // OnChange is called during form construction, avoid that.
  if comboUsers.ItemIndex = -1 then Exit;
  u := Users[comboUsers.ItemIndex];
  u.MaxQuestions := udMaxQuestions.Position;
  u.MaxUpdates := udMaxUpdates.Position;
  u.MaxConnections := udMaxConnections.Position;
  u.MaxUserConnections := udMaxUserConnections.Position;
  u.Modified := True;
  RefreshUserPulldown;
end;


{**
  OK clicked: Apply all changes
}
procedure TUserManagerForm.btnOKClick(Sender: TObject);
var
  i, j, k: Integer;
  u: TUser;
  p: TPrivilege;
  sql, TableName, SetFieldName: String;
  TableSet: TDataSet;
  AcctWhere, AcctValues, PrivWhere: String;
  AcctUpdates, PrivValues, PrivUpdates: TStringList;
  procedure LogSQL(sql: String);
  begin
    Mainform.Childwin.LogSQL(sql);
  end;
  procedure Exec(sql: String);
  begin
    //LogSQL(sql);  Exit;
    Mainform.Childwin.ExecuteNonQuery(sql);
  end;
  function Mask(sql: String): String;
  begin
    Result := Mainform.Mask(sql);
  end;
  function Delim(list: TStringList; spacing: Boolean = True): String;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to list.Count - 1 do begin
      if i > 0 then begin
        Result := Result + list.Delimiter;
        if spacing then Result := Result + ' ';
      end;
      Result := Result + list[i];
    end;
  end;
  function AccountSqlClause(name: String; host: String; canon: Boolean = False): String;
  begin
    Result := ' WHERE ' + mask('User') + '=' + esc(name) + ' AND (';
    Result := Result + mask('Host') + '=' + esc(host);
    if canon and (host = '%') then begin
      // Canonicalization: both '%' and '' means any host.
      Result := Result + ' OR ' + mask('Host') + '=' + esc('');
    end;
    Result := Result + ')';
  end;
begin
  LogSQL('Removing accounts marked for deletion...');
  for i := 0 to Users.Count - 1 do begin
    u := Users[i];
    // Only process deleted users in this loop.
    if not u.Deleted then Continue;
    // No need to delete users that do not exist yet.
    if u.Added then Continue;
    // Go.
    LogSQL('Deleting account ' + u.FOldName + '@' + u.FOldHost + '.');
    AcctWhere := AccountSqlClause(u.FOldName, u.FOldHost, True);
    sql := 'DELETE FROM ' + db + '.';
    Exec(sql + mask(PRIVTABLE_COLUMNS) + AcctWhere);
    Exec(sql + mask(PRIVTABLE_TABLES) + AcctWhere);
    Exec(sql + mask(PRIVTABLE_DB) + AcctWhere);
    Exec(sql + mask(PRIVTABLE_USERS) + AcctWhere);
  end;

  AcctUpdates := TStringList.Create;
  AcctUpdates.Delimiter := ',';
  LogSQL('Propagating key changes for renamed and redesignated accounts...');
  for i := 0 to Users.Count - 1 do begin
    u := Users[i];
    // Process accounts with name or host changes.
    if (u.FOldName = u.Name) and (u.FOldHost = u.Host) then Continue;
    // Note: must not skip added orphan users; they might have orphaned privileges that needs a key update.
    if (u.FOldName = 'New User') then Continue;
    AcctUpdates.Clear;
    // Apply user name update.
    if u.FOldName <> u.Name then
      AcctUpdates.Add(mask('User') + '=' + esc(u.Name));
    // Apply host criteria update.
    if u.FOldHost <> u.Host then
      AcctUpdates.Add(mask('Host') + '=' + esc(u.Host));
    // Skip accounts with other kinds of changes, they will be processed later.
    if AcctUpdates.Count = 0 then Continue;
    // Go.
    LogSQL('Renaming/redesignating account ' + u.FOldName + '@' + u.FOldHost + '.');
    AcctWhere := AccountSqlClause(u.FOldName, u.FOldHost);
    AcctValues := ' SET ' + Delim(AcctUpdates);
    sql := 'UPDATE ' + db + '.';
    // Todo: Allow concurrency by skipping this account and removing from Users array if changing key in mysql.user fails.
    Exec(sql + mask(PRIVTABLE_USERS) + AcctValues + AcctWhere);
    Exec(sql + mask(PRIVTABLE_DB) + AcctValues + AcctWhere);
    Exec(sql + mask(PRIVTABLE_TABLES) + AcctValues + AcctWhere);
    Exec(sql + mask(PRIVTABLE_COLUMNS) + AcctValues + AcctWhere);
  end;

  LogSQL('Applying changes to authentication details and limitations...');
  for i := 0 to Users.Count - 1 do begin
    u := Users[i];
    // Process accounts with changes in this loop.
    if not u.Modified then Continue;
    // No need to modify users that do not exist yet.
    if u.Added then Continue;
    // Decide what needs to be updated.
    AcctUpdates.Clear;
    // Apply password update.
    if u.Disabled then begin
      if u.OldPasswordHashed <> '!' then
        AcctUpdates.Add(mask('Password') + '=' + esc('!'));
    end else if u.PasswordModified then
      AcctUpdates.Add(mask('Password') + '= PASSWORD(' + esc(u.Password) + ')');
    // Apply limitation updates.
    if dsUser.FindField('max_questions') <> nil then begin
      if u.FOldMaxQuestions <> u.MaxQuestions then
        AcctUpdates.Add(mask('max_questions') + '=' + IntToStr(u.MaxQuestions));
      if u.FOldMaxUpdates <> u.MaxUpdates then
        AcctUpdates.Add(mask('max_updates') + '=' + IntToStr(u.MaxUpdates));
      if u.FOldMaxConnections <> u.MaxConnections then
        AcctUpdates.Add(mask('max_connections') + '=' + IntToStr(u.MaxConnections));
      if u.FOldMaxUserConnections <> u.MaxUserConnections then
        AcctUpdates.Add(mask('max_user_connections') + '=' + IntToStr(u.MaxUserConnections));
    end;
    // Skip accounts with fx only username / host changes, they've already been processed.
    if AcctUpdates.Count = 0 then Continue;
    // Go.
    LogSQL('Updating account ' + u.Name + '@' + u.Host + '.');
    AcctWhere := AccountSqlClause(u.Name, u.Host);
    AcctValues := ' SET ' + Delim(AcctUpdates);
    sql := 'UPDATE ' + db + '.';
    Exec(sql + mask(PRIVTABLE_USERS) + AcctValues + AcctWhere);
  end;

  LogSQL('Creating new accounts...');
  for i := 0 to Users.Count - 1 do begin
    u := Users[i];
    // Process only added accounts in this loop.
    if not u.Added then Continue;
    // Go.
    LogSQL('Creating account ' + u.Name + '@' + u.Host + '.');
    AcctUpdates.Clear;
    // Apply user name and host designation.
    AcctUpdates.Add(mask('Host') + '=' + esc(u.Host));
    AcctUpdates.Add(mask('User') + '=' + esc(u.Name));
    // Apply password.
    if u.Disabled then
      AcctUpdates.Add(mask('Password') + '=' + esc('!'))
    else
      AcctUpdates.Add(mask('Password') + '=PASSWORD(' + esc(u.Password) + ')');
    // Apply limits.
    if dsUser.FindField('max_questions') <> nil then begin
      AcctUpdates.Add(mask('max_questions') + '=' + IntToStr(u.MaxQuestions));
      AcctUpdates.Add(mask('max_updates') + '=' + IntToStr(u.MaxUpdates));
      AcctUpdates.Add(mask('max_connections') + '=' + IntToStr(u.MaxConnections));
      AcctUpdates.Add(mask('max_user_connections') + '=' + IntToStr(u.MaxUserConnections));
    end;
    // Special case: work around missing default values (bug) in MySQL.
    if dsUser.FindField('ssl_cipher') <> nil then begin
      AcctUpdates.Add(mask('ssl_cipher') + '=' + esc(''));
      AcctUpdates.Add(mask('x509_issuer') + '=' + esc(''));
      AcctUpdates.Add(mask('x509_subject') + '=' + esc(''));
    end;
    sql := 'INSERT INTO ' + db + '.' + mask(PRIVTABLE_USERS);
    sql := sql + ' SET ' + Delim(AcctUpdates);
    // Todo: Allow concurrency by skipping this account and removing from Users array if inserting key in mysql.user fails.
    Exec(sql);
  end;

  LogSQL('Applying privilege changes...');
  PrivUpdates := TStringList.Create;
  PrivUpdates.Delimiter := ',';
  PrivValues := TStringList.Create;
  PrivValues.Delimiter := ',';
  for i := 0 to Users.Count - 1 do begin
    u := Users[i];
    AcctWhere := AccountSqlClause(u.Name, u.Host, True);
    PrivUpdates.Clear;
    // Traverse the privilege definitions.
    for j := 0 to u.Privileges.Count - 1 do begin
      p := u.Privileges[j];
      // Apply only a collection of privilege values if it has modifications.
      if not p.Modified then Continue;
      // Go.
      LogSQL('Applying privilege to account ' + u.Name + '@' + u.Host + ' for ' + p.DBOPrettyKey + '.');
      case p.DBOType of
        NODETYPE_DEFAULT: begin
          TableSet := dsUser;
          TableName := mask(PRIVTABLE_USERS);
          SetFieldName := '';
        end;
        NODETYPE_DB: begin
          TableSet := dsDb;
          TableName := mask(PRIVTABLE_DB);
          SetFieldName := '';
        end;
        NODETYPE_BASETABLE: begin
          TableSet := dsTables;
          TableName := mask(PRIVTABLE_TABLES);
          SetFieldName := 'table_priv';
        end;
        NODETYPE_COLUMN: begin
          TableSet := dsColumns;
          TableName := mask(PRIVTABLE_COLUMNS);
          SetFieldName := 'column_priv';
        end;
        else begin
          raise Exception.Create('Processed privilege has an undefined db object type: ' + IntToStr(p.DBOType));
        end;
      end;
      // Deduce a key for this privilege definition, appropriate for DELETE.
      PrivWhere := '';
      for k := 0 to p.DBONames.Count - 1 do begin
        case k of
          0: PrivWhere := PrivWhere + ' AND ' + mask('Db') + '=' + esc(p.DBONames[k]);
          1: PrivWhere := PrivWhere + ' AND ' + mask('Table_name') + '=' + esc(p.DBONames[k]);
          2: PrivWhere := PrivWhere + ' AND ' + mask('Column_name') + '=' + esc(p.DBONames[k]);
        end;
      end;
      // Special case: remove redundant privileges in mysql.user.
      if (p.DBOType = NODETYPE_DB) and (p.DBOKey = '%') then begin
        PrivUpdates.Clear;
        for k := 0 to p.PrivNames.Count - 1 do begin
          if dsUser.FindField(p.PrivNames[k] + '_priv') <> nil then
            PrivUpdates.Add(mask(p.PrivNames[k] + '_priv') + '=' + esc('N'));
        end;
        sql := 'UPDATE ' + db + '.' + mask(PRIVTABLE_USERS);
        sql := sql + ' SET ' + Delim(PrivUpdates);
        Exec(sql + AcctWhere);
      end;
      // Remove old privilege definition.
      if (p.DBOType <> NODETYPE_DEFAULT) then begin
        sql := 'DELETE FROM ' + db + '.' + TableName;
        Exec(sql + AcctWhere + PrivWhere);
      end else begin
        // Special case: avoid removing old definition when dealing with
        //               server-level privileges, since they're entangled with
        //               authentication details over in mysql.user.
        //               instead, we have to set them manually to 'N'.
        PrivUpdates.Clear;
        for k := 0 to p.PrivNames.Count - 1 do
          if TableSet.FindField(p.PrivNames[k] + '_priv') <> nil then
            PrivUpdates.Add(mask(p.PrivNames[k] + '_priv') + '=' + esc('N'));
        sql := 'UPDATE ' + db + '.' + TableName;
        sql := sql + ' SET ' + Delim(PrivUpdates);
        sql := sql + AcctWhere;
      end;
      // Deduce a new key for this privilege definition, one appropriate for INSERT/UPDATE.
      PrivUpdates.Clear;
      PrivUpdates.Add(mask('Host') + '=' + esc(u.Host));
      PrivUpdates.Add(mask('User') + '=' + esc(u.Name));
      for k := 0 to p.DBONames.Count - 1 do begin
        case k of
          0: PrivUpdates.Add(mask('Db') + '=' + esc(p.DBONames[k]));
          1: PrivUpdates.Add(mask('Table_name') + '=' + esc(p.DBONames[k]));
          2: PrivUpdates.Add(mask('Column_name') + '=' + esc(p.DBONames[k]));
        end;
      end;
      PrivWhere := Delim(PrivUpdates);
      // Assemble list of key fields for new privilege definition.
      PrivUpdates.Clear;
      PrivValues.Clear;
      // Assemble values of new privilege definition.
      for k := 0 to p.SelectedPrivNames.Count - 1 do begin
        if TableSet.FindField(p.SelectedPrivNames[k] + '_priv') <> nil then begin
          // There's an ENUM field matching the privilege name.
          PrivUpdates.Add(mask(p.SelectedPrivNames[k] + '_priv') + '=' + esc('Y'));
        end else
          // It must be part of a SET field, then.
          PrivValues.Add(p.SelectedPrivNames[k]);
      end;
      // Insert new privilege definition.
      sql := 'INSERT INTO ' + db + '.' + TableName;
      if SetFieldName <> '' then
        PrivUpdates.Add(mask(SetFieldName) + '=' + esc(Delim(PrivValues, False)));
      sql := sql + ' SET ' + PrivWhere + ', ' + Delim(PrivUpdates);
      // Special case: UPDATE instead of INSERT for server-level privileges (see further above).
      if (p.DBOType = NODETYPE_DEFAULT) then begin
        // Server barfs if we do not set missing defaults, sigh.
        if dsUser.FindField('ssl_cipher') <> nil then begin
          PrivValues.Clear;
          PrivValues.Add(mask('ssl_cipher') + '=' + esc(''));
          PrivValues.Add(mask('x509_issuer') + '=' + esc(''));
          PrivValues.Add(mask('x509_subject') + '=' + esc(''));
          sql := sql + ', ' + Delim(PrivValues);
        end;
        sql := sql + ' ON DUPLICATE KEY UPDATE';
        sql := sql + ' ' + Delim(PrivUpdates);
      end;
      Exec(sql);
      // Special case: update redundant column privileges in mysql.tables_priv.
      if (p.DBOType = NODETYPE_COLUMN) and (dsTables.FindField('column_priv') <> nil) then begin
        // We need to deduce a completely new key because column_priv in mysql.tables_priv does not have a column field next to it, sigh.
        PrivUpdates.Clear;
        PrivUpdates.Add(mask('Host') + '=' + esc(u.Host));
        PrivUpdates.Add(mask('User') + '=' + esc(u.Name));
        for k := 0 to p.DBONames.Count - 1 do begin
          case k of
            0: PrivUpdates.Add(mask('Db') + '=' + esc(p.DBONames[k]));
            1: PrivUpdates.Add(mask('Table_name') + '=' + esc(p.DBONames[k]));
            // (2: Special case, do nothing.)
          end;
        end;
        PrivWhere := Delim(PrivUpdates);
        sql := 'INSERT INTO ' + db + '.' + PRIVTABLE_TABLES;
        sql := sql + ' SET ' + PrivWhere;
        sql := sql + ', ' + mask('column_priv') + '=' + esc(Delim(PrivValues, False));
        sql := sql + ' ON DUPLICATE KEY UPDATE';
        sql := sql + ' ' + mask('column_priv') + '=' + esc(Delim(PrivValues, False));
        Exec(sql);
      end;
    end;
  end;
  Exec('FLUSH PRIVILEGES');
end;


{ *** TUsers *** }

constructor TUsers.Create;
var
  u: TUser;
  i: Integer;
  user, host: String;
begin
  dsUser := CWin.GetResults('SELECT * FROM '+db+'.'+Mainform.Mask(PRIVTABLE_USERS) + ' ORDER BY '
    + Mainform.Mask('User')+', '
    + Mainform.Mask('Host'));
  dsDb := CWin.GetResults('SELECT * FROM '+db+'.'+Mainform.Mask(PRIVTABLE_DB)
    // Ignore db entries that contain magic pointers to the mysql.host table.
    + ' WHERE Db <> '#39#39
    + ' ORDER BY '
    + Mainform.Mask('User')+', '
    + Mainform.Mask('Host')+', '
    + Mainform.Mask('Db'));
  dsTables := CWin.GetResults('SELECT * FROM '+db+'.'+Mainform.Mask(PRIVTABLE_TABLES) + ' ORDER BY '
    + Mainform.Mask('User')+', '
    + Mainform.Mask('Host')+', '
    + Mainform.Mask('Db')+', '
    + Mainform.Mask('Table_name'));
  dsColumns := CWin.GetResults('SELECT * FROM '+db+'.'+Mainform.Mask(PRIVTABLE_COLUMNS) + ' ORDER BY '
    + Mainform.Mask('User')+', '
    + Mainform.Mask('Host')+', '
    + Mainform.Mask('Db')+', '
    + Mainform.Mask('Table_name')+', '
    + Mainform.Mask('Column_name'));
  dsTablesFields := CWin.GetResults('SHOW FIELDS FROM '+db+'.tables_priv LIKE ''%\_priv''');
  dsColumnsFields := CWin.GetResults('SHOW FIELDS FROM '+db+'.columns_priv LIKE ''%\_priv''');
  for i := 1 to dsUser.RecordCount do begin
    // Avoid using dsUser.Next and dsUser.Eof here because TUser.Create
    // also iterates through the global dsUser result and moves the cursor
    dsUser.RecNo := i;
    u := TUser.Create(dsUser.Fields);
    AddUser(u);
  end;
  // Find orphaned privileges in mysql.db.
  for i := 1 to dsDb.RecordCount do begin
    dsDb.RecNo := i;
    user := dsDb.Fields.FieldByName('User').AsString;
    host := dsDb.Fields.FieldByName('Host').AsString;
    if FindUser(user, host) = nil then AddUser(TUser.Create(user, host));
  end;
  // Find orphaned privileges in mysql.tables_priv.
  for i := 1 to dsTables.RecordCount do begin
    dsTables.RecNo := i;
    user := dsTables.Fields.FieldByName('User').AsString;
    host := dsTables.Fields.FieldByName('Host').AsString;
    if FindUser(user, host) = nil then AddUser(TUser.Create(user, host));
  end;
  // Find orphaned privileges in mysql.columns_priv.
  for i := 1 to dsColumns.RecordCount do begin
    dsColumns.RecNo := i;
    user := dsColumns.Fields.FieldByName('User').AsString;
    host := dsColumns.Fields.FieldByName('Host').AsString;
    if FindUser(user, host) = nil then AddUser(TUser.Create(user, host));
  end;
end;

procedure TUsers.AddUser(User: TUser);
begin
  SetLength(FUserItems, Length(FUserItems)+1);
  FUserItems[Length(FUserItems)-1] := User;
end;

function TUsers.GetUser(Index: Integer): TUser;
begin
  Result := FUserItems[Index];
end;

function TUsers.GetCount: Integer;
begin
  Result := Length(FUserItems);
end;

procedure TUsers.DeleteUser(Index: Integer);
var
  i: Integer;
begin
  if FUserItems[Index].Added then begin
    // This user was created only in memory, not yet on the server.
    // Erase this item out of UserItems, not only display the deleted icon
    for i := Index+1 to Length(FUserItems) - 1 do
      FUserItems[i-1] := FUserItems[i];
    SetLength(FUserItems, Length(FUserItems)-1);
  end else
    FUserItems[Index].Deleted := True
end;

function TUsers.FindUser(Name: String; Host: String): TUser;
var
  i: Integer;
begin
  Result := nil;
  // Host='' is canonicalized to Host='%' on load, so search for that instead.
  if Host = '' then Host := '%';
  for i := 0 to Length(FUserItems) - 1 do begin
    if (FUserItems[i].Name = Name) and (FUserItems[i].Host = Host) then begin
      Result := FUserItems[i];
      Exit;
    end;
  end;
end;


{ *** TUser *** }

constructor TUser.Create(Name: String; Host: String);
begin
  FAdded := True;
  FDeleted := False;
  FOtherModified := False;
  FPasswordModified := False;
  FDisabled := True;
  FOldPasswordHashed := '';
  FNameChanged := False;
  FOldName := Name;
  FOldHost := Host;
  if FOldHost = '' then begin
    // Get rid of duplicate entries.
    // Todo: handle collisions.
    FNewHost := '%';
    FOtherModified := True;
  end;
  FPassword := '';
  FPrivileges := TPrivileges.Create(Self);
end;

constructor TUser.Create(Fields: TFields);
begin
  // Loading an existing user
  FAdded := False;
  FDeleted := False;
  FOtherModified := False;
  FPasswordModified := False;
  FDisabled := False;
  FNameChanged := False;
  FOldName := Fields.FieldByName('User').AsString;
  FOldHost := Fields.FieldByName('Host').AsString;
  if FOldHost = '' then begin
    // Get rid of duplicate entries.
    // Todo: handle collisions.
    FNewHost := '%';
    FOtherModified := True;
  end;
  FPassword := '';
  FOldPasswordHashed := Fields.FieldByName('Password').AsString;
  FOldMaxQuestions := 0;
  if Fields.FindField('max_questions') <> nil then
    FOldMaxQuestions := MakeInt(Fields.FieldByName('max_questions').AsString);
  FMaxQuestions := FOldMaxQuestions;
  FOldMaxUpdates := 0;
  if Fields.FindField('max_updates') <> nil then
    FOldMaxUpdates := MakeInt(Fields.FieldByName('max_updates').AsString);
  FMaxUpdates := FOldMaxUpdates;
  FOldMaxConnections := 0;
  if Fields.FindField('max_connections') <> nil then
    FOldMaxConnections := MakeInt(Fields.FieldByName('max_connections').AsString);
  FMaxConnections := FOldMaxConnections;
  FOldMaxUserConnections := 0;
  if Fields.FindField('max_user_connections') <> nil then
    FOldMaxUserConnections := MakeInt(Fields.FieldByName('max_user_connections').AsString);
  FMaxUserConnections := FOldMaxUserConnections;

  FPrivileges := TPrivileges.Create(Self);
end;

function TUser.GetModified: Boolean;
begin
  Result := FPasswordModified or FOtherModified;
end;

function TUser.GetName: String;
begin
  Result := FOldName;
  if FNameChanged then Result := FNewName;
end;

procedure TUser.SetName(str: String);
begin
  FOtherModified := FOtherModified or (Name <> str);
  FNameChanged := True;
  FNewName := str;
end;

function TUser.GetHost: String;
begin
  Result := FOldHost;
  if FNewHost <> '' then
    Result := FNewHost;
end;

procedure TUser.SetHost(str: String);
begin
  if str = '' then
    str := '%';
  FOtherModified := FOtherModified or (Host <> str);
  FNewHost := str;
end;

procedure TUser.SetPassword(str: String);
begin
  // If password was edited but hasn't changed (fx by a copy+paste action)
  if FOldPasswordHashed = str then begin
    FPasswordModified := false;
    FPassword := '';
  end else begin
    FPasswordModified := true;
    FPassword := str;
  end;
end;


{ *** TPrivileges *** }
constructor TPrivileges.Create(AOwner: TUser);
var
  p: TPrivilege;
  host, user: String;
  // Extract privilege objects from results of user, db, tables_priv + columns_priv
  procedure LoadPrivs(ds: TDataset; FieldDefs: TDataset = nil; AvoidFieldDefs: TDataset = nil; CropDbFieldDefs: TDataset = nil);
  var
    hasUserField : Boolean;
    simulateDb : Boolean;
  begin
    ds.First;
    // The priv table 'host' does not have a user field, use '%'.
    hasUserField := ds.FieldDefs.IndexOf('User') > -1;
    user := '%';
    // When cropping the priv table 'user' to load only db privileges, use '%' for db.
    simulateDb := cropDbFieldDefs <> nil;
    while not ds.Eof do begin
      if hasUserField then user := ds.FieldByName('User').AsString;
      host := ds.FieldByName('Host').AsString;
      // Canonicalize: Host='' and Host='%' means the same.
      if host = '' then host := '%';
      if (host = FOwner.FOldHost) and (user = FOwner.FOldName) then begin
        // Find existing privilege, or create + add new one.
        p := FindPrivilege(ds.Fields, simulateDb);
        if (p = nil) then begin
          p := AddPrivilege(ds.Fields, FieldDefs, AvoidFieldDefs, CropDbFieldDefs, simulateDb);
        end;
        // Merge grants from row into the privilege object.
        p.Merge(ds.Fields)
      end;
      ds.Next;
    end;
  end;
begin
  FOwner := AOwner;
  if AOwner.Added then begin
    // Create blanket "server privileges" and "all objects" items.
    AddPrivilege(dsUser.Fields, nil, dsDb);
    AddPrivilege(dsUser.Fields, nil, nil, dsDb, True);
  end;
  // Load server privileges from 'user' rows, avoiding db privileges.
  LoadPrivs(dsUser, nil, dsDb);
  // Load db privileges from 'user' rows, avoiding server privileges.
  LoadPrivs(dsUser, nil, nil, dsDb);
  // Load the rest of the privileges.
  LoadPrivs(dsDb);
  LoadPrivs(dsTables, dsTablesFields);
  LoadPrivs(dsColumns, dsColumnsFields);
end;

function TPrivileges.GetPrivilege(Index: Integer): TPrivilege;
begin
 Result := FPrivilegeItems[Index];
end;

function TPrivileges.GetCount: Integer;
begin
 Result := Length(FPrivilegeItems);
end;

function TPrivileges.FindPrivilege(Fields: TFields; SimulateDbField: Boolean): TPrivilege;
var
  i : Integer;
  DBOType: Byte;
  DBONames: TStringList;
begin
  Result := nil;
  GetPrivilegeRowKey(Fields, SimulateDbField, DBOType, DBONames);
  for i := 0 to Length(FPrivilegeItems) - 1 do begin
    if
      (FPrivilegeItems[i].DBOType = DBOType) and
      (DBONames.DelimitedText = FPrivilegeItems[i].DBONames.DelimitedText)
    then begin
      Result := FPrivilegeItems[i];
      Exit;
    end;
  end;
end;

function TPrivileges.AddPrivilege(Fields: TFields; FieldDefs: TDataset = nil; AvoidFieldDefs: TDataSet = nil; CropFieldDefs: TDataSet = nil; SimulateDbField: Boolean = False): TPrivilege;
begin
  Result := TPrivilege.Create(Fields, FieldDefs, AvoidFieldDefs, CropFieldDefs, SimulateDbField);
  SetLength(FPrivilegeItems, Length(FPrivilegeItems)+1);
  FPrivilegeItems[Length(FPrivilegeItems)-1] := Result;
  // Minimum default privs for a new user should be read only for everything, or?
  if FOwner.Added and (Result.FDBOType = NODETYPE_DB) and (Result.DBOKey = '%') then begin
    Result.SelectedPrivNames.Add('Select');
    Result.Modified := True;
  end;
end;

procedure TPrivileges.DeletePrivilege(Index: Integer);
var
  i: Integer;
begin
  if FPrivilegeItems[Index].Added then begin
    // This user was created only in memory, not yet on the server.
    // Erase this item out of UserItems, not only display the deleted icon
    for i := Index+1 to Length(FPrivilegeItems) - 1 do
      FPrivilegeItems[i-1] := FPrivilegeItems[i];
    SetLength(FPrivilegeItems, Length(FPrivilegeItems)-1);
  end else begin
    FPrivilegeItems[Index].Deleted := True;
    FOwner.Modified := True;
  end;
end;


{ *** TPrivilege *** }

constructor TPrivilege.Create(Fields: TFields; FieldDefs: TDataset = nil; AvoidFieldDefs: TDataSet = nil; CropFieldDefs: TDataSet = nil; SimulateDbField: Boolean = False);
var
  i: Integer;
  tables_col_ignore: Boolean;
  cropNames: TStringList;
  // Find possible values for the given SET column
  function GetSETValues(FieldName: String): TStringList;
  begin
    FieldDefs.First;
    Result := TStringList.Create;
    while not FieldDefs.Eof do begin
      if FieldDefs.FieldByName('Field').AsString = FieldName + '_priv' then begin
        Result.QuoteChar := '''';
        Result.DelimitedText := getEnumValues( FieldDefs.FieldByName('Type').AsString );
      end;
      FieldDefs.Next;
    end;
  end;
begin
  // Defaults
  FDeleted := False;
  FAdded := False;
  FModified := False;
  DBONames := TStringList.Create;
  PrivNames := TStringList.Create;
  SelectedPrivNames := TStringList.Create;
  // Find out what xxxx_priv privilege columns this server/version has.
  Fields.GetFieldNames(PrivNames);
  for i := PrivNames.Count - 1 downto 0 do begin
    if Length(PrivNames[i]) > 5 then begin
      if Copy(PrivNames[i], Length(PrivNames[i]) - 4, 5) = '_priv' then begin
        // Avoid duplicated db privileges in user table.
        if AvoidFieldDefs = nil then Continue;
        if AvoidFieldDefs.FieldDefs.IndexOf(PrivNames[i]) = -1 then Continue;
      end;
    end;
    PrivNames.Delete(i)
  end;
  if CropFieldDefs <> nil then begin
    cropNames := TStringList.Create;
    CropFieldDefs.Fields.GetFieldNames(cropNames);
    for i := PrivNames.Count - 1 downto 0 do begin
      if cropNames.IndexOf(PrivNames[i]) = -1 then PrivNames.Delete(i);
    end;
  end;
  // Find out what SET fields of tables_priv this server/version has.
  // Only load tables_priv.Table_priv
  i := PrivNames.IndexOf('Table_priv');
  if i > -1 then begin
    PrivNames.Delete(i);
    PrivNames.AddStrings(GetSETValues('Table'));
  end;
  tables_col_ignore := i > -1;
  // Find out what SET fields of columns_priv this server/version has.
  i := PrivNames.IndexOf('Column_priv');
  if i > -1 then begin
    PrivNames.Delete(i);
    if not tables_col_ignore then
      PrivNames.AddStrings(GetSETValues('Column'));
  end;
  // Snip "_priv" from ENUM field names.
  for i := PrivNames.Count - 1 downto 0 do begin
    if Length(PrivNames[i]) > 5 then begin
      if Copy(PrivNames[i], Length(PrivNames[i]) - 4, 5) = '_priv' then begin
        PrivNames[i] := Copy(PrivNames[i], 0, Length(PrivNames[i]) - 5);
      end;
    end;
  end;
  // Get unique identifier for this privilege row.
  GetPrivilegeRowKey(Fields, SimulateDbField, FDBOType, DBONames);
end;

procedure TPrivilege.Merge(Fields: TFields);
var
  i: Integer;
  tmp: TStringList;
  fieldNames: TStringList;
begin
  fieldNames := TStringList.Create;
  Fields.GetFieldNames(fieldNames);
  // Apply ENUM privileges, skipping any SET privileges.
  for i := PrivNames.Count - 1 downto 0 do begin
    if fieldNames.IndexOf(PrivNames[i] + '_priv') > -1 then begin
      if UpperCase(Fields.FieldByName(PrivNames[i] + '_priv').AsString) = 'Y' then begin
        SelectedPrivNames.Add(PrivNames[i]);
      end;
    end;
  end;
  // Parse SET field in column "tables_priv"
  i := fieldNames.IndexOf('Table_priv');
  if i > -1 then begin
    tmp := TStringList.Create;
    tmp.CommaText := Fields.FieldByName('Table_priv').AsString;
    SelectedPrivNames.AddStrings(tmp);
  end else begin
    // Parse SET field in column "columns_priv"
    i := fieldNames.IndexOf('Column_priv');
    if i > -1 then begin
      tmp := TStringList.Create;
      tmp.CommaText := Fields.FieldByName('Column_priv').AsString;
      SelectedPrivNames.AddStrings(tmp);
    end;
  end;
  FreeAndNil(tmp);
end;


function TPrivilege.GetDBOPrettyKey: String;
begin
  Result := '';
  case FDBOType of
    NODETYPE_DEFAULT:   Result := Result + 'Server privileges';
    NODETYPE_DB:        Result := Result + 'Database: ';
    NODETYPE_BASETABLE: Result := Result + 'Table: ';
    NODETYPE_COLUMN:    Result := Result + 'Column: ';
  end;
  Result := Result + GetDBOKey;
  // Special case "db=%"
  if (FDBOType = NODETYPE_DB) and (DBOKey = '%') then
    Result := 'All databases';
end;


function TPrivilege.GetDBOKey: String;
begin
  DBONames.Delimiter := '.';
  Result := DBONames.DelimitedText;
end;

function TPrivilege.GetPrettyPrivNames: TStringList;
var
  i: Integer;
  p: String;
begin
  Result := TStringList.Create;
  for i := 0 to PrivNames.Count - 1 do begin
    // Fetch original name
    p := PrivNames[i];
    if Pos('_priv', p) = Length(p)-4 then
      p := Copy(p, 0, Length(p)-4);
    p := StringReplace(p, '_', ' ', [rfReplaceAll]);
    p := Trim(p);
    Result.Add(p);
  end;
end;


procedure GetPrivilegeRowKey(Fields: TFields; SimulateDbField: Boolean; out DBOType: Byte; out DBONames: TStringList);
begin
  DBOType := NODETYPE_DEFAULT;
  DBONames := TStringList.Create;
  DBONames.Delimiter := '.';
  if SimulateDbField then begin
    DBOType := NODETYPE_DB;
    DBONames.Add('%');
  end;
  if Fields.FindField('Db') <> nil then begin
    DBOType := NODETYPE_DB;
    DBONames.Add(Fields.FieldByName('Db').AsString);
  end;
  if Fields.FindField('Table_name') <> nil then begin
    DBOType := NODETYPE_BASETABLE;
    DBONames.Add(Fields.FieldByName('Table_name').AsString);
  end;
  if Fields.FindField('Column_name') <> nil then begin
    DBOType := NODETYPE_COLUMN;
    DBONames.Add(Fields.FieldByName('Column_name').AsString);
  end;
end;

end.
