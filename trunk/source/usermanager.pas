unit usermanager;


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls,
  CheckLst, ExtCtrls, ToolWin,
  mysql_connection, helpers, VirtualTrees;

{$I const.inc}


type
  TPrivilege = class(TObject)
    private
      FDBOType: TListNodeType;
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
      constructor Create(Fields: TMySQLQuery; FieldDefs: TMySQLQuery=nil; AvoidFieldDefs: TMySQLQuery=nil; CropFieldDefs: TMySQLQuery=nil; SimulateDbField: Boolean = False);
      procedure Merge(Fields: TMySQLQuery);
      property DBOType: TListNodeType read FDBOType;
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
      function AddPrivilege(Fields: TMySQLQuery; FieldDefs: TMySQLQuery=nil; AvoidFieldDefs: TMySQLQuery=nil; CropFieldDefs: TMySQLQuery=nil; SimulateDbField: Boolean = False): TPrivilege;
      property Items[Index: Integer]: TPrivilege read GetPrivilege; default;
      property Count: Integer read GetCount;
      procedure DeletePrivilege(Index: Integer);
      function FindPrivilege(Fields: TMySQLQuery; SimulateDbField: Boolean): TPrivilege;
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
      constructor Create(Fields: TMySQLQuery=nil); overload;
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
    pnlTop: TPanel;
    pnlRight: TPanel;
    PageControlUser: TPageControl;
    tabSettings: TTabSheet;
    lblFromHost: TLabel;
    lblPassword: TLabel;
    lblUsername: TLabel;
    lblWarning: TLabel;
    lblHostHints: TLabel;
    editPassword: TEdit;
    editFromHost: TEdit;
    editUsername: TEdit;
    chkDisabled: TCheckBox;
    tabLimitations: TTabSheet;
    lblMaxQuestions: TLabel;
    lblMaxUpdates: TLabel;
    lblMaxConnections: TLabel;
    lblMaxUserConnections: TLabel;
    lblLimitHint: TLabel;
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
    lblDescription: TLabel;
    lblEmail: TLabel;
    lblContactInfo: TLabel;
    editFullName: TEdit;
    editDescription: TEdit;
    editEmail: TEdit;
    memoContactInfo: TMemo;
    tabHints: TTabSheet;
    lblHints: TLabel;
    pnlLeft: TPanel;
    listUsers: TVirtualStringTree;
    tlbUsers: TToolBar;
    btnAddUser: TToolButton;
    btnDeleteUser: TToolButton;
    tlbObjects: TToolBar;
    btnAddObject: TToolButton;
    btnDeleteObject: TToolButton;
    treeObjects: TVirtualStringTree;
    Splitter1: TSplitter;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnAddObjectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddUserClick(Sender: TObject);
    procedure btnDeleteObjectClick(Sender: TObject);
    procedure btnDeleteUserClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure chkDisabledClick(Sender: TObject);
    procedure editFromHostChange(Sender: TObject);
    procedure editLimitations(Sender: TObject);
    procedure editPasswordExit(Sender: TObject);
    procedure editUsernameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure listUsersGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure listUsersGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure listUsersBeforePaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas);
    procedure treeObjectsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure treeObjectsBeforePaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas);
    procedure listUsersFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure treeObjectsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure treeObjectsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure treeObjectsInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure treeObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure treeObjectsExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure treeObjectsFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
  private
    { Private declarations }
    Users: TUsers;
    procedure RefreshGUI;
  public
    function TestUserAdmin: Boolean;
    { Public declarations }
  end;

procedure GetPrivilegeRowKey(Fields: TMySQLQuery; SimulateDbField: Boolean; out DBOType: TListNodeType; out DBONames: TStringList);

implementation


uses
  main, selectdbobject;


var
  db: String;
  // Results from SELECT * FROM user/db/...
  dsUser, dsDb, dsTables, dsColumns,
  // Results from SHOW FIELDS FROM user/db/...
  dsTablesFields, dsColumnsFields : TMySQLQuery;


{$R *.DFM}



{**
  FormCreate: Restore GUI setup
}
procedure TUserManagerForm.FormCreate(Sender: TObject);
begin
  Width := GetRegValue(REGNAME_USERMNGR_WINWIDTH, Width);
  Height := GetRegValue(REGNAME_USERMNGR_WINHEIGHT, Height);
  pnlLeft.Width := GetRegValue(REGNAME_USERMNGR_LISTWIDTH, pnlLeft.Width);
  db := Mainform.Mask(DBNAME_MYSQL);
  SetWindowSizeGrip( Self.Handle, True );
  InheritFont(Font);
  FixVT(listUsers);
  FixVT(treeObjects);
end;


{**
  FormDestroy: Save GUI setup
}
procedure TUserManagerForm.FormDestroy(Sender: TObject);
begin
  OpenRegistry;
  MainReg.WriteInteger( REGNAME_USERMNGR_WINWIDTH, Width );
  MainReg.WriteInteger( REGNAME_USERMNGR_WINHEIGHT, Height );
  MainReg.WriteInteger( REGNAME_USERMNGR_LISTWIDTH, pnlLeft.Width );
end;


{**
  Fail if no access to privilege database.
}
function TUserManagerForm.TestUserAdmin: Boolean;
var
  test_result: String;
begin
  // Test if we can access the privileges database and tables by
  // A. Using the mysql-DB
  try
    Mainform.Connection.Database := DBNAME_MYSQL;
  except
    MessageDlg('You have no access to the privileges database.', mtError, [mbOK], 0);
    Result := false;
    Exit;
  end;
  // B. retrieving a count of all users.
  test_result := Mainform.Connection.GetVar('SELECT COUNT(*) FROM '+db+'.'+Mainform.Mask(PRIVTABLE_USERS));
  if test_result = '' then begin
    MessageDlg('You have no access to the privileges tables.', mtError, [mbOK], 0);
    Result := false;
    Exit;
  end;
  Result := true;
end;


{**
  FormShow: Load users and privileges into memory
}
procedure TUserManagerForm.FormShow(Sender: TObject);
var
  snr: String;
begin
  // Set hints text
  snr := Mainform.Connection.GetVar('SHOW VARIABLES LIKE ' + esc('skip_name_resolve'));
  if snr = '' then snr := 'Unknown';
  lblHostHints.Caption := StringReplace(lblHostHints.Caption, '$SNR', snr, []);

  // Load users into memory
  try
    Users := TUsers.Create;
  except
    // At least one priv table is missing or non-accessible when we get an exception.
    // Proceeding would result in follow up errors, so cancel the whole dialog in that case.
    on E:Exception do begin
      MessageDlg(E.Message+CRLF+CRLF+'The user manager cannot proceed with this error.', mtError, [mbOK], 0);
      PostMessage(Handle, WM_CLOSE, 0, 0); 
      Exit;
    end;
  end;
  // Enable limitations editors only if relevant columns exist
  lblMaxQuestions.Enabled := dsUser.ColExists('max_questions');
  editMaxQuestions.Enabled := lblMaxQuestions.Enabled;
  udMaxQuestions.Enabled := lblMaxQuestions.Enabled;

  lblMaxUpdates.Enabled := dsUser.ColExists('max_updates');
  editMaxQuestions.Enabled := lblMaxUpdates.Enabled;
  udMaxUpdates.Enabled := lblMaxUpdates.Enabled;

  lblMaxConnections.Enabled := dsUser.ColExists('max_connections');
  editMaxConnections.Enabled := lblMaxConnections.Enabled;
  udMaxConnections.Enabled := lblMaxConnections.Enabled;

  lblMaxUserConnections.Enabled := dsUser.ColExists('max_user_connections');
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
  listUsers.Clear;
  listUsers.Repaint;
  listUsersFocusChanged(listUsers, listUsers.FocusedNode, listUsers.FocusedColumn);
  treeObjects.Repaint;
  treeObjectsFocusChanged(treeObjects, treeObjects.FocusedNode, treeObjects.FocusedColumn);
end;


procedure TUserManagerForm.listUsersBeforePaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas);
begin
  (Sender as TVirtualStringTree).RootNodeCount := Users.Count;
end;


procedure TUserManagerForm.listUsersGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  u: TUser;
begin
  if not (Kind in [ikNormal, ikSelected]) then Exit;
  u := Users[Node.Index];
  // Detect modified status of the user object itself
  if u.Name = '' then begin
    if u.Deleted then ImageIndex := 86
    else if u.Added then ImageIndex := 85
    else if u.Modified then ImageIndex := 84
    else ImageIndex := 11;
  end else begin
    if u.Deleted then ImageIndex := 83
    else if u.Added then ImageIndex := 21
    else if u.Modified then ImageIndex := 12
    else ImageIndex := 43;
  end;
end;


procedure TUserManagerForm.listUsersGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  u: TUser;
begin
  // Compose displayed username
  u := Users[Node.Index];
  CellText := u.Name;
  if u.Name = '' then
    CellText := 'Everybody';
  if u.Host <> '%' then
    CellText := CellText + '@' + u.Host;
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
  PageControlUser.ActivePage := tabSettings;
  // Avoid duplicates.
  u := Users.FindUser(name, '%');
  if u <> nil then begin
    MessageDlg('User/host combination "'+name+'@%" already exists.'+CRLF+CRLF+'Please chose a different username.', mtError, [mbOK], 0);
    Exit;
  end;
  Users.AddUser(TUser.Create(name, '%'));
  RefreshGUI;
  // Select newly added item.
  SelectNode(listUsers, listUsers.RootNodeCount - 1);
  // Focus the user name entry box.
  editUserName.SetFocus;
end;


{**
  Delete user
}
procedure TUserManagerForm.btnDeleteUserClick(Sender: TObject);
begin
  if MessageDlg('Delete user "'+listUsers.Text[listUsers.FocusedNode, listUsers.FocusedColumn]+'"?', mtConfirmation, [mbYes, mbCancel], 0 ) <> mrYes then
    Exit;
  Users.DeleteUser(listUsers.FocusedNode.Index);
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
  u := Users[listUsers.FocusedNode.Index];
  u.Disabled := disabled;
  // Reset password from "!" to empty string. Avoids again disabling it
  // by just leaving and saving the exclamation mark
  if u.Password = '!' then u.Password := '';
  u.Modified := True;
  listUsers.Invalidate;
end;


procedure TUserManagerForm.btnAddObjectClick(Sender: TObject);
var
  NewObj: TStringList;
  ds, FieldDefs: TMySQLQuery;
  NewPriv: TPrivilege;
  u: TUser;
  i: Integer;
begin
  NewObj := SelectDBO;
  if NewObj <> nil then begin
    u := Users[listUsers.FocusedNode.Index];
    for i := 0 to u.Privileges.Count - 1 do begin
      NewObj.Delimiter := u.Privileges[i].DBONames.Delimiter;
      if (u.Privileges[i].DBOKey = NewObj.DelimitedText) and (not u.Privileges[i].Deleted) then begin
        MessageDlg(NewObj.DelimitedText+' already exists.', mtError, [mbOK], 0);
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
    NewPriv := u.Privileges.AddPrivilege(ds, FieldDefs);
    NewPriv.Added := True;
    NewPriv.DBONames := NewObj;
    u.Modified := True;
    listUsers.Invalidate;
    treeObjects.ReinitChildren(treeObjects.RootNode, false);
    treeObjects.Repaint;
    SelectNode(treeObjects, treeObjects.GetLastChild(treeObjects.RootNode));
    treeObjects.Expanded[treeObjects.FocusedNode] := True;
  end;
end;


{**
  Revoke access to an object
}
procedure TUserManagerForm.btnDeleteObjectClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  case treeObjects.GetNodeLevel(treeObjects.FocusedNode) of
    0: Node := treeObjects.FocusedNode;
    1: Node := treeObjects.FocusedNode.Parent;
    else Raise Exception.Create(SUnhandledTreeLevel);
  end;
  Users[listUsers.FocusedNode.Index].Privileges.DeletePrivilege(Node.Index);
  treeObjects.ReInitNode(Node, True);
  treeObjects.Invalidate;
end;


{**
  Username edited
}
procedure TUserManagerForm.editUsernameChange(Sender: TObject);
var
  u, f: TUser;
  t: TNotifyEvent;
begin
  // User edit probably has to be reset to the previous value
  t := editUsername.OnChange;
  editUsername.OnChange := nil;
  // In case the user field contains '%', it's sort of a visual hoax.
  // The TUser.Name and mysql database contents of a match-all user is ''.
  // Allow entering '%' too, changing it to '' automatically.
  if editUsername.Text = '%' then editUsername.Text := '';
  u := Users[listUsers.FocusedNode.Index];
  // Check if user/host combination already exists
  f := Users.FindUser(editUsername.Text, u.Host);
  if (f = nil) or (f = u) then
    u.Name := editUsername.Text
  else
    MessageDlg('User/host combination "'+editUsername.Text+'@'+u.Host+'" already exists.'+CRLF+CRLF+'Please chose a different username.', mtError, [mbOK], 0);
  // Blank user field means:
  // - this user is used for authentication failures
  // - privileges for this user applies to everyone
  // In effect, User='' means the same as User='%', except
  // that the latter syntax is not allowed (in the database).
  if u.Name = '' then editUserName.Text := '%'
  else editUsername.Text := u.Name;
  editUsername.OnChange := t;
  listUsers.Invalidate;
end;


{**
  "From Host" edited
}
procedure TUserManagerForm.editFromHostChange(Sender: TObject);
var
  u, f: TUser;
  t: TNotifyEvent;
begin
  u := Users[listUsers.FocusedNode.Index];
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
  listUsers.Invalidate;
end;


{**
  Password field is unfocused: Apply change pw to user object
}
procedure TUserManagerForm.editPasswordExit(Sender: TObject);
var
  u: TUser;
  t: TNotifyEvent;
begin
  if not editPassword.Modified then
    Exit;
  u := Users[listUsers.FocusedNode.Index];
  u.Password := editPassword.Text;
  if u.PasswordModified then begin
    u.Disabled := False;
    t := chkDisabled.OnClick;
    chkDisabled.OnClick := nil;
    chkDisabled.Checked := False;
    chkDisabled.OnClick := t;
    editPassword.TextHint := '';
  end;
  // Show security warning for empty password if user is not disabled
  lblWarning.Visible := (not u.Disabled) and (
    (u.PasswordModified and (u.Password = '')) or
    ((not u.PasswordModified) and (u.OldPasswordHashed = ''))
    );
  lblWarning.Caption := 'This user has a blank password.';
  listUsers.Invalidate;
end;



procedure TUserManagerForm.editLimitations(Sender: TObject);
var
  u: TUser;
  isModified: Boolean;
begin
  // OnChange is called during form construction, avoid that.
  if not Assigned(listUsers.FocusedNode) then Exit;
  u := Users[listUsers.FocusedNode.Index];
  isModified :=
    Cardinal(u.MaxQuestions) +
    Cardinal(u.MaxUpdates) +
    Cardinal(u.MaxConnections) +
    Cardinal(u.MaxUserConnections) <>
    Cardinal(udMaxQuestions.Position) +
    Cardinal(udMaxUpdates.Position) +
    Cardinal(udMaxConnections.Position) +
    Cardinal(udMaxUserConnections.Position);
  u.MaxQuestions := udMaxQuestions.Position;
  u.MaxUpdates := udMaxUpdates.Position;
  u.MaxConnections := udMaxConnections.Position;
  u.MaxUserConnections := udMaxUserConnections.Position;
  if isModified then begin
    u.Modified := True;
    listUsers.Invalidate;
  end;
end;


procedure TUserManagerForm.treeObjectsBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  VT: TVirtualStringTree;
begin
  VT := Sender as TVirtualStringTree;
  if not Assigned(listUsers.FocusedNode) then
    VT.RootNodeCount := 0
  else
    VT.RootNodeCount := Users[listUsers.FocusedNode.Index].Privileges.Count;
end;


procedure TUserManagerForm.treeObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  u: TUser;
  p: TPrivilege;
  i: Integer;
begin
  case Sender.GetNodeLevel(Node) of
    0: Sender.Expanded[Node] := True;
    1: begin
      u := Users[listUsers.FocusedNode.Index];
      p := u.Privileges[Node.Parent.Index];
      case Node.CheckState of
        csCheckedNormal: p.SelectedPrivNames.Add(p.PrivNames[Node.Index]);
        csUncheckedNormal: begin
          i := p.SelectedPrivNames.IndexOf(p.PrivNames[Node.Index]);
          p.SelectedPrivNames.Delete(i);
        end;
      end;
      p.Modified := True;
      u.Modified := True;
      listUsers.Invalidate;
    end;
  end;
end;


procedure TUserManagerForm.treeObjectsExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  n: PVirtualNode;
begin
  // Collapse all uninvolved tree nodes, keeping the tree usable
  n := Sender.GetFirstChild(Node.Parent);
  while Assigned(n) do begin
    Sender.Expanded[n] := n = Node;
    n := Sender.GetNextSibling(n);
  end;
end;


procedure TUserManagerForm.treeObjectsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  EnableDelete: Boolean;
  i: Integer;
  priv: TPrivilege;
begin
  EnableDelete := False;
  if Assigned(Sender.FocusedNode) then begin
    case Sender.GetNodeLevel(Node) of
      0: i := Node.Index;
      1: i := Node.Parent.Index;
      else Raise Exception.Create(SUnhandledTreeLevel);
    end;
    priv := Users[listUsers.FocusedNode.Index].Privileges[i];
    EnableDelete := (priv.DBOType <> lntNone) and
      (priv.DBOKey <> '%') and (not priv.Deleted);
  end;
  if Assigned(listUsers.FocusedNode) then
    EnableDelete := EnableDelete and (not Users[listUsers.FocusedNode.Index].Deleted);
  btnDeleteObject.Enabled := EnableDelete;
end;


procedure TUserManagerForm.treeObjectsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  u: TUser;
begin
  if not (Kind in [ikNormal, ikSelected]) then Exit;
  case Sender.GetNodeLevel(Node) of
    0: begin
      u := Users[listUsers.FocusedNode.Index];
      case u.Privileges[Node.Index].DBOType of
        lntNone:      ImageIndex := ICONINDEX_SERVER;
        lntDb:        ImageIndex := ICONINDEX_DB;
        lntTable:     ImageIndex := ICONINDEX_TABLE;
        lntView:      ImageIndex := ICONINDEX_VIEW;
        lntColumn:    ImageIndex := ICONINDEX_FIELD;
      end;
      if u.Privileges[Node.Index].Deleted then
        ImageIndex := 46;
    end;
  end;
end;


procedure TUserManagerForm.treeObjectsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  u: TUser;
begin
  case Sender.GetNodeLevel(Node) of
    0: begin
      u := Users[listUsers.FocusedNode.Index];
      CellText := u.Privileges[Node.Index].DBOPrettyKey;
      if u.Privileges[Node.Index].Deleted then
        CellText := CellText + ' (deleted)';
    end;
    1: CellText := Users[listUsers.FocusedNode.Index].Privileges[Node.Parent.Index].PrivNames[Node.Index];
  end;
end;


procedure TUserManagerForm.treeObjectsInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  // Only first level nodes have child nodes
  case Sender.GetNodeLevel(Node) of
    0: ChildCount := Users[listUsers.FocusedNode.Index].Privileges[Node.Index].PrivNames.Count;
    1: ChildCount := 0;
  end;
end;


procedure TUserManagerForm.treeObjectsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p: TPrivilege;
begin
  Node.CheckType := ctTriStateCheckBox;
  case Sender.GetNodeLevel(Node) of
    0: begin
      // Display plus/minus button
      Include(InitialStates, ivsHasChildren);
      p := Users[listUsers.FocusedNode.Index].Privileges[Node.Index];
    end;
    1: begin
      p := Users[listUsers.FocusedNode.Index].Privileges[Node.Parent.Index];
      case p.SelectedPrivNames.IndexOf(p.PrivNames[Node.Index]) of
        -1: Node.CheckState := csUncheckedNormal;
        else Node.CheckState := csCheckedNormal;
      end;
    end;
    else Raise Exception.Create(SUnhandledTreeLevel);
  end;
  if p.Deleted then
    Include(Node.States, vsDisabled);
end;

procedure TUserManagerForm.listUsersFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  u: TUser;
  Enable: Boolean;
  t: TNotifyEvent;
begin
  lblWarning.Visible := False;
  Enable := False;
  if Assigned(Sender.FocusedNode) then begin
    u := Users[Sender.FocusedNode.Index];
    Enable := not u.Deleted;

    t := editUsername.OnChange;
    editUsername.OnChange := nil;
    editUsername.Text := u.Name;
    if u.Name = '' then editUsername.Text := '%';
    editUsername.OnChange := t;

    editPassword.Text := '';
    editPassword.TextHint := u.OldPasswordHashed;
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
  end;

  // Update top buttons
  btnDeleteUser.Enabled := Enable;
  // Update control in Settings tab
  lblUsername.Enabled := Enable;
  editUsername.Enabled := Enable;
  lblPassword.Enabled := Enable;
  editPassword.Enabled := Enable;
  editPassword.Modified := False;
  lblFromHost.Enabled := Enable;
  editFromHost.Enabled := Enable;
  lblHostHints.Enabled := Enable;
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
  btnAddObject.Enabled := Enable;
  treeObjects.Clear;
  treeObjects.Invalidate;
end;


{**
  OK clicked: Apply all changes
}
procedure TUserManagerForm.btnOKClick(Sender: TObject);
var
  i, j, k: Integer;
  c: Char;
  u: TUser;
  p: TPrivilege;
  sql, TableName, SetFieldName: String;
  TableSet: TMySQLQuery;
  AcctWhere, AcctValues, PrivWhere: String;
  AcctUpdates, PrivValues, PrivUpdates: TStringList;
  procedure LogSQL(sql: String);
  begin
    Mainform.LogSQL(sql);
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
    try
      Mainform.Connection.Query(sql + mask(PRIVTABLE_COLUMNS) + AcctWhere);
      Mainform.Connection.Query(sql + mask(PRIVTABLE_TABLES) + AcctWhere);
      Mainform.Connection.Query(sql + mask(PRIVTABLE_DB) + AcctWhere);
      Mainform.Connection.Query(sql + mask(PRIVTABLE_USERS) + AcctWhere);
    except
      on E:Exception do begin
        MessageDlg(E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
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
    try
      // Todo: Allow concurrency by skipping this account and removing from Users array if changing key in mysql.user fails.
      Mainform.Connection.Query(sql + mask(PRIVTABLE_USERS) + AcctValues + AcctWhere);
      Mainform.Connection.Query(sql + mask(PRIVTABLE_DB) + AcctValues + AcctWhere);
      Mainform.Connection.Query(sql + mask(PRIVTABLE_TABLES) + AcctValues + AcctWhere);
      Mainform.Connection.Query(sql + mask(PRIVTABLE_COLUMNS) + AcctValues + AcctWhere);
    except
      on E:Exception do begin
        MessageDlg(E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
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
    if dsUser.ColExists('max_questions') then
      if u.FOldMaxQuestions <> u.MaxQuestions then
        AcctUpdates.Add(mask('max_questions') + '=' + IntToStr(u.MaxQuestions));
    if dsUser.ColExists('max_updates') then
      if u.FOldMaxUpdates <> u.MaxUpdates then
        AcctUpdates.Add(mask('max_updates') + '=' + IntToStr(u.MaxUpdates));
    if dsUser.ColExists('max_connections') then
      if u.FOldMaxConnections <> u.MaxConnections then
        AcctUpdates.Add(mask('max_connections') + '=' + IntToStr(u.MaxConnections));
    if dsUser.ColExists('max_user_connections') then
      if u.FOldMaxUserConnections <> u.MaxUserConnections then
        AcctUpdates.Add(mask('max_user_connections') + '=' + IntToStr(u.MaxUserConnections));
    // Skip accounts with fx only username / host changes, they've already been processed.
    if AcctUpdates.Count = 0 then Continue;
    // Go.
    LogSQL('Updating account ' + u.Name + '@' + u.Host + '.');
    AcctWhere := AccountSqlClause(u.Name, u.Host);
    AcctValues := ' SET ' + Delim(AcctUpdates);
    sql := 'UPDATE ' + db + '.';
    try
      Mainform.Connection.Query(sql + mask(PRIVTABLE_USERS) + AcctValues + AcctWhere);
    except
      on E:Exception do begin
        MessageDlg(E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
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
    if dsUser.ColExists('max_questions') then
      AcctUpdates.Add(mask('max_questions') + '=' + IntToStr(u.MaxQuestions));
    if dsUser.ColExists('max_updates') then
      AcctUpdates.Add(mask('max_updates') + '=' + IntToStr(u.MaxUpdates));
    if dsUser.ColExists('max_connections') then
      AcctUpdates.Add(mask('max_connections') + '=' + IntToStr(u.MaxConnections));
    if dsUser.ColExists('max_user_connections') then
      AcctUpdates.Add(mask('max_user_connections') + '=' + IntToStr(u.MaxUserConnections));
    // Special case: work around missing default values (bug) in MySQL.
    if dsUser.ColExists('ssl_cipher') then
      AcctUpdates.Add(mask('ssl_cipher') + '=' + esc(''));
    if dsUser.ColExists('x509_issuer') then
      AcctUpdates.Add(mask('x509_issuer') + '=' + esc(''));
    if dsUser.ColExists('x509_subject') then
      AcctUpdates.Add(mask('x509_subject') + '=' + esc(''));
    sql := 'INSERT INTO ' + db + '.' + mask(PRIVTABLE_USERS);
    sql := sql + ' SET ' + Delim(AcctUpdates);
    try
      // Todo: Allow concurrency by skipping this account and removing from Users array if inserting key in mysql.user fails.
      Mainform.Connection.Query(sql);
    except
      on E:Exception do begin
        MessageDlg(E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
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
      if (not p.Modified) and (not p.Deleted) then Continue;
      // Go.
      LogSQL('Applying privilege to account ' + u.Name + '@' + u.Host + ' for ' + p.DBOPrettyKey + '.');
      case p.DBOType of
        lntNone: begin
          TableSet := dsUser;
          TableName := mask(PRIVTABLE_USERS);
          SetFieldName := '';
        end;
        lntDb: begin
          TableSet := dsDb;
          TableName := mask(PRIVTABLE_DB);
          SetFieldName := '';
        end;
        lntTable: begin
          TableSet := dsTables;
          TableName := mask(PRIVTABLE_TABLES);
          SetFieldName := 'table_priv';
        end;
        lntColumn: begin
          TableSet := dsColumns;
          TableName := mask(PRIVTABLE_COLUMNS);
          SetFieldName := 'column_priv';
        end;
        else begin
          raise Exception.Create('Processed privilege has an undefined db object type: ' + IntToStr(Integer(p.DBOType)));
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
      if (p.DBOType = lntDb) and (p.DBOKey = '%') then begin
        PrivUpdates.Clear;
        for k := 0 to p.PrivNames.Count - 1 do begin
          if dsUser.ColExists(p.PrivNames[k] + '_priv') then
            PrivUpdates.Add(mask(p.PrivNames[k] + '_priv') + '=' + esc('N'));
        end;
        sql := 'UPDATE ' + db + '.' + mask(PRIVTABLE_USERS);
        sql := sql + ' SET ' + Delim(PrivUpdates);
        try
          Mainform.Connection.Query(sql + AcctWhere);
        except
          on E:Exception do begin
            MessageDlg(E.Message, mtError, [mbOK], 0);
            Exit;
          end;
        end;
      end;
      // Remove old privilege definition.
      if (p.DBOType <> lntNone) then begin
        sql := 'DELETE FROM ' + db + '.' + TableName;
        try
          Mainform.Connection.Query(sql + AcctWhere + PrivWhere);
        except
          on E:Exception do begin
            MessageDlg(E.Message, mtError, [mbOK], 0);
            Exit;
          end;
        end;
        if p.Deleted then
          Continue;
      end else begin
        // Special case: avoid removing old definition when dealing with
        //               server-level privileges, since they're entangled with
        //               authentication details over in mysql.user.
        //               instead, we have to set them manually to 'N'.
        PrivUpdates.Clear;
        for k := 0 to p.PrivNames.Count - 1 do
          if TableSet.ColExists(p.PrivNames[k] + '_priv') then
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
      // Assemble list of key columns for new privilege definition.
      PrivUpdates.Clear;
      PrivValues.Clear;
      // Assemble values of new privilege definition.
      for k := 0 to p.PrivNames.Count - 1 do begin
        if p.SelectedPrivNames.IndexOf(p.PrivNames[k]) > -1 then c := 'Y' else c := 'N';
        if TableSet.ColExists(p.PrivNames[k] + '_priv') then begin
          // There's an ENUM field matching the privilege name.
          PrivUpdates.Add(mask(p.PrivNames[k] + '_priv') + '=' + esc(c));
        end else
          // It must be part of a SET field, then.
          if c = 'Y' then PrivValues.Add(p.PrivNames[k]);
      end;
      // Insert new privilege definition.
      sql := 'INSERT IGNORE INTO ' + db + '.' + TableName;
      if SetFieldName <> '' then
        PrivUpdates.Add(mask(SetFieldName) + '=' + esc(Delim(PrivValues, False)));
      sql := sql + ' SET ' + PrivWhere + ', ' + Delim(PrivUpdates);
      // Special case: UPDATE instead of INSERT for server-level privileges (see further above).
      if (p.DBOType = lntNone) then begin
        // Server barfs if we do not set missing defaults, sigh.
        PrivValues.Clear;
        if dsUser.ColExists('ssl_cipher') then
          PrivValues.Add(mask('ssl_cipher') + '=' + esc(''));
        if dsUser.ColExists('x509_issuer') then
          PrivValues.Add(mask('x509_issuer') + '=' + esc(''));
        if dsUser.ColExists('x509_subject') then
          PrivValues.Add(mask('x509_subject') + '=' + esc(''));
        if PrivValues.Count > 0 then
          sql := sql + ', ' + Delim(PrivValues);
      end;
      try
        Mainform.Connection.Query(sql);
        if Mainform.Connection.RowsAffected = 0 then
          Mainform.Connection.Query('UPDATE ' + db + '.' + TableName + ' SET ' +Delim(PrivUpdates) + AcctWhere);
      except
        on E:Exception do begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
      // Special case: update redundant column privileges in mysql.tables_priv.
      if (p.DBOType = lntColumn) and dsTables.ColExists('column_priv') then begin
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
        try
          Mainform.Connection.Query(sql);
        except
          on E:Exception do begin
            MessageDlg(E.Message, mtError, [mbOK], 0);
            Exit;
          end;
        end;
      end;
    end;
  end;
  try
    Mainform.Connection.Query('FLUSH PRIVILEGES');
    ModalResult := mrOK;
  except
    on E:Exception do begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
      Exit;
    end;
  end;
end;


{ *** TUsers *** }

constructor TUsers.Create;
var
  u: TUser;
  i: Integer;
  user, host: String;
begin
  dsUser := Mainform.Connection.GetResults('SELECT * FROM '+db+'.'+Mainform.Mask(PRIVTABLE_USERS) + ' ORDER BY '
    + Mainform.Mask('User')+', '
    + Mainform.Mask('Host'));
  dsDb := Mainform.Connection.GetResults('SELECT * FROM '+db+'.'+Mainform.Mask(PRIVTABLE_DB)
    // Ignore db entries that contain magic pointers to the mysql.host table.
    + ' WHERE Db <> '#39#39
    + ' ORDER BY '
    + Mainform.Mask('User')+', '
    + Mainform.Mask('Host')+', '
    + Mainform.Mask('Db'));
  dsTables := Mainform.Connection.GetResults('SELECT * FROM '+db+'.'+Mainform.Mask(PRIVTABLE_TABLES) + ' ORDER BY '
    + Mainform.Mask('User')+', '
    + Mainform.Mask('Host')+', '
    + Mainform.Mask('Db')+', '
    + Mainform.Mask('Table_name'));
  dsColumns := Mainform.Connection.GetResults('SELECT * FROM '+db+'.'+Mainform.Mask(PRIVTABLE_COLUMNS) + ' ORDER BY '
    + Mainform.Mask('User')+', '
    + Mainform.Mask('Host')+', '
    + Mainform.Mask('Db')+', '
    + Mainform.Mask('Table_name')+', '
    + Mainform.Mask('Column_name'));
  dsTablesFields := Mainform.Connection.GetResults('SHOW FIELDS FROM '+db+'.tables_priv LIKE ''%\_priv''');
  dsColumnsFields := Mainform.Connection.GetResults('SHOW FIELDS FROM '+db+'.columns_priv LIKE ''%\_priv''');
  for i:=0 to dsUser.RecordCount-1 do begin
    // Avoid using dsUser.Next and dsUser.Eof here because TUser.Create
    // also iterates through the global dsUser result and moves the cursor
    dsUser.RecNo := i;
    u := TUser.Create(dsUser);
    AddUser(u);
  end;
  // Find orphaned privileges in mysql.db.
  for i:=0 to dsDb.RecordCount-1 do begin
    dsDb.RecNo := i;
    user := dsDb.Col('User');
    host := dsDb.Col('Host');
    if FindUser(user, host) = nil then AddUser(TUser.Create(user, host));
  end;
  // Find orphaned privileges in mysql.tables_priv.
  for i:=0 to dsTables.RecordCount-1 do begin
    dsTables.RecNo := i;
    user := dsTables.Col('User');
    host := dsTables.Col('Host');
    if FindUser(user, host) = nil then AddUser(TUser.Create(user, host));
  end;
  // Find orphaned privileges in mysql.columns_priv.
  for i:=0 to dsColumns.RecordCount-1 do begin
    dsColumns.RecNo := i;
    user := dsColumns.Col('User');
    host := dsColumns.Col('Host');
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

constructor TUser.Create(Fields: TMySQLQuery);
begin
  // Loading an existing user
  FAdded := False;
  FDeleted := False;
  FOtherModified := False;
  FPasswordModified := False;
  FDisabled := False;
  FNameChanged := False;
  FOldName := Fields.Col('User', True);
  FOldHost := Fields.Col('Host', True);
  if FOldHost = '' then begin
    // Get rid of duplicate entries.
    // Todo: handle collisions.
    FNewHost := '%';
    FOtherModified := True;
  end;
  FPassword := '';
  FOldPasswordHashed := Fields.Col('Password', True);
  FOldMaxQuestions := 0;
  if Fields.ColExists('max_questions') then
    FOldMaxQuestions := MakeInt(Fields.Col('max_questions'));
  FMaxQuestions := FOldMaxQuestions;
  FOldMaxUpdates := 0;
  if Fields.ColExists('max_updates') then
    FOldMaxUpdates := MakeInt(Fields.Col('max_updates'));
  FMaxUpdates := FOldMaxUpdates;
  FOldMaxConnections := 0;
  if Fields.ColExists('max_connections') then
    FOldMaxConnections := MakeInt(Fields.Col('max_connections'));
  FMaxConnections := FOldMaxConnections;
  FOldMaxUserConnections := 0;
  if Fields.ColExists('max_user_connections') then
    FOldMaxUserConnections := MakeInt(Fields.Col('max_user_connections'));
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
  procedure LoadPrivs(ds: TMySQLQuery; FieldDefs: TMySQLQuery=nil; AvoidFieldDefs: TMySQLQuery=nil; CropDbFieldDefs: TMySQLQuery=nil);
  var
    hasUserField : Boolean;
    simulateDb : Boolean;
  begin
    ds.First;
    // The priv table 'host' does not have a user field, use '%'.
    hasUserField := ds.ColExists('User');
    user := '%';
    // When cropping the priv table 'user' to load only db privileges, use '%' for db.
    simulateDb := cropDbFieldDefs <> nil;
    while not ds.Eof do begin
      if hasUserField then user := ds.Col('User');
      host := ds.Col('Host');
      // Canonicalize: Host='' and Host='%' means the same.
      if host = '' then host := '%';
      if (host = FOwner.FOldHost) and (user = FOwner.FOldName) then begin
        // Find existing privilege, or create + add new one.
        p := FindPrivilege(ds, simulateDb);
        if (p = nil) then begin
          p := AddPrivilege(ds, FieldDefs, AvoidFieldDefs, CropDbFieldDefs, simulateDb);
        end;
        // Merge grants from row into the privilege object.
        p.Merge(ds)
      end;
      ds.Next;
    end;
  end;
begin
  FOwner := AOwner;
  if AOwner.Added then begin
    // Create blanket "server privileges" and "all objects" items.
    AddPrivilege(dsUser, nil, dsDb);
    AddPrivilege(dsUser, nil, nil, dsDb, True);
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

function TPrivileges.FindPrivilege(Fields: TMySQLQuery; SimulateDbField: Boolean): TPrivilege;
var
  i : Integer;
  DBOType: TListNodeType;
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

function TPrivileges.AddPrivilege(Fields: TMySQLQuery; FieldDefs: TMySQLQuery=nil; AvoidFieldDefs: TMySQLQuery=nil; CropFieldDefs: TMySQLQuery=nil; SimulateDbField: Boolean = False): TPrivilege;
begin
  Result := TPrivilege.Create(Fields, FieldDefs, AvoidFieldDefs, CropFieldDefs, SimulateDbField);
  SetLength(FPrivilegeItems, Length(FPrivilegeItems)+1);
  FPrivilegeItems[Length(FPrivilegeItems)-1] := Result;
  // Minimum default privs for a new user should be read only for everything, or?
  if FOwner.Added and (Result.FDBOType = lntDb) and (Result.DBOKey = '%') then begin
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

constructor TPrivilege.Create(Fields: TMySQLQuery; FieldDefs: TMySQLQuery=nil; AvoidFieldDefs: TMySQLQuery=nil; CropFieldDefs: TMySQLQuery=nil; SimulateDbField: Boolean = False);
var
  i: Integer;
  tables_col_ignore: Boolean;
  // Find possible values for the given SET column
  function GetSETValues(FieldName: String): TStringList;
  begin
    FieldDefs.First;
    Result := TStringList.Create;
    while not FieldDefs.Eof do begin
      if FieldDefs.Col('Field') = FieldName + '_priv' then begin
        Result.QuoteChar := '''';
        Result.DelimitedText := getEnumValues(FieldDefs.Col('Type'));
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
  PrivNames.Text := Fields.ColumnNames.Text;
  for i := PrivNames.Count - 1 downto 0 do begin
    if Length(PrivNames[i]) > 5 then begin
      if Copy(PrivNames[i], Length(PrivNames[i]) - 4, 5) = '_priv' then begin
        // Avoid duplicated db privileges in user table.
        if AvoidFieldDefs = nil then Continue;
        if AvoidFieldDefs.ColumnNames.IndexOf(PrivNames[i]) = -1 then Continue;
      end;
    end;
    PrivNames.Delete(i);
  end;
  if CropFieldDefs <> nil then begin
    for i := PrivNames.Count - 1 downto 0 do begin
      if CropFieldDefs.ColumnNames.IndexOf(PrivNames[i]) = -1 then PrivNames.Delete(i);
    end;
  end;
  // Find out what SET columns in tables_priv this server/version has.
  // Only load tables_priv.Table_priv
  i := PrivNames.IndexOf('Table_priv');
  if i > -1 then begin
    PrivNames.Delete(i);
    PrivNames.AddStrings(GetSETValues('Table'));
  end;
  tables_col_ignore := i > -1;
  // Find out what SET columns in columns_priv this server/version has.
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

procedure TPrivilege.Merge(Fields: TMySQLQuery);
var
  i: Integer;
  tmp: TStringList;
begin
  // Apply ENUM privileges, skipping any SET privileges.
  for i := PrivNames.Count - 1 downto 0 do begin
    if Fields.ColumnNames.IndexOf(PrivNames[i] + '_priv') > -1 then begin
      if UpperCase(Fields.Col(PrivNames[i] + '_priv')) = 'Y' then begin
        SelectedPrivNames.Add(PrivNames[i]);
      end;
    end;
  end;
  // Parse SET field in column "tables_priv"
  if Fields.ColExists('Table_priv') then begin
    tmp := TStringList.Create;
    tmp.CommaText := Fields.Col('Table_priv');
    SelectedPrivNames.AddStrings(tmp);
  end else begin
    // Parse SET field in column "columns_priv"
    if Fields.ColExists('Column_priv') then begin
      tmp := TStringList.Create;
      tmp.CommaText := Fields.Col('Column_priv');
      SelectedPrivNames.AddStrings(tmp);
    end;
  end;
  FreeAndNil(tmp);
end;


function TPrivilege.GetDBOPrettyKey: String;
begin
  Result := '';
  case FDBOType of
    lntNone:      Result := Result + 'Server privileges';
    lntDb:        Result := Result + 'Database: ';
    lntTable:     Result := Result + 'Table: ';
    lntColumn:    Result := Result + 'Column: ';
  end;
  Result := Result + GetDBOKey;
  // Special case "db=%"
  if (FDBOType = lntDb) and (DBOKey = '%') then
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
    p := StringReplace(p, '_', ' ', [rfReplaceAll]);
    p := Trim(p);
    Result.Add(p);
  end;
end;


procedure GetPrivilegeRowKey(Fields: TMySQLQuery; SimulateDbField: Boolean; out DBOType: TListNodeType; out DBONames: TStringList);
var
  RowsExist: Boolean;
begin
  DBOType := lntNone;
  DBONames := TStringList.Create;
  DBONames.Delimiter := '.';
  if SimulateDbField then begin
    DBOType := lntDb;
    DBONames.Add('%');
  end;
  RowsExist := Fields.RecordCount > 0;
  if Fields.ColExists('Db') then begin
    DBOType := lntDb;
    if RowsExist then
      DBONames.Add(Fields.Col('Db'));
  end;
  if Fields.ColExists('Table_name') then begin
    DBOType := lntTable;
    if RowsExist then
      DBONames.Add(Fields.Col('Table_name'));
  end;
  if Fields.ColExists('Column_name') then begin
    DBOType := lntColumn;
    if RowsExist then
      DBONames.Add(Fields.Col('Column_name'));
  end;
end;

end.
