unit rolemanagermssql;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, StdCtrls, EditBtn, ExtCtrls,
  ComCtrls, Buttons, Dialogs, Generics.Collections,
  laz.VirtualTrees, apphelpers, extra_controls, dbconnection, dbstructures,
  main;

{$I const.inc}

type
  TLoginItem = class(TObject)
    Name, TypeDesc, DefaultDatabase: String;
    IsSqlLogin, Disabled, PolicyChecked, ExpirationChecked: Boolean;
  end;

  TMembership = class(TObject)
    RoleName: String;
    IsMember, OriginalIsMember: Boolean;
  end;

  { TRoleManagerMssqlForm }

  TRoleManagerMssqlForm = class(TExtForm)
    btnAdd: TToolButton;
    btnClone: TToolButton;
    btnClose: TBitBtn;
    btnDelete: TToolButton;
    btnDiscard: TBitBtn;
    btnSave: TBitBtn;
    checkGroupAttributes: TCheckGroup;
    comboDefaultDatabase: TComboBox;
    editFilter: TEditButton;
    editName: TEdit;
    editPassword: TEdit;
    lblDefaultDatabase: TLabel;
    lblMembership: TLabel;
    lblName: TLabel;
    lblPassword: TLabel;
    PageControl1: TPageControl;
    pnlBottom: TPanel;
    pnlLeft: TPanel;
    pnlMembership: TPanel;
    pnlRight: TPanel;
    splitterLeft: TSplitter;
    tabAttributes: TTabSheet;
    tabLogin: TTabSheet;
    toolbarRoles: TToolBar;
    treeMemberships: TLazVirtualStringTree;
    treeLogins: TLazVirtualStringTree;
    procedure btnAddClick(Sender: TObject);
    procedure btnCloneClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure checkGroupAttributesItemClick(Sender: TObject; Index: Integer);
    procedure editFilterButtonClick(Sender: TObject);
    procedure editFilterChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MarkModified(Sender: TObject);
    procedure treeMembershipsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure treeMembershipsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure treeMembershipsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure treeLoginsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure treeLoginsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure treeLoginsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure treeLoginsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    FConnection: TDBConnection;
    FLogins: TObjectList<TLoginItem>;
    FServerRoles: TStringList;
    FMemberships: TObjectList<TMembership>;
    FOwnedLogin: TLoginItem;
    FLogin: TLoginItem;
    FOriginalName: String;
    FIsNewLogin: Boolean;
    FLoading: Boolean;
    FModified: Boolean;
    procedure LoadLogins;
    procedure LoadServerRoles;
    procedure LoadDatabases;
    procedure LoadMemberships(const LoginName: String);
    procedure FillMembershipTree;
    procedure ApplyLoginToEditor;
    procedure ClearEditor;
    function SelectLogin(Login: TLoginItem): Boolean;
    procedure UpdateButtons;
    function ConfirmDiscard: Boolean;
    function FindLogin(const LoginName: String): TLoginItem;
    function FindLoginNode(const LoginName: String): PVirtualNode;
    function SelectedLoginItem: TLoginItem;
    function Sql(AId: TQueryId): String; overload;
    function Sql(AId: TQueryId; const Args: array of const): String; overload;
    function BracketIdent(const Identifier: String): String;
    function MsBool(Value: String): Boolean;
    function LoginTypeText(TypeDesc: String): String;
    procedure SaveLogin;
    procedure SaveMemberships(const LoginName: String);
    procedure FocusNameEditor;
  public
  end;

implementation

{$R *.lfm}

const
  idxAttrEnabled = 0;
  idxAttrPolicy = 1;
  idxAttrExpiration = 2;

{ TRoleManagerMssqlForm }

procedure TRoleManagerMssqlForm.FormCreate(Sender: TObject);
begin
  FLogins := TObjectList<TLoginItem>.Create(False);
  FServerRoles := TStringList.Create;
  FServerRoles.CaseSensitive := False;
  FMemberships := TObjectList<TMembership>.Create(True);
  FLoading := False;
  FModified := False;
  ClearEditor;
end;

procedure TRoleManagerMssqlForm.FormDestroy(Sender: TObject);
var
  Login: TLoginItem;
begin
  for Login in FLogins do
    Login.Free;
  FLogins.Free;
  FServerRoles.Free;
  FMemberships.Free;
  FOwnedLogin.Free;
end;

procedure TRoleManagerMssqlForm.FormShow(Sender: TObject);
begin
  FConnection := MainForm.ActiveConnection;
  if FConnection = nil then begin
    Close;
    Exit;
  end;
  LoadServerRoles;
  LoadDatabases;
  LoadLogins;
end;

procedure TRoleManagerMssqlForm.ClearEditor;
begin
  FLoading := True;
  FLogin := nil;
  FOwnedLogin.Free;
  FOwnedLogin := nil;
  FIsNewLogin := False;
  FOriginalName := '';
  FModified := False;
  editName.Clear;
  editPassword.Clear;
  comboDefaultDatabase.Text := '';
  checkGroupAttributes.Checked[idxAttrEnabled] := False;
  checkGroupAttributes.Checked[idxAttrPolicy] := False;
  checkGroupAttributes.Checked[idxAttrExpiration] := False;
  checkGroupAttributes.CheckEnabled[idxAttrPolicy] := True;
  checkGroupAttributes.CheckEnabled[idxAttrExpiration] := True;
  editPassword.Enabled := True;
  lblPassword.Enabled := True;
  PageControl1.Enabled := False;
  pnlMembership.Enabled := False;
  FLoading := False;
  UpdateButtons;
end;

procedure TRoleManagerMssqlForm.LoadLogins;
var
  Res: TDBQuery;
  Login: TLoginItem;
  i: Integer;
begin
  FLoading := True;
  treeLogins.Clear;
  for Login in FLogins do
    Login.Free;
  FLogins.Clear;
  Res := FConnection.GetResults(Sql(qMssqlLoginAttributes));
  try
    for i := 0 to Res.RecordCount - 1 do begin
      Res.RecNo := i + 1;
      Login := TLoginItem.Create;
      Login.Name := Res.Col('name');
      Login.TypeDesc := Res.Col('type_desc');
      Login.IsSqlLogin := SameText(Login.TypeDesc, 'SQL_LOGIN');
      Login.Disabled := MsBool(Res.Col('is_disabled'));
      Login.DefaultDatabase := Res.Col('default_database_name');
      Login.PolicyChecked := MsBool(Res.Col('is_policy_checked'));
      Login.ExpirationChecked := MsBool(Res.Col('is_expiration_checked'));
      FLogins.Add(Login);
      treeLogins.AddChild(nil, Login);
    end;
  finally
    Res.Free;
  end;
  FLoading := False;
end;

procedure TRoleManagerMssqlForm.LoadServerRoles;
var
  Res: TDBQuery;
  i: Integer;
begin
  FServerRoles.Clear;
  Res := FConnection.GetResults(Sql(qMssqlServerRoles));
  try
    for i := 0 to Res.RecordCount - 1 do begin
      Res.RecNo := i + 1;
      // "public" is implicitly granted to every login and cannot be granted or revoked
      if not SameText(Res.Col('name'), 'public') then
        FServerRoles.Add(Res.Col('name'));
    end;
  finally
    Res.Free;
  end;
end;

procedure TRoleManagerMssqlForm.LoadDatabases;
var
  Res: TDBQuery;
  i: Integer;
begin
  comboDefaultDatabase.Items.Clear;
  Res := FConnection.GetResults(Sql(qMssqlDatabases));
  try
    for i := 0 to Res.RecordCount - 1 do begin
      Res.RecNo := i + 1;
      comboDefaultDatabase.Items.Add(Res.Col('name'));
    end;
  finally
    Res.Free;
  end;
end;

procedure TRoleManagerMssqlForm.LoadMemberships(const LoginName: String);
var
  Res: TDBQuery;
  Membership: TMembership;
  MemberOf: TStringList;
  RoleName: String;
  i: Integer;
begin
  FMemberships.Clear;
  MemberOf := TStringList.Create;
  MemberOf.CaseSensitive := False;
  try
    if LoginName <> '' then begin
      Res := FConnection.GetResults(Sql(qMssqlLoginMemberships, [FConnection.EscapeString(LoginName)]));
      try
        for i := 0 to Res.RecordCount - 1 do begin
          Res.RecNo := i + 1;
          MemberOf.Add(Res.Col('name'));
        end;
      finally
        Res.Free;
      end;
    end;
    for RoleName in FServerRoles do begin
      Membership := TMembership.Create;
      Membership.RoleName := RoleName;
      Membership.IsMember := MemberOf.IndexOf(RoleName) > -1;
      Membership.OriginalIsMember := Membership.IsMember;
      FMemberships.Add(Membership);
    end;
  finally
    MemberOf.Free;
  end;
end;

procedure TRoleManagerMssqlForm.FillMembershipTree;
var
  Node: PVirtualNode;
  Membership: TMembership;
begin
  FLoading := True;
  treeMemberships.Clear;
  for Membership in FMemberships do begin
    Node := treeMemberships.AddChild(nil, Membership);
    Node.CheckType := ctCheckBox;
    if Membership.IsMember then
      Node.CheckState := csCheckedNormal
    else
      Node.CheckState := csUncheckedNormal;
  end;
  FLoading := False;
end;

procedure TRoleManagerMssqlForm.ApplyLoginToEditor;
begin
  FLoading := True;
  editName.Text := FLogin.Name;
  checkGroupAttributes.Checked[idxAttrEnabled] := not FLogin.Disabled;
  checkGroupAttributes.CheckEnabled[idxAttrPolicy] := FLogin.IsSqlLogin;
  checkGroupAttributes.CheckEnabled[idxAttrExpiration] := FLogin.IsSqlLogin;
  checkGroupAttributes.Checked[idxAttrPolicy] := FLogin.PolicyChecked;
  checkGroupAttributes.Checked[idxAttrExpiration] := FLogin.ExpirationChecked;
  comboDefaultDatabase.Text := FLogin.DefaultDatabase;
  editPassword.Clear;
  editPassword.Enabled := FLogin.IsSqlLogin;
  lblPassword.Enabled := FLogin.IsSqlLogin;
  PageControl1.Enabled := True;
  pnlMembership.Enabled := True;
  FModified := False;
  FLoading := False;
  UpdateButtons;
end;

function TRoleManagerMssqlForm.SelectLogin(Login: TLoginItem): Boolean;
begin
  Result := False;
  if not ConfirmDiscard then
    Exit;
  Result := True;
  FLogin := Login;
  FOwnedLogin.Free;
  FOwnedLogin := nil;
  FIsNewLogin := False;
  FOriginalName := Login.Name;
  LoadMemberships(Login.Name);
  FillMembershipTree;
  ApplyLoginToEditor;
end;

procedure TRoleManagerMssqlForm.treeLoginsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if FLoading or (Node = nil) then
    Exit;
  if not SelectLogin(SelectedLoginItem) then begin
    // User declined to discard: move focus back to the login being edited
    FLoading := True;
    if FIsNewLogin then
      treeLogins.FocusedNode := nil
    else
      treeLogins.FocusedNode := FindLoginNode(FOriginalName);
    FLoading := False;
  end;
  UpdateButtons;
end;

procedure TRoleManagerMssqlForm.treeLoginsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(Pointer);
end;

procedure TRoleManagerMssqlForm.treeLoginsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: ^TLoginItem;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    0: CellText := Data^.Name;
    1: CellText := LoginTypeText(Data^.TypeDesc);
    2: if not Data^.Disabled then CellText := _('Yes') else CellText := '';
    3: CellText := Data^.DefaultDatabase;
    else CellText := '';
  end;
end;

procedure TRoleManagerMssqlForm.treeLoginsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if (Column <> 0) or not (Kind in [ikNormal, ikSelected]) then
    Exit;
  ImageIndex := ICONINDEX_USER;
end;

procedure TRoleManagerMssqlForm.treeMembershipsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: ^TMembership;
begin
  if FLoading or (Node = nil) then
    Exit;
  Data := Sender.GetNodeData(Node);
  Data^.IsMember := Node.CheckState in [csCheckedNormal, csCheckedPressed];
  MarkModified(Sender);
end;

procedure TRoleManagerMssqlForm.treeMembershipsGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(Pointer);
end;

procedure TRoleManagerMssqlForm.treeMembershipsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: ^TMembership;
begin
  Data := Sender.GetNodeData(Node);
  CellText := Data^.RoleName;
end;

procedure TRoleManagerMssqlForm.checkGroupAttributesItemClick(Sender: TObject;
  Index: Integer);
begin
  // CHECK_EXPIRATION requires CHECK_POLICY to be ON
  if (Index = idxAttrPolicy) and not checkGroupAttributes.Checked[idxAttrPolicy] then
    checkGroupAttributes.Checked[idxAttrExpiration] := False;
  MarkModified(Sender);
end;

procedure TRoleManagerMssqlForm.FocusNameEditor;
begin
  PageControl1.ActivePage := tabAttributes;
  if editName.CanFocus then
    editName.SetFocus;
end;

procedure TRoleManagerMssqlForm.btnAddClick(Sender: TObject);
begin
  if not ConfirmDiscard then
    Exit;
  FOwnedLogin.Free;
  FOwnedLogin := TLoginItem.Create;
  FLogin := FOwnedLogin;
  FLogin.Name := '';
  FLogin.TypeDesc := 'SQL_LOGIN';
  FLogin.IsSqlLogin := True;
  FLogin.Disabled := False;
  FLogin.PolicyChecked := True;
  FLogin.ExpirationChecked := False;
  FLogin.DefaultDatabase := 'master';
  FIsNewLogin := True;
  FOriginalName := '';
  LoadMemberships('');
  FillMembershipTree;
  ApplyLoginToEditor;
  FocusNameEditor;
  FModified := True;
  UpdateButtons;
end;

procedure TRoleManagerMssqlForm.btnCloneClick(Sender: TObject);
var
  Membership: TMembership;
begin
  if FLogin = nil then
    Exit;
  if not ConfirmDiscard then
    Exit;
  FOwnedLogin.Free;
  FOwnedLogin := TLoginItem.Create;
  FOwnedLogin.Name := '';
  FOwnedLogin.TypeDesc := FLogin.TypeDesc;
  FOwnedLogin.IsSqlLogin := FLogin.IsSqlLogin;
  FOwnedLogin.Disabled := FLogin.Disabled;
  FOwnedLogin.PolicyChecked := FLogin.PolicyChecked;
  FOwnedLogin.ExpirationChecked := FLogin.ExpirationChecked;
  FOwnedLogin.DefaultDatabase := FLogin.DefaultDatabase;
  FLogin := FOwnedLogin;
  FIsNewLogin := True;
  FOriginalName := '';
  for Membership in FMemberships do
    Membership.OriginalIsMember := False;
  FillMembershipTree;
  ApplyLoginToEditor;
  FocusNameEditor;
  FModified := True;
  UpdateButtons;
end;

procedure TRoleManagerMssqlForm.btnDeleteClick(Sender: TObject);
var
  Login: TLoginItem;
begin
  Login := SelectedLoginItem;
  if Login = nil then
    Exit;
  if MessageDlg(_('Delete login'), f_('Really delete login "%s"?', [Login.Name]),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  try
    FConnection.Query(Sql(qMssqlLoginDrop, [BracketIdent(Login.Name)]));
  except
    on E: Exception do begin
      ErrorDialog(E.Message);
      Exit;
    end;
  end;
  if (FLogin <> nil) and SameText(FLogin.Name, Login.Name) then
    ClearEditor;
  LoadLogins;
end;

procedure TRoleManagerMssqlForm.btnSaveClick(Sender: TObject);
begin
  SaveLogin;
end;

procedure TRoleManagerMssqlForm.btnDiscardClick(Sender: TObject);
begin
  if FIsNewLogin then
    ClearEditor
  else if FLogin <> nil then begin
    FLogin := FindLogin(FOriginalName);
    FIsNewLogin := False;
    if FLogin <> nil then begin
      FOriginalName := FLogin.Name;
      LoadMemberships(FLogin.Name);
      FillMembershipTree;
      ApplyLoginToEditor;
    end else
      ClearEditor;
  end else
    ClearEditor;
end;

procedure TRoleManagerMssqlForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TRoleManagerMssqlForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := ConfirmDiscard;
end;

procedure TRoleManagerMssqlForm.editFilterChange(Sender: TObject);
begin
  FilterNodesByEdit(editFilter, treeLogins);
end;

procedure TRoleManagerMssqlForm.editFilterButtonClick(Sender: TObject);
begin
  editFilter.Clear;
end;

procedure TRoleManagerMssqlForm.MarkModified(Sender: TObject);
begin
  if FLoading then
    Exit;
  FModified := True;
  UpdateButtons;
end;

procedure TRoleManagerMssqlForm.UpdateButtons;
begin
  btnSave.Enabled := FModified and (FLogin <> nil);
  btnDiscard.Enabled := FModified and (FLogin <> nil);
  btnDelete.Enabled := SelectedLoginItem <> nil;
  btnClone.Enabled := (FLogin <> nil) and not FIsNewLogin;
end;

function TRoleManagerMssqlForm.ConfirmDiscard: Boolean;
begin
  Result := True;
  if FModified then
    Result := MessageDlg(_('Unsaved changes'), _('Discard unsaved changes on login?'),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

function TRoleManagerMssqlForm.FindLogin(const LoginName: String): TLoginItem;
var
  Login: TLoginItem;
begin
  Result := nil;
  for Login in FLogins do
    if SameText(Login.Name, LoginName) then
      Exit(Login);
end;

function TRoleManagerMssqlForm.FindLoginNode(const LoginName: String): PVirtualNode;
var
  Node: PVirtualNode;
  Data: ^TLoginItem;
begin
  Result := nil;
  Node := treeLogins.GetFirst;
  while Node <> nil do begin
    Data := treeLogins.GetNodeData(Node);
    if SameText(Data^.Name, LoginName) then
      Exit(Node);
    Node := treeLogins.GetNext(Node);
  end;
end;

function TRoleManagerMssqlForm.SelectedLoginItem: TLoginItem;
var
  Data: ^TLoginItem;
begin
  Result := nil;
  if treeLogins.FocusedNode = nil then
    Exit;
  Data := treeLogins.GetNodeData(treeLogins.FocusedNode);
  Result := Data^;
end;

function TRoleManagerMssqlForm.Sql(AId: TQueryId): String;
begin
  Result := FConnection.SqlProvider.GetSql(AId);
end;

function TRoleManagerMssqlForm.Sql(AId: TQueryId; const Args: array of const): String;
begin
  Result := FConnection.SqlProvider.GetSql(AId, Args);
end;

// Login names in CREATE/ALTER LOGIN and ALTER SERVER ROLE are no quoted identifiers,
// they must be enclosed in square brackets instead, see issue #17
function TRoleManagerMssqlForm.BracketIdent(const Identifier: String): String;
begin
  Result := '[' + StringReplace(Identifier, ']', ']]', [rfReplaceAll]) + ']';
end;

function TRoleManagerMssqlForm.MsBool(Value: String): Boolean;
begin
  Value := LowerCase(Value);
  Result := (Value = '1') or (Value = 'true') or (Value = 't') or (Value = 'y');
end;

function TRoleManagerMssqlForm.LoginTypeText(TypeDesc: String): String;
begin
  if SameText(TypeDesc, 'SQL_LOGIN') then
    Result := 'SQL'
  else if SameText(TypeDesc, 'WINDOWS_LOGIN') then
    Result := _('Windows')
  else if SameText(TypeDesc, 'WINDOWS_GROUP') then
    Result := _('Windows group')
  else
    Result := TypeDesc;
end;

procedure TRoleManagerMssqlForm.SaveLogin;
var
  LoginName, Options, DefaultDbClause: String;
  PolicyChecked, ExpirationChecked: Boolean;
begin
  if FLogin = nil then
    Exit;
  LoginName := Trim(editName.Text);
  if LoginName = '' then begin
    ErrorDialog(_('Please enter a login name.'));
    Exit;
  end;
  if (FIsNewLogin or not SameText(LoginName, FOriginalName)) and (FindLogin(LoginName) <> nil) then begin
    ErrorDialog(f_('A login named "%s" already exists.', [LoginName]));
    Exit;
  end;

  // CHECK_EXPIRATION requires CHECK_POLICY to be ON
  PolicyChecked := checkGroupAttributes.Checked[idxAttrPolicy];
  ExpirationChecked := checkGroupAttributes.Checked[idxAttrExpiration] and PolicyChecked;

  try
    if FIsNewLogin then begin
      if editPassword.Text = '' then begin
        ErrorDialog(_('Please enter a password for the new login.'));
        Exit;
      end;
      DefaultDbClause := '';
      if Trim(comboDefaultDatabase.Text) <> '' then
        DefaultDbClause := ', DEFAULT_DATABASE = ' + BracketIdent(Trim(comboDefaultDatabase.Text));
      FConnection.Query(Sql(qMssqlLoginCreate, [
        BracketIdent(LoginName),
        FConnection.EscapeString(editPassword.Text),
        IfThen(PolicyChecked, 'ON', 'OFF'),
        IfThen(ExpirationChecked, 'ON', 'OFF'),
        DefaultDbClause
      ]));
    end else begin
      if LoginName <> FOriginalName then
        FConnection.Query(Sql(qMssqlLoginRename, [
          BracketIdent(FOriginalName),
          BracketIdent(LoginName)
        ]));
      Options := '';
      if FLogin.IsSqlLogin and (editPassword.Text <> '') then
        Options := 'PASSWORD = ' + FConnection.EscapeString(editPassword.Text);
      if not SameText(Trim(comboDefaultDatabase.Text), FLogin.DefaultDatabase) then begin
        if Options <> '' then
          Options := Options + ', ';
        Options := Options + 'DEFAULT_DATABASE = ' + BracketIdent(Trim(comboDefaultDatabase.Text));
      end;
      if FLogin.IsSqlLogin then begin
        if PolicyChecked <> FLogin.PolicyChecked then begin
          if Options <> '' then
            Options := Options + ', ';
          Options := Options + 'CHECK_POLICY = ' + IfThen(PolicyChecked, 'ON', 'OFF');
        end;
        if ExpirationChecked <> FLogin.ExpirationChecked then begin
          if Options <> '' then
            Options := Options + ', ';
          Options := Options + 'CHECK_EXPIRATION = ' + IfThen(ExpirationChecked, 'ON', 'OFF');
        end;
      end;
      if Options <> '' then
        FConnection.Query(Sql(qMssqlLoginAlter, [
          BracketIdent(LoginName),
          'WITH ' + Options
        ]));
      if (not FLogin.Disabled) <> checkGroupAttributes.Checked[idxAttrEnabled] then begin
        if checkGroupAttributes.Checked[idxAttrEnabled] then
          FConnection.Query(Sql(qMssqlLoginAlter, [BracketIdent(LoginName), 'ENABLE']))
        else
          FConnection.Query(Sql(qMssqlLoginAlter, [BracketIdent(LoginName), 'DISABLE']));
      end;
    end;
    SaveMemberships(LoginName);
  except
    on E: Exception do begin
      ErrorDialog(E.Message);
      Exit;
    end;
  end;

  LoadLogins;
  FLogin := FindLogin(LoginName);
  FIsNewLogin := False;
  if FLogin <> nil then begin
    FOriginalName := LoginName;
    LoadMemberships(LoginName);
    FillMembershipTree;
    ApplyLoginToEditor;
    treeLogins.FocusedNode := FindLoginNode(LoginName);
  end else
    ClearEditor;
end;

procedure TRoleManagerMssqlForm.SaveMemberships(const LoginName: String);
var
  Membership: TMembership;
begin
  for Membership in FMemberships do begin
    if Membership.IsMember and not Membership.OriginalIsMember then
      FConnection.Query(Sql(qMssqlRoleMembershipGrant, [
        BracketIdent(Membership.RoleName),
        BracketIdent(LoginName)
      ]))
    else if (not Membership.IsMember) and Membership.OriginalIsMember then
      FConnection.Query(Sql(qMssqlRoleMembershipRevoke, [
        BracketIdent(Membership.RoleName),
        BracketIdent(LoginName)
      ]));
  end;
end;

end.
