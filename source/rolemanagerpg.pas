unit rolemanagerpg;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, EditBtn, ExtCtrls,
  ComCtrls, Buttons, Dialogs, Spin, Generics.Collections,
  laz.VirtualTrees, apphelpers, extra_controls, dbconnection, dbstructures,
  main;

{$I const.inc}

type
  TRoleItem = class(TObject)
    Name: String;
    CanLogin, SuperUser, CreateDB, CreateRole, InheritRights, Replication,
      BypassRLS: Boolean;
    ConnLimit: Integer;
    ValidUntil: String;
  end;

  TMembership = class(TObject)
    RoleName: String;
    IsMember, HasAdmin, OriginalIsMember, OriginalHasAdmin: Boolean;
  end;

  { TRoleManagerPgForm }

  TRoleManagerPgForm = class(TExtForm)
    btnAdd: TToolButton;
    btnClone: TToolButton;
    btnClose: TBitBtn;
    btnDelete: TToolButton;
    btnDiscard: TBitBtn;
    btnSave: TBitBtn;
    chkAdminOption: TCheckBox;
    chkClearPassword: TCheckBox;
    checkGroupAttributes: TCheckGroup;
    editConnLimit: TSpinEdit;
    editFilter: TEditButton;
    editName: TEdit;
    editPassword: TEdit;
    lblConnLimit: TLabel;
    lblMembership: TLabel;
    lblName: TLabel;
    lblPassword: TLabel;
    PageControl1: TPageControl;
    pnlBottom: TPanel;
    pnlLeft: TPanel;
    pnlMembership: TPanel;
    pnlMembershipFooter: TPanel;
    pnlRight: TPanel;
    splitterLeft: TSplitter;
    tabAttributes: TTabSheet;
    tabLogin: TTabSheet;
    toolbarRoles: TToolBar;
    treeMemberships: TLazVirtualStringTree;
    treeRoles: TLazVirtualStringTree;
    procedure btnAddClick(Sender: TObject);
    procedure btnCloneClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure checkGroupAttributesItemClick(Sender: TObject; Index: Integer);
    procedure chkAdminOptionChange(Sender: TObject);
    procedure editFilterButtonClick(Sender: TObject);
    procedure editFilterChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MarkModified(Sender: TObject);
    procedure treeMembershipsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure treeMembershipsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure treeMembershipsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure treeMembershipsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure treeRolesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure treeRolesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure treeRolesGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure treeRolesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    FConnection: TDBConnection;
    FRoles: TObjectList<TRoleItem>;
    FMemberships: TObjectList<TMembership>;
    FOwnedRole: TRoleItem;
    FRole: TRoleItem;
    FOriginalName: String;
    FIsNewRole: Boolean;
    FLoading: Boolean;
    FModified: Boolean;
    procedure LoadRoles;
    procedure LoadMemberships(const RoleName: String);
    procedure FillMembershipTree;
    procedure ApplyRoleToEditor;
    procedure ClearEditor;
    function SelectRole(Role: TRoleItem): Boolean;
    procedure UpdateButtons;
    function ConfirmDiscard: Boolean;
    function FindRole(const RoleName: String): TRoleItem;
    function FindRoleNode(const RoleName: String): PVirtualNode;
    function SelectedRoleItem: TRoleItem;
    function Sql(AId: TQueryId): String; overload;
    function Sql(AId: TQueryId; const Args: array of const): String; overload;
    function SqlBool(const TrueToken, FalseToken: String; Value: Boolean): String;
    function PgBool(Value: String): Boolean;
    procedure SaveRole;
    procedure SaveMemberships(const RoleName: String);
    procedure FocusNameEditor;
  public
  end;

implementation

{$R *.lfm}

const
  idxAttrLogin = 0;
  idxAttrSuperuser = 1;
  idxAttrCreateDB = 2;
  idxAttrCreateRole = 3;
  idxAttrInherit = 4;
  idxAttrReplication = 5;
  idxAttrBypassRLS = 6;

{ TRoleManagerPgForm }

procedure TRoleManagerPgForm.FormCreate(Sender: TObject);
begin
  FRoles := TObjectList<TRoleItem>.Create(False);
  FMemberships := TObjectList<TMembership>.Create(True);
  FLoading := False;
  FModified := False;
  ClearEditor;
end;

procedure TRoleManagerPgForm.FormDestroy(Sender: TObject);
var
  Role: TRoleItem;
begin
  for Role in FRoles do
    Role.Free;
  FRoles.Free;
  FMemberships.Free;
  FOwnedRole.Free;
end;

procedure TRoleManagerPgForm.FormShow(Sender: TObject);
begin
  FConnection := MainForm.ActiveConnection;
  if FConnection = nil then begin
    Close;
    Exit;
  end;
  checkGroupAttributes.CheckEnabled[idxAttrBypassRLS] := FConnection.ServerVersionInt >= 90500;
  LoadRoles;
end;

procedure TRoleManagerPgForm.ClearEditor;
begin
  FLoading := True;
  FRole := nil;
  FOwnedRole.Free;
  FOwnedRole := nil;
  FIsNewRole := False;
  FOriginalName := '';
  FModified := False;
  editName.Clear;
  editPassword.Clear;
  editConnLimit.Value := -1;
  chkClearPassword.Checked := False;
  checkGroupAttributes.Checked[idxAttrLogin] := False;
  checkGroupAttributes.Checked[idxAttrSuperuser] := False;
  checkGroupAttributes.Checked[idxAttrCreateDB] := False;
  checkGroupAttributes.Checked[idxAttrCreateRole] := False;
  checkGroupAttributes.Checked[idxAttrInherit] := False;
  checkGroupAttributes.Checked[idxAttrReplication] := False;
  checkGroupAttributes.Checked[idxAttrBypassRLS] := False;
  PageControl1.Enabled := False;
  pnlMembership.Enabled := False;
  FLoading := False;
  UpdateButtons;
end;

procedure TRoleManagerPgForm.LoadRoles;
var
  Res: TDBQuery;
  Role: TRoleItem;
  i: Integer;
begin
  FLoading := True;
  treeRoles.Clear;
  for Role in FRoles do
    Role.Free;
  FRoles.Clear;
  Res := FConnection.GetResults(Sql(qPgRoleAttributes));
  try
    for i := 0 to Res.RecordCount - 1 do begin
      Res.RecNo := i + 1;
      Role := TRoleItem.Create;
      Role.Name := Res.Col('rolname');
      Role.SuperUser := PgBool(Res.Col('rolsuper'));
      Role.InheritRights := PgBool(Res.Col('rolinherit'));
      Role.CreateRole := PgBool(Res.Col('rolcreaterole'));
      Role.CreateDB := PgBool(Res.Col('rolcreatedb'));
      Role.CanLogin := PgBool(Res.Col('rolcanlogin'));
      Role.Replication := PgBool(Res.Col('rolreplication'));
      Role.BypassRLS := PgBool(Res.Col('rolbypassrls')) and (FConnection.ServerVersionInt >= 90500);
      Role.ConnLimit := StrToIntDef(Res.Col('rolconnlimit'), -1);
      Role.ValidUntil := Res.Col('rolvaliduntil');
      FRoles.Add(Role);
      treeRoles.AddChild(nil, Role);
    end;
  finally
    Res.Free;
  end;
  FLoading := False;
end;

procedure TRoleManagerPgForm.LoadMemberships(const RoleName: String);
var
  Res: TDBQuery;
  Membership: TMembership;
  MemberOf: TStringList;
  AdminOf: TStringList;
  Role: TRoleItem;
  i: Integer;
begin
  FMemberships.Clear;
  MemberOf := TStringList.Create;
  MemberOf.Duplicates := dupIgnore;
  AdminOf := TStringList.Create;
  try
    if RoleName <> '' then begin
      Res := FConnection.GetResults(Sql(qPgRoleMemberships, [FConnection.EscapeString(RoleName)]));
      try
        for i := 0 to Res.RecordCount - 1 do begin
          Res.RecNo := i + 1;
          MemberOf.Add(Res.Col('rolname'));
          if PgBool(Res.Col('admin_option')) then
            AdminOf.Add(Res.Col('rolname'));
        end;
      finally
        Res.Free;
      end;
    end;
    for Role in FRoles do begin
      if SameText(Role.Name, RoleName) then
        Continue;
      Membership := TMembership.Create;
      Membership.RoleName := Role.Name;
      Membership.IsMember := MemberOf.IndexOf(Role.Name) > -1;
      Membership.HasAdmin := AdminOf.IndexOf(Role.Name) > -1;
      Membership.OriginalIsMember := Membership.IsMember;
      Membership.OriginalHasAdmin := Membership.HasAdmin;
      FMemberships.Add(Membership);
    end;
  finally
    AdminOf.Free;
    MemberOf.Free;
  end;
end;

procedure TRoleManagerPgForm.FillMembershipTree;
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
  chkAdminOption.Checked := False;
  chkAdminOption.Enabled := False;
  FLoading := False;
end;

procedure TRoleManagerPgForm.ApplyRoleToEditor;
begin
  FLoading := True;
  editName.Text := FRole.Name;
  checkGroupAttributes.Checked[idxAttrLogin] := FRole.CanLogin;
  checkGroupAttributes.Checked[idxAttrSuperuser] := FRole.SuperUser;
  checkGroupAttributes.Checked[idxAttrCreateDB] := FRole.CreateDB;
  checkGroupAttributes.Checked[idxAttrCreateRole] := FRole.CreateRole;
  checkGroupAttributes.Checked[idxAttrInherit] := FRole.InheritRights;
  checkGroupAttributes.Checked[idxAttrReplication] := FRole.Replication;
  checkGroupAttributes.Checked[idxAttrBypassRLS] := FRole.BypassRLS;
  editConnLimit.Value := FRole.ConnLimit;
  editPassword.Clear;
  chkClearPassword.Checked := False;
  PageControl1.Enabled := True;
  pnlMembership.Enabled := True;
  FModified := False;
  FLoading := False;
  UpdateButtons;
end;

function TRoleManagerPgForm.SelectRole(Role: TRoleItem): Boolean;
begin
  Result := False;
  if not ConfirmDiscard then
    Exit;
  Result := True;
  FRole := Role;
  FOwnedRole.Free;
  FOwnedRole := nil;
  FIsNewRole := False;
  FOriginalName := Role.Name;
  LoadMemberships(Role.Name);
  FillMembershipTree;
  ApplyRoleToEditor;
end;

procedure TRoleManagerPgForm.treeRolesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if FLoading or (Node = nil) then
    Exit;
  if not SelectRole(SelectedRoleItem) then begin
    // User declined to discard: move focus back to the role being edited
    FLoading := True;
    if FIsNewRole then
      treeRoles.FocusedNode := nil
    else
      treeRoles.FocusedNode := FindRoleNode(FOriginalName);
    FLoading := False;
  end;
  UpdateButtons;
end;

procedure TRoleManagerPgForm.treeRolesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(Pointer);
end;

procedure TRoleManagerPgForm.treeRolesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: ^TRoleItem;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    0: CellText := Data^.Name;
    1: if Data^.CanLogin then CellText := _('Yes') else CellText := '';
    2: if Data^.SuperUser then CellText := _('Yes') else CellText := '';
    3: CellText := Data^.ValidUntil;
    4: if Data^.ConnLimit = -1 then CellText := '' else CellText := IntToStr(Data^.ConnLimit);
    else CellText := '';
  end;
end;

procedure TRoleManagerPgForm.treeRolesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: ^TRoleItem;
begin
  if (Column <> 0) or not (Kind in [ikNormal, ikSelected]) then
    Exit;
  Data := Sender.GetNodeData(Node);
  if Data^.CanLogin then
    ImageIndex := ICONINDEX_USER
  else
    ImageIndex := ICONINDEX_ROLE;
end;

procedure TRoleManagerPgForm.treeMembershipsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: ^TMembership;
begin
  if FLoading or (Node = nil) then
    Exit;
  Data := Sender.GetNodeData(Node);
  Data^.IsMember := Node.CheckState in [csCheckedNormal, csCheckedPressed];
  MarkModified(Sender);
  chkAdminOption.Enabled := Data^.IsMember;
  if not Data^.IsMember then
    chkAdminOption.Checked := False
  else
    chkAdminOption.Checked := Data^.HasAdmin;
end;

procedure TRoleManagerPgForm.treeMembershipsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  Data: ^TMembership;
begin
  if FLoading or (Node = nil) then
    Exit;
  Data := Sender.GetNodeData(Node);
  chkAdminOption.Enabled := Data^.IsMember;
  chkAdminOption.Checked := Data^.HasAdmin;
end;

procedure TRoleManagerPgForm.treeMembershipsGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(Pointer);
end;

procedure TRoleManagerPgForm.treeMembershipsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: ^TMembership;
begin
  Data := Sender.GetNodeData(Node);
  CellText := Data^.RoleName;
end;

procedure TRoleManagerPgForm.checkGroupAttributesItemClick(Sender: TObject;
  Index: Integer);
begin
  MarkModified(Sender);
end;

procedure TRoleManagerPgForm.chkAdminOptionChange(Sender: TObject);
var
  Data: ^TMembership;
begin
  if FLoading or (treeMemberships.FocusedNode = nil) then
    Exit;
  Data := treeMemberships.GetNodeData(treeMemberships.FocusedNode);
  Data^.HasAdmin := chkAdminOption.Checked;
  MarkModified(Sender);
end;

procedure TRoleManagerPgForm.FocusNameEditor;
begin
  PageControl1.ActivePage := tabAttributes;
  if editName.CanFocus then
    editName.SetFocus;
end;

procedure TRoleManagerPgForm.btnAddClick(Sender: TObject);
begin
  if not ConfirmDiscard then
    Exit;
  FOwnedRole.Free;
  FOwnedRole := TRoleItem.Create;
  FRole := FOwnedRole;
  FRole.Name := '';
  FRole.CanLogin := True;
  FRole.InheritRights := True;
  FIsNewRole := True;
  FOriginalName := '';
  LoadMemberships('');
  FillMembershipTree;
  ApplyRoleToEditor;
  FocusNameEditor;
  FModified := True;
  UpdateButtons;
end;

procedure TRoleManagerPgForm.btnCloneClick(Sender: TObject);
var
  Membership: TMembership;
begin
  if FRole = nil then
    Exit;
  if not ConfirmDiscard then
    Exit;
  FOwnedRole.Free;
  FOwnedRole := TRoleItem.Create;
  FOwnedRole.Name := '';
  FOwnedRole.CanLogin := FRole.CanLogin;
  FOwnedRole.SuperUser := FRole.SuperUser;
  FOwnedRole.CreateDB := FRole.CreateDB;
  FOwnedRole.CreateRole := FRole.CreateRole;
  FOwnedRole.InheritRights := FRole.InheritRights;
  FOwnedRole.Replication := FRole.Replication;
  FOwnedRole.BypassRLS := FRole.BypassRLS;
  FOwnedRole.ConnLimit := FRole.ConnLimit;
  FRole := FOwnedRole;
  FIsNewRole := True;
  FOriginalName := '';
  for Membership in FMemberships do begin
    Membership.OriginalIsMember := False;
    Membership.OriginalHasAdmin := False;
  end;
  FillMembershipTree;
  ApplyRoleToEditor;
  FocusNameEditor;
  FModified := True;
  UpdateButtons;
end;

procedure TRoleManagerPgForm.btnDeleteClick(Sender: TObject);
var
  Role: TRoleItem;
begin
  Role := SelectedRoleItem;
  if Role = nil then
    Exit;
  if MessageDlg(_('Delete role'), f_('Really delete role "%s"?', [Role.Name]),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  try
    FConnection.Query(Sql(qPgRoleDrop, [FConnection.QuoteIdent(Role.Name)]));
  except
    on E: Exception do begin
      ErrorDialog(E.Message);
      Exit;
    end;
  end;
  if (FRole <> nil) and SameText(FRole.Name, Role.Name) then
    ClearEditor;
  LoadRoles;
end;

procedure TRoleManagerPgForm.btnSaveClick(Sender: TObject);
begin
  SaveRole;
end;

procedure TRoleManagerPgForm.btnDiscardClick(Sender: TObject);
begin
  if FIsNewRole then
    ClearEditor
  else if FRole <> nil then begin
    FRole := FindRole(FOriginalName);
    FIsNewRole := False;
    if FRole <> nil then begin
      FOriginalName := FRole.Name;
      LoadMemberships(FRole.Name);
      FillMembershipTree;
      ApplyRoleToEditor;
    end else
      ClearEditor;
  end else
    ClearEditor;
end;

procedure TRoleManagerPgForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TRoleManagerPgForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := ConfirmDiscard;
end;

procedure TRoleManagerPgForm.editFilterChange(Sender: TObject);
begin
  FilterNodesByEdit(editFilter, treeRoles);
end;

procedure TRoleManagerPgForm.editFilterButtonClick(Sender: TObject);
begin
  editFilter.Clear;
end;

procedure TRoleManagerPgForm.MarkModified(Sender: TObject);
begin
  if FLoading then
    Exit;
  FModified := True;
  UpdateButtons;
end;

procedure TRoleManagerPgForm.UpdateButtons;
begin
  btnSave.Enabled := FModified and (FRole <> nil);
  btnDiscard.Enabled := FModified and (FRole <> nil);
  btnDelete.Enabled := SelectedRoleItem <> nil;
  btnClone.Enabled := (FRole <> nil) and not FIsNewRole;
end;

function TRoleManagerPgForm.ConfirmDiscard: Boolean;
begin
  Result := True;
  if FModified then
    Result := MessageDlg(_('Unsaved changes'), _('Discard unsaved changes on role?'),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

function TRoleManagerPgForm.FindRole(const RoleName: String): TRoleItem;
var
  Role: TRoleItem;
begin
  Result := nil;
  for Role in FRoles do
    if SameText(Role.Name, RoleName) then
      Exit(Role);
end;

function TRoleManagerPgForm.FindRoleNode(const RoleName: String): PVirtualNode;
var
  Node: PVirtualNode;
  Data: ^TRoleItem;
begin
  Result := nil;
  Node := treeRoles.GetFirst;
  while Node <> nil do begin
    Data := treeRoles.GetNodeData(Node);
    if SameText(Data^.Name, RoleName) then
      Exit(Node);
    Node := treeRoles.GetNext(Node);
  end;
end;

function TRoleManagerPgForm.SelectedRoleItem: TRoleItem;
var
  Data: ^TRoleItem;
begin
  Result := nil;
  if treeRoles.FocusedNode = nil then
    Exit;
  Data := treeRoles.GetNodeData(treeRoles.FocusedNode);
  Result := Data^;
end;

function TRoleManagerPgForm.Sql(AId: TQueryId): String;
begin
  Result := FConnection.SqlProvider.GetSql(AId);
end;

function TRoleManagerPgForm.Sql(AId: TQueryId; const Args: array of const): String;
begin
  Result := FConnection.SqlProvider.GetSql(AId, Args);
end;

function TRoleManagerPgForm.SqlBool(const TrueToken, FalseToken: String;
  Value: Boolean): String;
begin
  if Value then
    Result := TrueToken
  else
    Result := FalseToken;
end;

function TRoleManagerPgForm.PgBool(Value: String): Boolean;
begin
  Result := (Value = 't') or (Value = 'true') or (Value = '1') or (Value = 'y');
end;

procedure TRoleManagerPgForm.SaveRole;
var
  RoleName, PasswordClause, BypassToken: String;
begin
  if FRole = nil then
    Exit;
  RoleName := Trim(editName.Text);
  if RoleName = '' then begin
    ErrorDialog(_('Please enter a role name.'));
    Exit;
  end;
  if (FIsNewRole or not SameText(RoleName, FOriginalName)) and (FindRole(RoleName) <> nil) then begin
    ErrorDialog(f_('A role named "%s" already exists.', [RoleName]));
    Exit;
  end;

  PasswordClause := '';
  if chkClearPassword.Checked then
    PasswordClause := 'PASSWORD NULL'
  else if editPassword.Text <> '' then
    PasswordClause := 'PASSWORD ' + FConnection.EscapeString(editPassword.Text);

  if FConnection.ServerVersionInt >= 90500 then
    BypassToken := SqlBool('BYPASSRLS', 'NOBYPASSRLS',
      checkGroupAttributes.Checked[idxAttrBypassRLS])
  else
    BypassToken := '';

  try
    if FIsNewRole then
      FConnection.Query(Sql(qPgRoleCreate, [
        FConnection.QuoteIdent(RoleName),
        SqlBool('LOGIN', 'NOLOGIN', checkGroupAttributes.Checked[idxAttrLogin]),
        SqlBool('SUPERUSER', 'NOSUPERUSER', checkGroupAttributes.Checked[idxAttrSuperuser]),
        SqlBool('CREATEDB', 'NOCREATEDB', checkGroupAttributes.Checked[idxAttrCreateDB]),
        SqlBool('CREATEROLE', 'NOCREATEROLE', checkGroupAttributes.Checked[idxAttrCreateRole]),
        SqlBool('INHERIT', 'NOINHERIT', checkGroupAttributes.Checked[idxAttrInherit]),
        SqlBool('REPLICATION', 'NOREPLICATION', checkGroupAttributes.Checked[idxAttrReplication]),
        BypassToken,
        editConnLimit.Value,
        PasswordClause
      ]))
    else begin
      if RoleName <> FOriginalName then
        FConnection.Query(Sql(qPgRoleRename, [
          FConnection.QuoteIdent(FOriginalName),
          FConnection.QuoteIdent(RoleName)
        ]));
      FConnection.Query(Sql(qPgRoleAlter, [
        FConnection.QuoteIdent(RoleName),
        SqlBool('LOGIN', 'NOLOGIN', checkGroupAttributes.Checked[idxAttrLogin]),
        SqlBool('SUPERUSER', 'NOSUPERUSER', checkGroupAttributes.Checked[idxAttrSuperuser]),
        SqlBool('CREATEDB', 'NOCREATEDB', checkGroupAttributes.Checked[idxAttrCreateDB]),
        SqlBool('CREATEROLE', 'NOCREATEROLE', checkGroupAttributes.Checked[idxAttrCreateRole]),
        SqlBool('INHERIT', 'NOINHERIT', checkGroupAttributes.Checked[idxAttrInherit]),
        SqlBool('REPLICATION', 'NOREPLICATION', checkGroupAttributes.Checked[idxAttrReplication]),
        BypassToken,
        editConnLimit.Value,
        PasswordClause
      ]));
    end;
    SaveMemberships(RoleName);
  except
    on E: Exception do begin
      ErrorDialog(E.Message);
      Exit;
    end;
  end;

  LoadRoles;
  FRole := FindRole(RoleName);
  FIsNewRole := False;
  if FRole <> nil then begin
    FOriginalName := RoleName;
    LoadMemberships(RoleName);
    FillMembershipTree;
    ApplyRoleToEditor;
    treeRoles.FocusedNode := FindRoleNode(RoleName);
  end else
    ClearEditor;
end;

procedure TRoleManagerPgForm.SaveMemberships(const RoleName: String);
var
  Membership: TMembership;
  AdminClause: String;
begin
  for Membership in FMemberships do begin
    if Membership.IsMember and not Membership.OriginalIsMember then begin
      if Membership.HasAdmin then
        AdminClause := ' WITH ADMIN OPTION'
      else
        AdminClause := '';
      FConnection.Query(Sql(qPgRoleMembershipGrant, [
        FConnection.QuoteIdent(Membership.RoleName),
        FConnection.QuoteIdent(RoleName),
        AdminClause
      ]));
    end
    else if not Membership.IsMember and Membership.OriginalIsMember then
      FConnection.Query(Sql(qPgRoleMembershipRevoke, [
        FConnection.QuoteIdent(Membership.RoleName),
        FConnection.QuoteIdent(RoleName)
      ]))
    else if Membership.IsMember and (Membership.HasAdmin <> Membership.OriginalHasAdmin) then begin
      FConnection.Query(Sql(qPgRoleMembershipRevoke, [
        FConnection.QuoteIdent(Membership.RoleName),
        FConnection.QuoteIdent(RoleName)
      ]));
      if Membership.HasAdmin then
        AdminClause := ' WITH ADMIN OPTION'
      else
        AdminClause := '';
      FConnection.Query(Sql(qPgRoleMembershipGrant, [
        FConnection.QuoteIdent(Membership.RoleName),
        FConnection.QuoteIdent(RoleName),
        AdminClause
      ]));
    end;
  end;
end;

end.
