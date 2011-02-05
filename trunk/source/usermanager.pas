unit usermanager;


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, ToolWin, ClipBrd, Generics.Collections, Generics.Defaults, SynRegExpr,
  mysql_connection, helpers, VirtualTrees, Menus;

{$I const.inc}


type
  TUser = class(TObject)
    Username, Host, Password: String;
  end;
  PUser = ^TUser;
  TUserList = TObjectList<TUser>;

  TPrivObj = class(TObject)
    GrantCode: String;
    DBObj: TDBObject;
    OrgPrivs, AddedPrivs, DeletedPrivs: TStringList;
    AllPrivileges: TStringList;
    Added: Boolean;
    public
      constructor Create;
      destructor Destroy; override;
  end;
  TPrivObjList = TObjectList<TPrivObj>;
  TPrivComparer = class(TComparer<TPrivObj>)
    function Compare(const Left, Right: TPrivObj): Integer; override;
  end;

  EInputError = class(Exception);

  TUserManagerForm = class(TForm)
    btnCancel: TButton;
    btnSave: TButton;
    pnlLeft: TPanel;
    listUsers: TVirtualStringTree;
    Splitter1: TSplitter;
    pnlRight: TPanel;
    tlbObjects: TToolBar;
    btnAddObject: TToolButton;
    treePrivs: TVirtualStringTree;
    grpCredentials: TGroupBox;
    lblFromHost: TLabel;
    lblPassword: TLabel;
    lblUsername: TLabel;
    lblRepeatPassword: TLabel;
    editFromHost: TButtonedEdit;
    editUsername: TEdit;
    editPassword: TButtonedEdit;
    editRepeatPassword: TEdit;
    btnDiscard: TButton;
    lblUsers: TLabel;
    ToolBar1: TToolBar;
    btnAddUser: TToolButton;
    btnDeleteUser: TToolButton;
    btnCloneUser: TToolButton;
    lblWarning: TLabel;
    Label1: TLabel;
    menuHost: TPopupMenu;
    menuHost1: TMenuItem;
    menuHostLocal4: TMenuItem;
    menuHost2: TMenuItem;
    menuHost3: TMenuItem;
    N1: TMenuItem;
    menuPassword: TPopupMenu;
    menuPassword1: TMenuItem;
    menuPassword2: TMenuItem;
    menuPassword3: TMenuItem;
    menuPassword4: TMenuItem;
    menuPassword5: TMenuItem;
    menuDummy1: TMenuItem;
    menuDummy2: TMenuItem;
    menuDummy3: TMenuItem;
    menuDummy4: TMenuItem;
    menuDummy5: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAddUserClick(Sender: TObject);
    procedure btnDeleteUserClick(Sender: TObject);
    procedure listUsersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure listUsersBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure listUsersGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure listUsersInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure listUsersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure listUsersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure listUsersFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode;
      OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure btnSaveClick(Sender: TObject);
    procedure Modification(Sender: TObject);
    procedure treePrivsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure treePrivsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure treePrivsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure treePrivsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure treePrivsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure btnDiscardClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnAddObjectClick(Sender: TObject);
    procedure treePrivsExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure treePrivsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure listUsersHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure listUsersCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure listUsersAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure menuHostClick(Sender: TObject);
    procedure menuHostPopup(Sender: TObject);
    procedure menuPasswordClick(Sender: TObject);
    procedure menuPasswordInsert(Sender: TObject);
    procedure editPasswordChange(Sender: TObject);
    procedure listUsersHotChange(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode);
  private
    { Private declarations }
    FUsers: TUserList;
    FModified, FAdded: Boolean;
    CloneGrants: TStringList;
    FPrivObjects: TPrivObjList;
    PrivsGlobal, PrivsDb, PrivsTable, PrivsRoutine, PrivsColumn: TStringList;
    procedure SetModified(Value: Boolean);
    property Modified: Boolean read FModified write SetModified;
    function GetPrivByNode(Node: PVirtualNode): TPrivObj;
  public
    { Public declarations }
  end;

  function ComparePrivs(List: TStringList; Index1, Index2: Integer): Integer;

implementation

uses
  main, selectdbobject;
var
  PrivsRead, PrivsWrite, PrivsAdmin: TStringList;

{$R *.DFM}

function ComparePrivs(List: TStringList; Index1, Index2: Integer): Integer;
var
  s1, s2: String;
  s1val, s2val: Integer;
begin
  s1 := List[Index1];
  s2 := List[Index2];
  s1val := 0;
  s2val := 0;
  if PrivsRead.IndexOf(s1) > -1 then s1val := 1
  else if PrivsWrite.IndexOf(s1) > -1 then s1val := 2
  else if PrivsAdmin.IndexOf(s1) > -1 then s1val := 3;
  if PrivsRead.IndexOf(s2) > -1 then s2val := 1
  else if PrivsWrite.IndexOf(s2) > -1 then s2val := 2
  else if PrivsAdmin.IndexOf(s2) > -1 then s2val := 3;
  if s1val > s2val then
    Result := 1
  else if s1val = s2val then
    Result := CompareText(s1, s2)
  else
    Result := -1;
end;


procedure TUserManagerForm.FormCreate(Sender: TObject);
begin
  // Restore GUI setup
  InheritFont(Font);
  Width := GetRegValue(REGNAME_USERMNGR_WINWIDTH, Width);
  Height := GetRegValue(REGNAME_USERMNGR_WINHEIGHT, Height);
  pnlLeft.Width := GetRegValue(REGNAME_USERMNGR_LISTWIDTH, pnlLeft.Width);
  SetWindowSizeGrip( Self.Handle, True );
  FixVT(listUsers);
  FixVT(treePrivs);
  Mainform.RestoreListSetup(listUsers);
  PrivsRead := Explode(',', 'SELECT,SHOW VIEW,SHOW DATABASES,PROCESS,EXECUTE');
  PrivsWrite := Explode(',', 'ALTER,CREATE,DROP,DELETE,UPDATE,INSERT,ALTER ROUTINE,CREATE ROUTINE,CREATE TEMPORARY TABLES,CREATE VIEW,INDEX,TRIGGER,EVENT,REFERENCES');
  PrivsAdmin := Explode(',', 'RELOAD,SHUTDOWN,REPLICATION CLIENT,REPLICATION SLAVE,SUPER,LOCK TABLES,GRANT,FILE,CREATE USER');
end;


procedure TUserManagerForm.FormDestroy(Sender: TObject);
begin
  // FormDestroy: Save GUI setup
  OpenRegistry;
  MainReg.WriteInteger( REGNAME_USERMNGR_WINWIDTH, Width );
  MainReg.WriteInteger( REGNAME_USERMNGR_WINHEIGHT, Height );
  MainReg.WriteInteger( REGNAME_USERMNGR_LISTWIDTH, pnlLeft.Width );
  Mainform.SaveListSetup(listUsers);
end;


procedure TUserManagerForm.FormShow(Sender: TObject);
var
  Version: Integer;
  Users: TMySQLQuery;
  U: TUser;

function InitPrivList(Values: String): TStringList;
begin
  Result := Explode(',', Values);
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
end;

begin
  Version := Mainform.ActiveConnection.ServerVersionInt;
  PrivsGlobal := InitPrivList('FILE,PROCESS,RELOAD,SHUTDOWN');
  PrivsDb := InitPrivList('');
  PrivsTable := InitPrivList('ALTER,CREATE,DELETE,DROP,GRANT,INDEX');
  PrivsRoutine := InitPrivList('GRANT');
  PrivsColumn := InitPrivList('INSERT,SELECT,UPDATE,REFERENCES');

  if Version >= 40002 then begin
    PrivsGlobal.Add('REPLICATION CLIENT');
    PrivsGlobal.Add('REPLICATION SLAVE');
    PrivsGlobal.Add('SHOW DATABASES');
    PrivsGlobal.Add('SUPER');
    PrivsDb.Add('CREATE TEMPORARY TABLES');
    PrivsDb.Add('LOCK TABLES');
    PrivsRoutine.Add('EXECUTE');
  end;
  if Version >= 50001 then begin
    PrivsTable.Add('CREATE VIEW');
    PrivsTable.Add('SHOW VIEW');
  end;
  if Version >= 50003 then begin
    PrivsGlobal.Add('CREATE USER');
    PrivsDb.Add('CREATE ROUTINE');
    PrivsRoutine.Add('ALTER ROUTINE');
  end;
  if Version >= 50106 then begin
    PrivsDb.Add('TRIGGER');
    PrivsDb.Add('EVENT');
  end;

  PrivsTable.AddStrings(PrivsColumn);
  PrivsDb.AddStrings(PrivsTable);
  PrivsDb.AddStrings(PrivsRoutine);
  PrivsGlobal.AddStrings(PrivsDb);

  PrivsGlobal.Sorted := False;
  PrivsGlobal.CustomSort(ComparePrivs);
  PrivsDb.Sorted := False;
  PrivsDb.CustomSort(ComparePrivs);
  PrivsTable.Sorted := False;
  PrivsTable.CustomSort(ComparePrivs);
  PrivsRoutine.Sorted := False;
  PrivsRoutine.CustomSort(ComparePrivs);
  PrivsColumn.Sorted := False;
  PrivsColumn.CustomSort(ComparePrivs);


  // Load user@host list
  try
    FUsers := TUserList.Create(True);
    Users := MainForm.ActiveConnection.GetResults(
      'SELECT '+QuoteIdent('user')+', '+QuoteIdent('host')+', '+QuoteIdent('password')+' '+
      'FROM '+QuoteIdent('mysql')+'.'+QuoteIdent('user')
      );
    while not Users.Eof do begin
      U := TUser.Create;
      U.Username := Users.Col('user');
      U.Host := Users.Col('host');
      U.Password := Users.Col('password');
      FUsers.Add(U);
      Users.Next;
    end;
    listUsers.Clear;
    InvalidateVT(listUsers, VTREE_NOTLOADED, False);
    FPrivObjects := TPrivObjList.Create(TPrivComparer.Create, True);
    Modified := False;
    FAdded := False;
    listUsers.OnFocusChanged(listUsers, listUsers.FocusedNode, listUsers.FocusedColumn);
  except
    on E:EDatabaseError do begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
      // Closing form in OnShow does not work. Instead, do that in listUsers.OnBeforePaint.
    end;
  end;
end;


procedure TUserManagerForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Try to unfocus user item. If not done, user clicked "Cancel"
  listUsers.FocusedNode := nil;
  CanClose := not Assigned(listUsers.FocusedNode);
end;


procedure TUserManagerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Free user list and list of available priv names
  FreeAndNil(FUsers);
  FreeAndNil(FPrivObjects);
  FreeAndNil(PrivsGlobal);
  FreeAndNil(PrivsDb);
  FreeAndNil(PrivsTable);
  FreeAndNil(PrivsRoutine);
  FreeAndNil(PrivsColumn);
end;


procedure TUserManagerForm.SetModified(Value: Boolean);
begin
  FModified := Value;
  btnSave.Enabled := FModified;
  btnDiscard.Enabled := FModified and (not FAdded);
  listUsers.Invalidate;
end;


procedure TUserManagerForm.Modification(Sender: TObject);
begin
  Modified := True;
end;


procedure TUserManagerForm.listUsersAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
begin
  // Background painting for sorted column
  MainForm.vstAfterPaint(Sender, TargetCanvas);
end;


procedure TUserManagerForm.listUsersBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
var
  VT: TVirtualStringTree;
begin
  // Users may have got new or removed ones - reinit nodes.
  // If Form.OnShow failed to get the list of users, close form from here.
  if not Assigned(FUsers) then
    Close
  else begin
    VT := Sender as TVirtualStringTree;
    if VT.Tag = VTREE_NOTLOADED then begin
      VT.RootNodeCount := FUsers.Count;
      VT.FocusedNode := nil;
      VT.ClearSelection;
      VT.Tag := VTREE_LOADED;
    end;
  end;
end;


procedure TUserManagerForm.listUsersFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  // Allow selecting a user? Also, set allowed to false if new node is the same as
  // the old one, otherwise OnFocusChanged will be triggered.
  Allowed := (NewNode <> OldNode) and (not Assigned(NewNode) or (not (vsDisabled in NewNode.States)));
  if Allowed and FModified then begin
    case MessageDlg('Save modified user?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: begin
        btnSave.Click;
        Allowed := not FModified;
      end;
      mrNo: begin
        Allowed := True;
        if FAdded then
          btnDeleteUser.Click;
      end;
      mrCancel: Allowed := False;
    end;
  end;
end;


procedure TUserManagerForm.listUsersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var
  P, Ptmp, PCol: TPrivObj;
  User: PUser;
  UserHost: String;
  Grants, AllPNames, Cols: TStringList;
  rxA, rxB: TRegExpr;
  i, j: Integer;
  UserSelected: Boolean;
begin
  // Parse and display privileges of focused user
  UserSelected := Assigned(Node);
  FPrivObjects.Clear;
  Caption := MainForm.actUserManager.Caption;
  editUsername.Clear;
  editFromHost.Clear;
  editPassword.Clear;
  editPassword.TextHint := '';
  editRepeatPassword.Clear;

  if UserSelected then begin
    User := Sender.GetNodeData(Node);
    UserHost := esc(User.Username)+'@'+esc(User.Host);
    editUsername.Text := User.Username;
    editFromHost.Text := User.Host;
    Caption := Caption + ' - ' + User.Username;

    AllPNames := TStringList.Create;
    AllPNames.AddStrings(PrivsGlobal);
    AllPNames.AddStrings(PrivsDb);
    AllPNames.AddStrings(PrivsTable);
    AllPNames.AddStrings(PrivsRoutine);
    AllPNames.AddStrings(PrivsColumn);

    // New or existing user mode
    if FAdded then begin
      if Assigned(CloneGrants) then begin
        Grants := TStringList.Create;
        Grants.AddStrings(CloneGrants);
      end else begin
        Grants := TStringList.Create;
        Grants.Add('GRANT USAGE ON *.* TO '+UserHost);
      end;
    end else
      Grants := MainForm.ActiveConnection.GetCol('SHOW GRANTS FOR '+esc(User.Username)+'@'+esc(User.Host));

    rxA := TRegExpr.Create;
    rxA.ModifierI := True;
    rxB := TRegExpr.Create;
    rxB.ModifierI := True;

    for i:=0 to Grants.Count-1 do begin
      // Find selected priv objects via regular expression
      { GRANT USAGE ON *.* TO 'newbie'@'%' IDENTIFIED BY PASSWORD '*99D8973ECC09819DF81624F051BFF4FC6695140B' WITH GRANT OPTION
      GRANT SELECT ON `avtoserver`.* TO 'newbie'@'%'
      GRANT SELECT, SELECT (Enter (column) name), INSERT, INSERT (Enter (column) name), UPDATE, UPDATE (Enter (column) name), DELETE, CREATE ON `avtoserver`.`avtomodel` TO 'newbie'@'%'
      GRANT EXECUTE, ALTER ROUTINE ON PROCEDURE `pulle`.`f_procedure` TO 'newbie'@'%' }
      rxA.Expression := '^GRANT\s+(.+)\s+ON\s+((TABLE|FUNCTION|PROCEDURE)\s+)?`?([^`.]+)`?\.`?([^`]+)`?\s+TO\s+\S+(\s+IDENTIFIED\s+BY\s+(PASSWORD)?\s+''?([^'']+)''?)?(\s+WITH.+GRANT\s+OPTION)?';
      if rxA.Exec(Grants[i]) then begin
        P := TPrivObj.Create;
        P.GrantCode := Grants[i];
        P.Added := FAdded;
        FPrivObjects.Add(P);

        if (rxA.Match[4] = '*') and (rxA.Match[5] = '*') then begin
          P.DBObj.NodeType := lntNone;
          P.AllPrivileges := PrivsGlobal;
          if not FAdded then
            editPassword.TextHint := MainForm.ActiveConnection.UnescapeString(rxA.Match[8]);
        end else if (rxA.Match[5] = '*') then begin
          P.DBObj.NodeType := lntDb;
          P.DBObj.Database := rxA.Match[4];
          P.AllPrivileges := PrivsDb;
        end else begin
          P.DBObj.Database := rxA.Match[4];
          P.DBObj.Name := rxA.Match[5];
          if UpperCase(rxA.Match[3]) = 'FUNCTION' then begin
            P.DBObj.NodeType := lntFunction;
            P.AllPrivileges := PrivsRoutine;
          end else if (UpperCase(rxA.Match[3]) = 'PROCEDURE') then begin
            P.DBObj.NodeType := lntProcedure;
            P.AllPrivileges := PrivsRoutine;
          end else begin
            P.DBObj.NodeType := lntTable;
            P.AllPrivileges := PrivsTable;
          end;
        end;

        // Find selected privileges
        { USAGE
          SELECT, SELECT (id, colname), INSERT, INSERT (id, colname), UPDATE, UPDATE (colname), DELETE, CREATE
          EXECUTE, ALTER ROUTINE }
        if rxA.Match[1] = 'ALL PRIVILEGES' then begin
          P.OrgPrivs.AddStrings(P.AllPrivileges);
          P.OrgPrivs.Delete(P.OrgPrivs.IndexOf('GRANT'));
        end else begin
          rxB.Expression := '\b('+ImplodeStr('|', AllPnames)+')(\s+\(([^\)]+)\))?,';
          if rxB.Exec(rxA.Match[1]+',') then while True do begin
            if rxB.Match[3] = '' then
              P.OrgPrivs.Add(rxB.Match[1])
            else begin
              // Find previously created column priv or create new one
              Cols := Explode(',', rxB.Match[3]);
              for j:=0 to Cols.Count-1 do begin
                PCol := nil;
                for Ptmp in FPrivObjects do begin
                  if (Ptmp.DBObj.NodeType=lntColumn)
                    and (Ptmp.DBObj.Database=P.DBObj.Database)
                    and (Ptmp.DBObj.Name=P.DBObj.Name)
                    and (Ptmp.DBObj.Column=Trim(Cols[j])) then begin
                    PCol := Ptmp;
                    break;
                  end;
                end;
                if PCol = nil then begin
                  PCol := TPrivObj.Create;
                  PCol.DBObj.NodeType := lntColumn;
                  PCol.DBObj.Database := P.DBObj.Database;
                  PCol.DBObj.Name := P.DBObj.Name;
                  PCol.DBObj.Column := Trim(Cols[j]);
                  PCol.AllPrivileges := PrivsColumn;
                  FPrivObjects.Add(PCol);
                end;
                PCol.OrgPrivs.Add(rxB.Match[1]);
                PCol.GrantCode := PCol.GrantCode + rxB.Match[1] + ' ('+Trim(Cols[j])+')' + ', ';
              end;

            end;
            if not rxB.ExecNext then
              break;
          end;

        end;
        // WITH .. GRANT OPTION ?
        if rxA.Match[9] <> '' then
          P.OrgPrivs.Add('GRANT');
        if (P.OrgPrivs.Count = 0) and (P.DBObj.NodeType = lntTable) then
          FPrivObjects.Remove(P);
      end;
    end;

    // Generate grant code for column privs by hand
    for Ptmp in FPrivObjects do begin
      if Ptmp.DBObj.NodeType = lntColumn then begin
        Ptmp.GrantCode := 'GRANT ' + Copy(Ptmp.GrantCode, 1, Length(Ptmp.GrantCode)-2) + ' ON ' +
          QuoteIdent(Ptmp.DBObj.Database) + '.' +
          QuoteIdent(Ptmp.DBObj.Name) +
          ' TO ' + UserHost;
      end;
      // Flag all privs as added, so "Save" action applies them
      if Assigned(CloneGrants) then
        Ptmp.AddedPrivs.AddStrings(Ptmp.OrgPrivs);
    end;

    FPrivObjects.Sort;
    rxA.Free;
    rxB.Free;
    FreeAndNil(Grants);
    FreeAndNil(CloneGrants);
    FreeAndNil(AllPnames);
    FreeAndNil(Cols);
  end;

  // Populate privilege tree
  Modified := False;
  treePrivs.FocusedNode := nil;
  treePrivs.Clear;
  treePrivs.RootNodeCount := FPrivObjects.Count;
  treePrivs.ReinitNode(nil, True);
  treePrivs.Invalidate;

  // Enable input boxes
  lblUsername.Enabled := UserSelected;
  editUsername.Enabled := UserSelected;
  lblFromHost.Enabled := UserSelected;
  editFromHost.Enabled := UserSelected;
  lblPassword.Enabled := UserSelected;
  editPassword.Enabled := UserSelected;
  lblRepeatPassword.Enabled := UserSelected;
  editRepeatPassword.Enabled := UserSelected;
  grpCredentials.Enabled := UserSelected;
  btnAddObject.Enabled := UserSelected;
  btnDeleteUser.Enabled := UserSelected;
  btnCloneUser.Enabled := UserSelected and (not FAdded);

  // Ensure the warning hint is displayed or cleared. This is not done when the dialog shows up.
  listUsers.OnHotChange(Sender, nil, Node);
end;


procedure TUserManagerForm.listUsersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  User: PUser;
begin
  if Column <> 0 then
    Exit;
  case Kind of
    ikNormal, ikSelected: ImageIndex := 43;
    ikOverlay: begin
      User := Sender.GetNodeData(Node);
      if User.Password = '' then
        ImageIndex := 161;
      if FModified and (Node = Sender.FocusedNode) then
        ImageIndex := 162;
    end;
  end;
end;


procedure TUserManagerForm.listUsersGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TUser);
end;


procedure TUserManagerForm.listUsersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  User: PUser;
begin
  if not Assigned(FUsers) then
    Exit;
  User := Sender.GetNodeData(Node);
  case Column of
    0: CellText := User.Username;
    1: CellText := User.Host;
  end;
end;


procedure TUserManagerForm.listUsersHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  Mainform.vstHeaderClick(Sender, HitInfo);
end;


procedure TUserManagerForm.listUsersHotChange(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode);
var
  Node: PVirtualNode;
  User: PUser;
  Msg: String;
begin
  // Display warning hint for problematic stuff in the lower left corner.
  Node := NewNode;
  if not Assigned(Node) then
    Node := Sender.FocusedNode;
  Msg := '';
  if Assigned(Node) then begin
    User := Sender.GetNodeData(Node);
    case Length(User.Password) of
      0: Msg := 'This user has an empty password.';
      16, 41: Msg := '';
      else Msg := 'This user is inactive due to an invalid length of its encrypted password. Please fix that in the mysql.user table.';
    end;
  end;
  lblWarning.Caption := Msg;
end;


procedure TUserManagerForm.listUsersCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
begin
  Mainform.vstCompareNodes(Sender, Node1, Node2, Column, Result);
end;


procedure TUserManagerForm.listUsersInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  User: PUser;
begin
  User := Sender.GetNodeData(Node);
  User^ := FUsers[Node.Index];
  if not (Length(User.Password) in [0, 16, 41]) then
    Include(InitialStates, ivsDisabled);
end;


function TUserManagerForm.GetPrivByNode(Node: PVirtualNode): TPrivObj;
begin
  // Return priv object by node
  if treePrivs.GetNodeLevel(Node) = 0 then
    Result := FPrivObjects[Node.Index]
  else
    Result := FPrivObjects[Node.Parent.Index];
end;


procedure TUserManagerForm.treePrivsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  P: TPrivObj;
  idxO, idxA, idxD: Integer;
  PrivName: String;
begin
  // Checked some privilege check box
  case Sender.GetNodeLevel(Node) of
    0: begin
      Sender.Expanded[Node] := True;
      Sender.Invalidate;
    end;
    1: begin
      Modification(Sender);
      P := GetPrivByNode(Node);
      PrivName := P.AllPrivileges[Node.Index];
      idxO := P.OrgPrivs.IndexOf(PrivName);
      idxA := P.AddedPrivs.IndexOf(PrivName);
      idxD := P.DeletedPrivs.IndexOf(PrivName);
      if idxA > -1 then
        P.AddedPrivs.Delete(idxA);
      if idxD > -1 then
        P.DeletedPrivs.Delete(idxD);
      if (Node.CheckState in CheckedStates) and (idxO = -1) then
        P.AddedPrivs.Add(PrivName);
      if (not (Node.CheckState in CheckedStates)) and (idxO > -1) then
        P.DeletedPrivs.Add(PrivName);
    end;
  end;
end;


procedure TUserManagerForm.treePrivsExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
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


procedure TUserManagerForm.treePrivsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  // Icon for privilege
  if Sender.GetNodeLevel(Node) <> 0 then
    Exit;
  case Kind of
    ikNormal, ikSelected:
      ImageIndex := FPrivObjects[Node.Index].DBObj.ImageIndex;
    ikOverlay: begin
      if FPrivObjects[Node.Index].Added then
        ImageIndex := 163;
    end;
  end;
end;


procedure TUserManagerForm.treePrivsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p: TPrivObj;
begin
  // Display priv object text
  p := GetPrivByNode(Node);
  case Sender.GetNodeLevel(Node) of
    0: begin
      case p.DBObj.NodeType of
        lntNone:
          CellText := 'Global privileges';
        lntDb:
          CellText := 'Database: '+p.DBObj.Database;
        lntTable, lntProcedure, lntFunction:
          CellText := p.DBObj.ObjType+': '+p.DBObj.Database+'.'+p.DBObj.Name;
        lntColumn:
          CellText := p.DBObj.ObjType+': '+p.DBObj.Database+'.'+p.DBObj.Name+'.'+p.DBObj.Column;
      end;
    end;
    1: CellText := p.AllPrivileges[Node.Index];
  end;
end;


procedure TUserManagerForm.treePrivsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
begin
  if Sender.GetNodeLevel(Node) = 0 then
    ChildCount := FPrivObjects[Node.Index].AllPrivileges.Count;
end;


procedure TUserManagerForm.treePrivsInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p: TPrivObj;
begin
  Node.CheckType := ctTriStateCheckBox;
  p := GetPrivByNode(Node);
  case Sender.GetNodeLevel(Node) of
    0: begin
      // Display plus/minus button
      Include(InitialStates, ivsHasChildren);
      // AutoOptions.toAutoTristateTracking does a good job but it does not auto-check parent nodes when initializing
      if p.OrgPrivs.Count = 0 then
        Node.CheckState := csUncheckedNormal
      else if p.OrgPrivs.Count < p.AllPrivileges.Count then
        Node.CheckState := csMixedNormal
      else
        Node.CheckState := csCheckedNormal;
    end;
    1: begin
      // Added objects have some basic added privs, others only have original privs.
      Node.CheckState := csUncheckedNormal;
      if (p.OrgPrivs.IndexOf(p.AllPrivileges[Node.Index]) > -1)
        or (p.AddedPrivs.IndexOf(p.AllPrivileges[Node.Index]) > -1) then
        Node.CheckState := csCheckedNormal;
    end;
  end;
end;


procedure TUserManagerForm.treePrivsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  PrivName: String;
begin
  // Colors for privilege names
  if (Sender.GetNodeLevel(Node) = 1) and (not (vsSelected in Node.States)) then begin
    PrivName := FPrivObjects[Node.Parent.Index].AllPrivileges[Node.Index];
    if PrivsRead.IndexOf(PrivName) > -1 then
      TargetCanvas.Font.Color := clGreen
    else if PrivsWrite.IndexOf(PrivName) > -1 then
      TargetCanvas.Font.Color := clMaroon
    else if PrivsAdmin.IndexOf(PrivName) > -1 then
      TargetCanvas.Font.Color := clNavy;
  end;
end;


procedure TUserManagerForm.btnAddUserClick(Sender: TObject);
var
  P: TPrivObj;
  User: TUser;
  NodeUser: PUser;
  Node: PVirtualNode;
begin
  // Create new user
  // Try to unfocus current user which triggers saving modifications.
  listUsers.FocusedNode := nil;
  if Assigned(listUsers.FocusedNode) then
    Exit;
  User := TUser.Create;
  User.Username := 'Unnamed';
  User.Host := 'localhost';
  FUsers.Add(User);
  FAdded := True;
  if Sender = btnCloneUser then begin
    CloneGrants := TStringList.Create;
    for P in FPrivObjects do
      CloneGrants.Add(P.GrantCode);
  end;
  InvalidateVT(listUsers, VTREE_NOTLOADED, True);
  // Select newly added item.
  Node := listUsers.GetFirst;
  while Assigned(Node) do begin
    NodeUser := listUsers.GetNodeData(Node);
    if User = NodeUser^ then begin
      SelectNode(listUsers, Node);
      break;
    end;
    Node := listUsers.GetNextSibling(Node);
  end;
  Modified := True;
  // Focus the user name entry box.
  editUserName.SetFocus;
end;


procedure TUserManagerForm.btnAddObjectClick(Sender: TObject);
var
  DBObj: TDBObject;
  Priv: TPrivObj;
  Node, Child: PVirtualNode;
begin
  // Add new privilege object
  DBObj := SelectDBO;
  if not Assigned(DBObj) then
    Exit;
  // Check for unsupported object type, selectable in tree
  if not (DBObj.NodeType in [lntDb, lntTable, lntFunction, lntProcedure, lntColumn]) then begin
    MessageDlg('Objects of type '+DBObj.ObjType+' cannot be part of privileges.', mtError, [mbOK], 0);
    Exit;
  end;
  // Check if this would be a duplicate object
  for Priv in FPrivObjects do begin
    if Priv.DBObj.IsSameAs(DBObj) then begin
      MessageDlg('Selected object is already accessible.', mtError, [mbOK], 0);
      Exit;
    end;
  end;

  Priv := TPrivObj.Create;
  Priv.DBObj := DBObj;
  case Priv.DBObj.NodeType of
    lntNone: Priv.AllPrivileges := PrivsGlobal;
    lntDb: Priv.AllPrivileges := PrivsDb;
    lntTable: Priv.AllPrivileges := PrivsTable;
    lntFunction, lntProcedure: Priv.AllPrivileges := PrivsRoutine;
    lntColumn: Priv.AllPrivileges := PrivsColumn;
  end;
  // Assign minimum privileges
  case Priv.DBObj.NodeType of
    lntFunction, lntProcedure: Priv.AddedPrivs.Add('EXECUTE');
    else Priv.AddedPrivs.Add('SELECT');
  end;
  Priv.Added := True;
  FPrivObjects.Add(Priv);
  Node := treePrivs.AddChild(nil);
  Child := treePrivs.GetFirstChild(Node);
  while Assigned(Child) do
    Child := treePrivs.GetNextSibling(Child);
  treePrivs.Expanded[Node] := True;
  treePrivs.SetFocus;
  SelectNode(treePrivs, Node);
  Modified := True;
end;


procedure TUserManagerForm.btnSaveClick(Sender: TObject);
var
  Conn: TMySQLConnection;
  UserHost, OrgUserHost, Create, Table, Revoke, Grant, OnObj: String;
  User: TUser;
  FocusedUser: PUser;
  Tables: TStringList;
  P: TPrivObj;
  i: Integer;
  PasswordSet: Boolean;
begin
  // Save changes
  Conn := MainForm.ActiveConnection;
  FocusedUser := listUsers.GetNodeData(listUsers.FocusedNode);
  if FAdded then begin
    FocusedUser.Username := editUsername.Text;
    FocusedUser.Host := editFromHost.Text;
  end;
  OrgUserHost := esc(FocusedUser.Username)+'@'+esc(FocusedUser.Host);
  UserHost := esc(editUsername.Text)+'@'+esc(editFromHost.Text);

  try
    // Ensure we have a unique user@host combination
    for User in FUsers do begin
      if User = FocusedUser^ then
        Continue;
      if (User.Username = editUsername.Text) and (User.Host = editFromHost.Text) then
        raise EInputError.Create('User <'+editUsername.Text+'@'+editFromHost.Text+'> already exists.');
    end;

    // Check input: Ensure we have a unique user@host combination
    if editPassword.Text <> editRepeatPassword.Text then
      raise EInputError.Create('Repeated password does not match first one.');

    // Create added user
    PasswordSet := False;
    if FAdded and (Conn.ServerVersionInt >= 50002) then begin
      Create := 'CREATE USER '+UserHost;
      if editPassword.Modified then
        Create := Create + ' IDENTIFIED BY '+esc(editPassword.Text);
      Conn.Query(Create);
      PasswordSet := True;
    end;

    // Grant added privileges and revoke deleted ones
    for P in FPrivObjects do begin

      case P.DBObj.NodeType of
        lntNone:
          OnObj := '*.*';
        lntDb:
          OnObj := QuoteIdent(P.DBObj.Database) + '.*';
        lntTable, lntFunction, lntProcedure:
          OnObj := UpperCase(P.DBObj.ObjType) + ' ' + QuoteIdent(P.DBObj.Database) + '.' + QuoteIdent(P.DBObj.Name);
        lntColumn:
          OnObj := 'TABLE ' + QuoteIdent(P.DBObj.Database) + '.' + QuoteIdent(P.DBObj.Name);
        else
          raise Exception.Create('Unhandled privilege object: '+P.DBObj.ObjType);
      end;

      // Revoke privileges
      if (not P.Added) and (P.DeletedPrivs.Count > 0) then begin
        Revoke := '';
        for i:=0 to P.DeletedPrivs.Count-1 do begin
          Revoke := Revoke + P.DeletedPrivs[i];
          if P.DeletedPrivs[i] = 'GRANT' then
            Revoke := Revoke + ' OPTION';
          if P.DBObj.NodeType = lntColumn then
            Revoke := Revoke + '('+QuoteIdent(P.DBObj.Column)+')';
          Revoke := Revoke + ', ';
        end;
        Delete(Revoke, Length(Revoke)-1, 1);
        Revoke := 'REVOKE ' + Revoke + ' ON ' + OnObj + ' FROM ' + OrgUserHost;
        Conn.Query(Revoke);
      end;

      // Grant privileges. Must be applied with USAGE for added users without specific privs.
      if P.Added or (P.AddedPrivs.Count > 0) then begin
        Grant := '';
        for i:=0 to P.AddedPrivs.Count-1 do begin
          if P.AddedPrivs[i] = 'GRANT' then
            Continue;
          Grant := Grant + P.AddedPrivs[i];
          if P.DBObj.NodeType = lntColumn then
            Grant := Grant + '('+QuoteIdent(P.DBObj.Column)+')';
          Grant := Grant + ', ';
        end;
        Delete(Grant, Length(Grant)-1, 1);
        if Grant = '' then
          Grant := 'USAGE';
        Grant := 'GRANT ' + Grant + ' ON ' + OnObj + ' TO ' + OrgUserHost;
        if P.AddedPrivs.IndexOf('GRANT') > -1 then
          Grant := Grant + ' WITH GRANT OPTION';
        Conn.Query(Grant);
      end;
    end;

    // Set password
    if editPassword.Modified and (not PasswordSet) then begin
      Conn.Query('SET PASSWORD FOR ' + OrgUserHost + ' = PASSWORD('+esc(editPassword.Text)+')');
    end;

    // Rename user
    if (FocusedUser.Username <> editUsername.Text) or (FocusedUser.Host <> editFromHost.Text) then begin
      if Conn.ServerVersionInt >= 50002 then
        Conn.Query('RENAME USER '+OrgUserHost+' TO '+UserHost)
      else begin
        Tables := Explode(',', 'user,db,tables_priv,columns_priv');
        for Table in Tables do begin
          Conn.Query('UPDATE '+QuoteIdent('mysql')+'.'+QuoteIdent(Table)+
            ' SET User='+esc(editUsername.Text)+', Host='+esc(editFromHost.Text)+
            ' WHERE User='+esc(FocusedUser.Username)+' AND Host='+esc(FocusedUser.Host)
            );
        end;
        FreeAndNil(Tables);
      end;
    end;

    Conn.Query('FLUSH PRIVILEGES');
    Modified := False;
    FAdded := False;
    FocusedUser.Username := editUsername.Text;
    FocusedUser.Host := editFromHost.Text;
    if editPassword.Modified then
      FocusedUser.Password := editPassword.Text;
    listUsers.OnFocusChanged(listUsers, listUsers.FocusedNode, listUsers.FocusedColumn);
  except
    on E:EDatabaseError do
      MessageDlg(E.Message, mtError, [mbOK], 0);
    on E:EInputError do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;

end;


procedure TUserManagerForm.btnDeleteUserClick(Sender: TObject);
var
  UserHost: String;
  Conn: TMySQLConnection;
  User: PUser;
begin
  // Delete user
  User := listUsers.GetNodeData(listUsers.FocusedNode);
  if FAdded then begin
    FUsers.Remove(User^);
    listUsers.DeleteNode(listUsers.FocusedNode);
    FAdded := False;
  end else if MessageDlg('Delete user '+User.Username+'@'+User.Host+'?', mtConfirmation, [mbYes, mbCancel], 0 ) = mrYes then begin
    Conn := MainForm.ActiveConnection;
    UserHost := esc(User.Username)+'@'+esc(User.Host);
    try
      // Revoke privs explicitly, required on old servers.
      // Newer servers only require one DROP USER query
      if Conn.ServerVersionInt < 50002 then begin
        Conn.Query('REVOKE ALL PRIVILEGES ON *.* FROM '+UserHost);
        Conn.Query('REVOKE GRANT OPTION ON *.* FROM '+UserHost);
      end;
      if Conn.ServerVersionInt < 40101 then
        Conn.Query('DELETE FROM mysql.user WHERE User='+esc(User.Username)+' AND Host='+esc(User.Host))
      else
        Conn.Query('DROP USER '+UserHost);
      FUsers.Remove(User^);
      listUsers.DeleteNode(listUsers.FocusedNode);
    except on E:EDatabaseError do
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;


procedure TUserManagerForm.btnDiscardClick(Sender: TObject);
begin
  // Reset modifications
  Modified := False;
  listUsers.OnFocusChanged(listUsers, listUsers.FocusedNode, listUsers.FocusedColumn);
end;


procedure TUserManagerForm.menuHostClick(Sender: TObject);
begin
  // Insert predefined host
  editFromHost.Text := (Sender as TMenuItem).Hint;
end;


procedure TUserManagerForm.menuHostPopup(Sender: TObject);
var
  Item: TMenuItem;
  i: Integer;
  User: TUser;
  ItemExists: Boolean;
begin
  // Delete custom items and readd unique ones
  for i:=menuHost.Items.Count-1 downto 0 do begin
    if menuHost.Items[i].Caption = '-' then
      break;
    menuHost.Items.Delete(i);
  end;
  for User in FUsers do begin
    if User.Host = '' then
      Continue;
    ItemExists := False;
    for Item in menuHost.Items do begin
      if Item.Hint = User.Host then begin
        ItemExists := True;
        Break;
      end;
    end;
    if not ItemExists then begin
      Item := TMenuItem.Create(menuHost);
      Item.Caption := User.Host;
      Item.Hint := User.Host;
      Item.OnClick := menuHostClick;
      menuHost.Items.Add(Item);
    end;
  end;
  // Auto check current host if any matches
  for Item in menuHost.Items do
    Item.Checked := Item.Hint = editFromHost.Text;
end;


procedure TUserManagerForm.editPasswordChange(Sender: TObject);
begin
  // Password manually edited
  editRepeatPassword.Enabled := True;
  editPassword.PasswordChar := '*';
  editRepeatPassword.PasswordChar := editPassword.PasswordChar;
  Modification(Sender);
end;


procedure TUserManagerForm.menuPasswordInsert(Sender: TObject);
var
  Item: TMenuItem;
begin
  // Insert password from menu item
  Item := Sender as TMenuItem;
  editPassword.Text := Item.Caption;
  editPassword.Modified := True;
  editPassword.PasswordChar := #0;
  editRepeatPassword.Text := editPassword.Text;
  editRepeatPassword.PasswordChar := editPassword.PasswordChar;
  editRepeatPassword.Enabled := False;
end;


procedure TUserManagerForm.menuPasswordClick(Sender: TObject);
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



{ TPrivObj }

constructor TPrivObj.Create;
begin
  OrgPrivs := TStringList.Create;
  AddedPrivs := TStringList.Create;
  AddedPrivs.Duplicates := dupIgnore;
  DeletedPrivs := TStringList.Create;
  DeletedPrivs.Duplicates := dupIgnore;
  Added := False;
  DBObj := TDBObject.Create(MainForm.ActiveConnection);
end;


destructor TPrivObj.Destroy;
begin
  FreeAndNil(DBObj);
  FreeAndNil(OrgPrivs);
  FreeAndNil(AddedPrivs);
  FreeAndNil(DeletedPrivs);
end;


{ TPrivComparer }

function TPrivComparer.Compare(const Left, Right: TPrivObj): Integer;
begin
  // Prio for global > db > table > view > function > proc > event > column
  if (Left.DBObj.NodeType < Right.DBObj.NodeType) then
    Result := -1
  else if (Left.DBObj.NodeType > Right.DBObj.NodeType) then
    Result := 1
  else begin
    Result := CompareText(
      Left.DBObj.Database+Left.DBObj.Name+Left.DBObj.Column,
      Right.DBObj.Database+Right.DBObj.Name+Right.DBObj.Column
      );
  end;
end;


end.
