unit usermanager;


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, ToolWin, ClipBrd, Generics.Collections, Generics.Defaults, SynRegExpr,
  mysql_connection, helpers, VirtualTrees;

{$I const.inc}


type
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
    editFromHost: TEdit;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAddUserClick(Sender: TObject);
    procedure btnDeleteUserClick(Sender: TObject);
    procedure editPasswordRightButtonClick(Sender: TObject);
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
  private
    { Private declarations }
    FUsers: TStringList;
    FModified, FAdded: Boolean;
    CloneGrants: TStringList;
    FPrivObjects: TPrivObjList;
    PrivsGlobal, PrivsDb, PrivsTable, PrivsRoutine, PrivsColumn: TStringList;
    OrgUsername, OrgFromHost: String;
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
end;


procedure TUserManagerForm.FormShow(Sender: TObject);
var
  Version: Integer;

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
    FUsers := MainForm.ActiveConnection.GetCol(
      'SELECT CONCAT('+MainForm.mask('user')+', '+esc('@')+', '+MainForm.mask('host')+') '+
      'FROM '+MainForm.mask('mysql')+'.'+MainForm.mask('user')+' '+
      'WHERE '+MainForm.mask('Password')+'!='+esc('!')+' '+
      'ORDER BY LOWER('+MainForm.mask('user')+'), LOWER('+MainForm.mask('host')+')');
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
  if (OldNode <> NewNode) and FModified then begin
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
  end else
    Allowed := True;
end;


procedure TUserManagerForm.listUsersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var
  P, Ptmp, PCol: TPrivObj;
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
  lblWarning.Visible := False;
  editUsername.Clear;
  editFromHost.Clear;
  editPassword.Clear;
  editPassword.TextHint := '';
  editRepeatPassword.Clear;

  if UserSelected then begin
    UserHost := FUsers[listUsers.FocusedNode.Index];
    OrgUsername := Copy(UserHost, 1, Pos('@', UserHost)-1);
    OrgFromHost := Copy(UserHost, Pos('@', UserHost)+1, MaxInt);
    UserHost := esc(OrgUsername)+'@'+esc(OrgFromHost);
    editUsername.Text := OrgUsername;
    editFromHost.Text := OrgFromHost;
    Caption := Caption + ' - ' + FUsers[listUsers.FocusedNode.Index];

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
      Grants := MainForm.ActiveConnection.GetCol('SHOW GRANTS FOR '+esc(OrgUsername)+'@'+esc(OrgFromHost));

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
          if not FAdded then begin
            lblWarning.Visible := rxA.Match[8] = '';
            editPassword.TextHint := MainForm.ActiveConnection.UnescapeString(rxA.Match[8]);
          end;
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
          MainForm.mask(Ptmp.DBObj.Database) + '.' +
          MainForm.mask(Ptmp.DBObj.Name) +
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
end;


procedure TUserManagerForm.listUsersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if Kind in [ikNormal, ikSelected] then begin
    if FModified and (Node = Sender.FocusedNode) then
      ImageIndex := 12
    else
      ImageIndex := 43;
  end;
end;


procedure TUserManagerForm.listUsersGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(Cardinal);
end;


procedure TUserManagerForm.listUsersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Idx: PCardinal;
begin
  if not Assigned(FUsers) then
    Exit;
  Idx := Sender.GetNodeData(Node);
  CellText := FUsers[Idx^];
end;


procedure TUserManagerForm.listUsersInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Idx: PCardinal;
begin
  Idx := Sender.GetNodeData(Node);
  Idx^ := Node.Index;
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
  if (Sender.GetNodeLevel(Node) = 0) and (Kind in [ikNormal, ikSelected]) then
    ImageIndex := FPrivObjects[Node.Index].DBObj.ImageIndex;
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
      case p.OrgPrivs.IndexOf(p.AllPrivileges[Node.Index]) of
        -1: Node.CheckState := csUncheckedNormal;
        else Node.CheckState := csCheckedNormal;
      end;
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
begin
  // Create new user
  FUsers.Add('UnNamed@localhost');
  FAdded := True;
  if Sender = btnCloneUser then begin
    CloneGrants := TStringList.Create;
    for P in FPrivObjects do
      CloneGrants.Add(P.GrantCode);
  end;
  InvalidateVT(listUsers, VTREE_NOTLOADED, True);
  // Select newly added item.
  SelectNode(listUsers, listUsers.RootNodeCount - 1);
  Modified := True;
  // Focus the user name entry box.
  editUserName.SetFocus;
end;


procedure TUserManagerForm.btnAddObjectClick(Sender: TObject);
var
  DBObj: TDBObject;
  Priv: TPrivObj;
  Node: PVirtualNode;
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
    lntFunction, lntProcedure: Priv.OrgPrivs.Add('EXECUTE');
    else Priv.OrgPrivs.Add('SELECT');
  end;
  Priv.AddedPrivs.AddStrings(Priv.OrgPrivs);
  Priv.Added := True;
  FPrivObjects.Add(Priv);
  Node := treePrivs.AddChild(nil);
  treePrivs.Expanded[Node] := True;
  treePrivs.SetFocus;
  SelectNode(treePrivs, Node);
  Modified := True;
end;


procedure TUserManagerForm.btnSaveClick(Sender: TObject);
var
  Conn: TMySQLConnection;
  UserHost, NewUserHost, Create, Table, Revoke, Grant, OnObj: String;
  Tables: TStringList;
  P: TPrivObj;
  i: Integer;
  PasswordSet: Boolean;
begin
  // Save changes
  Conn := MainForm.ActiveConnection;
  if FAdded then begin
    OrgUsername := editUsername.Text;
    OrgFromHost := editFromHost.Text;
  end;
  UserHost := esc(OrgUsername)+'@'+esc(OrgFromHost);
  NewUserHost := editUsername.Text+'@'+editFromHost.Text;

  try
    // Ensure we have a unique user@host combination
    for i:=0 to FUsers.Count-1 do begin
      if i = Integer(listUsers.FocusedNode.Index) then
        Continue;
      if FUsers[i] = NewUserHost then
        raise EInputError.Create('User <'+NewUserHost+'> already exists.');
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
          OnObj := Mainform.mask(P.DBObj.Database) + '.*';
        lntTable, lntFunction, lntProcedure:
          OnObj := UpperCase(P.DBObj.ObjType) + ' ' + Mainform.mask(P.DBObj.Database) + '.' + Mainform.mask(P.DBObj.Name);
        lntColumn:
          OnObj := 'TABLE ' + Mainform.mask(P.DBObj.Database) + '.' + Mainform.mask(P.DBObj.Name);
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
            Revoke := Revoke + '('+Mainform.mask(P.DBObj.Column)+')';
          Revoke := Revoke + ', ';
        end;
        Delete(Revoke, Length(Revoke)-1, 1);
        Revoke := 'REVOKE ' + Revoke + ' ON ' + OnObj + ' FROM ' + UserHost;
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
            Grant := Grant + '('+Mainform.mask(P.DBObj.Column)+')';
          Grant := Grant + ', ';
        end;
        Delete(Grant, Length(Grant)-1, 1);
        if Grant = '' then
          Grant := 'USAGE';
        Grant := 'GRANT ' + Grant + ' ON ' + OnObj + ' TO ' + UserHost;
        if P.AddedPrivs.IndexOf('GRANT') > -1 then
          Grant := Grant + ' WITH GRANT OPTION';
        Conn.Query(Grant);
      end;
    end;

    // Set password
    if editPassword.Modified and (not PasswordSet) then begin
      Conn.Query('SET PASSWORD FOR ' + UserHost + ' = PASSWORD('+esc(editPassword.Text)+')');
    end;

    // Rename user
    if (OrgUsername <> editUsername.Text) or (OrgFromHost <> editFromHost.Text) then begin
      if Conn.ServerVersionInt >= 50002 then
        Conn.Query('RENAME USER '+UserHost+' TO '+esc(editUsername.Text)+'@'+esc(editFromHost.Text))
      else begin
        Tables := Explode(',', 'user,db,tables_priv,columns_priv');
        for Table in Tables do begin
          Conn.Query('UPDATE '+Mainform.mask('mysql')+'.'+Mainform.mask(Table)+
            ' SET User='+esc(editUsername.Text)+', Host='+esc(editFromHost.Text)+
            ' WHERE User='+esc(OrgUsername)+' AND Host='+esc(OrgFromHost)
            );
        end;
        FreeAndNil(Tables);
      end;
    end;

    Conn.Query('FLUSH PRIVILEGES');
    Modified := False;
    FAdded := False;
    FUsers[listUsers.FocusedNode.Index] := NewUserHost;
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
begin
  // Delete user
  if FAdded then begin
    FUsers.Delete(listUsers.FocusedNode.Index);
    listUsers.DeleteNode(listUsers.FocusedNode);
    FAdded := False;
  end else if MessageDlg('Delete user "'+FUsers[listUsers.FocusedNode.Index]+'"?',
    mtConfirmation, [mbYes, mbCancel], 0 ) = mrYes then begin
    Conn := MainForm.ActiveConnection;
    UserHost := esc(OrgUsername)+'@'+esc(OrgFromHost);
    try
      // Revoke privs explicitly, required on old servers.
      // Newer servers only require one DROP USER query
      if Conn.ServerVersionInt < 50002 then begin
        Conn.Query('REVOKE ALL PRIVILEGES ON *.* FROM '+UserHost);
        Conn.Query('REVOKE GRANT OPTION ON *.* FROM '+UserHost);
      end;
      if Conn.ServerVersionInt < 40101 then
        Conn.Query('DELETE FROM mysql.user WHERE User='+esc(OrgUsername)+' AND Host='+esc(OrgFromHost))
      else
        Conn.Query('DROP USER '+UserHost);
      FUsers.Delete(listUsers.FocusedNode.Index);
      InvalidateVT(listUsers, VTREE_NOTLOADED, False);
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


procedure TUserManagerForm.editPasswordRightButtonClick(Sender: TObject);
begin
  // Auto generate a random password, display it for a second and copy it to clipboard
  Screen.Cursor := crHourglass;
  editPassword.SetFocus;
  editPassword.Text := GeneratePassword(8);
  editPassword.Modified := True;
  editPassword.PasswordChar := #0;
  editPassword.Repaint;
  Sleep(1000);
  editPassword.PasswordChar := '*';
  Clipboard.AsText := editPassword.Text;
  Screen.Cursor := crDefault;
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
