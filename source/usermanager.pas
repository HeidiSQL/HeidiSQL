unit usermanager;


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, ToolWin, ClipBrd, Generics.Collections, Generics.Defaults, SynRegExpr, extra_controls,
  dbconnection, dbstructures, apphelpers, VirtualTrees, Menus, gnugettext;

{$I const.inc}


type
  TUserProblem = (upNone, upEmptyPassword, upInvalidPasswordLen, upSkipNameResolve, upUnknown);

  TUser = class(TObject)
    Username, Host, Password, Cipher, Issuer, Subject: String;
    MaxQueries, MaxUpdates, MaxConnections, MaxUserConnections, SSL: Integer;
    Problem: TUserProblem;
    function HostRequiresNameResolve: Boolean;
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

  TUserManagerForm = class(TExtForm)
    btnCancel: TButton;
    btnSave: TButton;
    pnlLeft: TPanel;
    listUsers: TVirtualStringTree;
    Splitter1: TSplitter;
    pnlRight: TPanel;
    tlbObjects: TToolBar;
    btnAddObject: TToolButton;
    treePrivs: TVirtualStringTree;
    btnDiscard: TButton;
    lblUsers: TLabel;
    ToolBar1: TToolBar;
    btnAddUser: TToolButton;
    btnDeleteUser: TToolButton;
    btnCloneUser: TToolButton;
    lblWarning: TLabel;
    lblAllowAccessTo: TLabel;
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
    PageControlSettings: TPageControl;
    tabCredentials: TTabSheet;
    tabLimitations: TTabSheet;
    lblUsername: TLabel;
    lblFromHost: TLabel;
    lblPassword: TLabel;
    lblRepeatPassword: TLabel;
    editRepeatPassword: TEdit;
    editPassword: TButtonedEdit;
    editFromHost: TButtonedEdit;
    editUsername: TEdit;
    lblMaxQueries: TLabel;
    lblMaxUpdates: TLabel;
    lblMaxConnections: TLabel;
    lblMaxUserConnections: TLabel;
    editMaxQueries: TEdit;
    editMaxUpdates: TEdit;
    editMaxConnections: TEdit;
    editMaxUserConnections: TEdit;
    udMaxQueries: TUpDown;
    udMaxUpdates: TUpDown;
    udMaxConnections: TUpDown;
    udMaxUserConnections: TUpDown;
    tabSSL: TTabSheet;
    lblCipher: TLabel;
    editCipher: TEdit;
    lblIssuer: TLabel;
    lblSubject: TLabel;
    editIssuer: TEdit;
    editSubject: TEdit;
    comboSSL: TComboBox;
    lblSSL: TLabel;
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
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure listUsersFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode;
      OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure btnSaveClick(Sender: TObject);
    procedure Modification(Sender: TObject);
    procedure treePrivsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
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
    procedure udMaxQueriesClick(Sender: TObject; Button: TUDBtnType);
    procedure comboSSLChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FUsers: TUserList;
    FModified, FAdded: Boolean;
    CloneGrants: TStringList;
    FPrivObjects: TPrivObjList;
    PrivsGlobal, PrivsDb, PrivsTable, PrivsRoutine, PrivsColumn: TStringList;
    FConnection: TDBConnection;
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
  HasSizeGrip := True;
  lblWarning.Font.Color := clRed;
  Width := AppSettings.ReadInt(asUsermanagerWindowWidth);
  Height := AppSettings.ReadInt(asUsermanagerWindowHeight);
  pnlLeft.Width := AppSettings.ReadInt(asUsermanagerListWidth);
  FixVT(listUsers);
  FixVT(treePrivs);
  Mainform.RestoreListSetup(listUsers);
  PrivsRead := Explode(',', 'SELECT,SHOW VIEW,SHOW DATABASES,PROCESS,EXECUTE');
  PrivsWrite := Explode(',', 'ALTER,CREATE,DROP,DELETE,UPDATE,INSERT,ALTER ROUTINE,CREATE ROUTINE,CREATE TEMPORARY TABLES,CREATE VIEW,INDEX,TRIGGER,EVENT,REFERENCES,CREATE TABLESPACE');
  PrivsAdmin := Explode(',', 'RELOAD,SHUTDOWN,REPLICATION CLIENT,REPLICATION SLAVE,SUPER,LOCK TABLES,GRANT,FILE,CREATE USER');
end;


procedure TUserManagerForm.FormDestroy(Sender: TObject);
begin
  // FormDestroy: Save GUI setup
  AppSettings.WriteInt(asUsermanagerWindowWidth, Width);
  AppSettings.WriteInt(asUsermanagerWindowHeight, Height);
  AppSettings.WriteInt(asUsermanagerListWidth, pnlLeft.Width);
  Mainform.SaveListSetup(listUsers);
end;


procedure TUserManagerForm.FormResize(Sender: TObject);
begin
  // Manually right align "Add object" button
  lblAllowAccessTo.Width := pnlRight.Width - btnAddObject.Width;
end;


procedure TUserManagerForm.FormShow(Sender: TObject);
var
  Version: Integer;
  Users: TDBQuery;
  U: TUser;
  tmp, PasswordExpr: String;
  SkipNameResolve,
  HasPassword,
  HasAuthString,
  PasswordLengthMatters: Boolean;
  UserTableColumns: TStringList;

function InitPrivList(Values: String): TStringList;
begin
  Result := Explode(',', Values);
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
end;

begin
  FConnection := Mainform.ActiveConnection;
  Version := FConnection.ServerVersionInt;
  PrivsGlobal := InitPrivList('FILE,PROCESS,RELOAD,SHUTDOWN');
  PrivsDb := InitPrivList('');
  PrivsTable := InitPrivList('ALTER,CREATE,DELETE,DROP,GRANT,INDEX');
  PrivsRoutine := InitPrivList('GRANT');
  PrivsColumn := InitPrivList('INSERT,SELECT,UPDATE,REFERENCES');
  PasswordLengthMatters := True;

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
  if Version >= 50404 then begin
    PrivsGlobal.Add('CREATE TABLESPACE');
  end;
  { TODO: PROXY priv must be applied with another GRANT syntax:
  GRANT PROXY ON 'employee'@'localhost' TO 'external_auth'@'localhost';
  if Version >= 50507 then begin
    PrivsDb.Add('PROXY');
  end;
  }
  if Version >= 80000 then begin
    // MySQL 8 has predefined length of hashed passwords only with
    // mysql_native_password plugin enabled users
    PasswordLengthMatters := False;
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

    tmp := FConnection.GetSessionVariable('skip_name_resolve');
    SkipNameResolve := LowerCase(tmp) = 'on';

    FConnection.Query('FLUSH PRIVILEGES');

    // Peek into user table structure, and find out where the password hash is stored
    UserTableColumns := FConnection.GetCol('SHOW COLUMNS FROM '+FConnection.QuoteIdent('mysql')+'.'+FConnection.QuoteIdent('user'));
    HasPassword := UserTableColumns.IndexOf('password') > -1;
    HasAuthString := UserTableColumns.IndexOf('authentication_string') > -1;
    if HasPassword and (not HasAuthString) then
      PasswordExpr := 'password'
    else if (not HasPassword) and HasAuthString then
      PasswordExpr := 'authentication_string'
    else if HasPassword and HasAuthString then
      PasswordExpr := 'IF(LENGTH(password)>0, password, authentication_string)'
    else
      Raise Exception.Create(_('No password hash column available'));
    PasswordExpr := PasswordExpr + ' AS ' + FConnection.QuoteIdent('password');

    Users := FConnection.GetResults(
      'SELECT '+FConnection.QuoteIdent('user')+', '+FConnection.QuoteIdent('host')+', '+PasswordExpr+' '+
      'FROM '+FConnection.QuoteIdent('mysql')+'.'+FConnection.QuoteIdent('user')
      );
    FUsers := TUserList.Create(True);
    while not Users.Eof do begin
      U := TUser.Create;
      U.Username := Users.Col('user');
      U.Host := Users.Col('host');
      U.Password := Users.Col('password');
      U.Problem := upNone;
      if Length(U.Password) = 0 then
        U.Problem := upEmptyPassword;
      if PasswordLengthMatters and (not (Length(U.Password) in [0, 16, 41])) then
        U.Problem := upInvalidPasswordLen
      else if SkipNameResolve and U.HostRequiresNameResolve then
        U.Problem := upSkipNameResolve;
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
    on E:EDbError do begin
      ErrorDialog(E.Message);
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
  Action := caFree;
end;


procedure TUserManagerForm.SetModified(Value: Boolean);
begin
  FModified := Value;
  btnSave.Enabled := FModified;
  btnDiscard.Enabled := FModified and (not FAdded);
  listUsers.Invalidate;
end;


procedure TUserManagerForm.Modification(Sender: TObject);
var
  User: PUser;
begin
  if not Assigned(listUsers.FocusedNode) then
    Exit;
  if TWinControl(Sender).Parent = tabLimitations then begin
    // Any TUpDown triggers a OnChange event on its TEdit when the UpDown gets painted
    User := listUsers.GetNodeData(listUsers.FocusedNode);
    Modified := Modified
      or (editMaxQueries.Text <> IntToStr(User.MaxQueries))
      or (editMaxUpdates.Text <> IntToStr(User.MaxUpdates))
      or (editMaxConnections.Text <> IntToStr(User.MaxConnections))
      or (editMaxUserConnections.Text <> IntToStr(User.MaxUserConnections));
  end else begin
    Modified := True;
  end;
end;


procedure TUserManagerForm.udMaxQueriesClick(Sender: TObject; Button: TUDBtnType);
begin
  Modification(Sender);
end;


procedure TUserManagerForm.listUsersAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
begin
  // Background painting for sorted column
  MainForm.AnyGridAfterPaint(Sender, TargetCanvas);
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
    case MessageDialog(_('Save modified user?'), mtConfirmation, [mbYes, mbNo, mbCancel]) of
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
  UserHost, RequireClause, WithClause, Msg: String;
  Grants, AllPNames, Cols: TStringList;
  rxTemp, rxGrant: TRegExpr;
  i, j: Integer;
  UserSelected: Boolean;
  Obj: TDBObject;
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
  udMaxQueries.Position := 0;
  udMaxUpdates.Position := 0;
  udMaxConnections.Position := 0;
  udMaxUserConnections.Position := 0;
  comboSSL.ItemIndex := 0;
  comboSSL.OnChange(Sender);
  editCipher.Clear;
  editIssuer.Clear;
  editSubject.Clear;

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
    end else try
      Grants := FConnection.GetCol('SHOW GRANTS FOR '+esc(User.Username)+'@'+esc(User.Host));
    except
      on E:EDbError do begin
        Msg := E.Message;
        if FConnection.LastErrorCode = 1141 then begin
          // Disable this user node lately, for old server which do not show skip-name-resolve variable
          Msg := Msg + CRLF + CRLF + f_('Starting the server without %s may solve this issue.', ['--skip-name-resolve']);
          User.Problem := upUnknown;
          Node.States := Node.States + [vsDisabled];
        end;
        MessageDialog(Msg, mtError, [mbOK]);
        FModified := False;
        SelectNode(listUsers, nil);
        Exit;
      end;
    end;

    { GRANT USAGE ON *.* TO 'newbie'@'%' IDENTIFIED BY PASSWORD '*99D8973ECC09819DF81624F051BFF4FC6695140B' REQUIRE (NONE | ssl_option [[AND] ssl_option] ...) WITH GRANT OPTION
    GRANT SELECT ON `avtoserver`.* TO 'newbie'@'%'
    GRANT SELECT, SELECT (Enter (column) name), INSERT, INSERT (Enter (column) name), UPDATE, UPDATE (Enter (column) name), DELETE, CREATE ON `avtoserver`.`avtomodel` TO 'newbie'@'%'
    GRANT EXECUTE, ALTER ROUTINE ON PROCEDURE `pulle`.`f_procedure` TO 'newbie'@'%' }
    rxTemp := TRegExpr.Create;
    rxTemp.ModifierI := True;
    rxGrant := TRegExpr.Create;
    rxGrant.ModifierI := True;
    rxGrant.Expression := '^GRANT\s+(.+)\s+ON\s+((TABLE|FUNCTION|PROCEDURE)\s+)?(\*|[`"]([^`"]+)[`"])\.(\*|[`"]([^`"]+)[`"])\s+TO\s+\S+(\s+IDENTIFIED\s+BY\s+(PASSWORD)?\s+''?([^'']+)''?)?(\s+.+)?$';

    for i:=0 to Grants.Count-1 do begin
      // Find selected priv objects via regular expression
      if rxGrant.Exec(Grants[i]) then begin
        P := TPrivObj.Create;
        P.GrantCode := Grants[i];
        P.Added := FAdded;
        FPrivObjects.Add(P);

        if (rxGrant.Match[4] = '*') and (rxGrant.Match[6] = '*') then begin
          P.DBObj.NodeType := lntNone;
          P.AllPrivileges := PrivsGlobal;
          // http://dev.mysql.com/doc/refman/5.7/en/show-grants.html
          // As of MySQL 5.7.6, SHOW GRANTS output does not include IDENTIFIED BY PASSWORD clauses.
          // Use the SHOW CREATE USER statement instead. See Section 14.7.5.12, "SHOW CREATE USER Syntax".
          if FConnection.Parameters.IsMySQL and (FConnection.ServerVersionInt < 50706) then begin
            if not FAdded then begin
              editPassword.TextHint := FConnection.UnescapeString(rxGrant.Match[10]);
              // Set password for changed user, to silence the error message about invalid length
              User.Password := editPassword.TextHint;
            end else begin
              // Set password for cloned user
              User.Password := FConnection.UnescapeString(rxGrant.Match[10]);
              editPassword.Text := User.Password;
              editRepeatPassword.Text := User.Password;
              editPassword.Modified := True;
            end;
          end;
        end else if (rxGrant.Match[6] = '*') then begin
          P.DBObj.NodeType := lntDb;
          P.DBObj.Database := rxGrant.Match[5];
          P.AllPrivileges := PrivsDb;
        end else begin
          P.DBObj.Database := rxGrant.Match[5];
          P.DBObj.Name := rxGrant.Match[7];
          if UpperCase(rxGrant.Match[3]) = 'FUNCTION' then begin
            P.DBObj.NodeType := lntFunction;
            P.AllPrivileges := PrivsRoutine;
          end else if (UpperCase(rxGrant.Match[3]) = 'PROCEDURE') then begin
            P.DBObj.NodeType := lntProcedure;
            P.AllPrivileges := PrivsRoutine;
          end else begin
            Obj := P.DBObj.Connection.FindObject(P.DBObj.Database, P.DBObj.Name);
            if (Obj <> nil) and (Obj.NodeType = lntView) then
              P.DBObj.NodeType := lntView
            else
              P.DBObj.NodeType := lntTable;
            P.AllPrivileges := PrivsTable;
          end;
        end;

        // Find selected privileges
        { USAGE
          SELECT, SELECT (id, colname), INSERT, INSERT (id, colname), UPDATE, UPDATE (colname), DELETE, CREATE
          EXECUTE, ALTER ROUTINE }
        if rxGrant.Match[1] = 'ALL PRIVILEGES' then begin
          P.OrgPrivs.AddStrings(P.AllPrivileges);
          P.OrgPrivs.Delete(P.OrgPrivs.IndexOf('GRANT'));
        end else begin
          rxTemp.Expression := '\b('+ImplodeStr('|', AllPnames)+')(\s+\(([^\)]+)\))?,';
          if rxTemp.Exec(rxGrant.Match[1]+',') then while True do begin
            if rxTemp.Match[3] = '' then
              P.OrgPrivs.Add(rxTemp.Match[1])
            else begin
              // Find previously created column priv or create new one
              Cols := Explode(',', rxTemp.Match[3]);
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
                PCol.OrgPrivs.Add(rxTemp.Match[1]);
                PCol.GrantCode := PCol.GrantCode + rxTemp.Match[1] + ' ('+Trim(Cols[j])+')' + ', ';
              end;
              Cols.Free;

            end;
            if not rxTemp.ExecNext then
              break;
          end;

        end;

        // REQUIRE SSL X509 ISSUER '456' SUBJECT '789' CIPHER '123' NONE
        rxTemp.Expression := '\sREQUIRE\s+(.+)';
        if rxTemp.Exec(rxGrant.Match[11]) then begin
          RequireClause := rxTemp.Match[1];
          User.SSL := 0;
          User.Cipher := '';
          User.Issuer := '';
          User.Subject := '';
          rxTemp.Expression := '\bSSL\b';
          if rxTemp.Exec(RequireClause) then
            User.SSL := 1;
          rxTemp.Expression := '\bX509\b';
          if rxTemp.Exec(RequireClause) then
            User.SSL := 2;
          rxTemp.Expression := '\bCIPHER\s+''([^'']+)';
          if rxTemp.Exec(RequireClause) then
            User.Cipher := rxTemp.Match[1];
          rxTemp.Expression := '\bISSUER\s+''([^'']+)';
          if rxTemp.Exec(RequireClause) then
            User.Issuer := rxTemp.Match[1];
          rxTemp.Expression := '\bSUBJECT\s+''([^'']+)';
          if rxTemp.Exec(RequireClause) then
            User.Subject := rxTemp.Match[1];
          if IsNotEmpty(User.Cipher) or IsNotEmpty(User.Issuer) or IsNotEmpty(User.Subject) then
            User.SSL := 3;
          comboSSL.ItemIndex := User.SSL;
          comboSSL.OnChange(Sender);
          editCipher.Text := User.Cipher;
          editIssuer.Text := User.Issuer;
          editSubject.Text := User.Subject;
        end;

        // WITH .. GRANT OPTION
        // MAX_QUERIES_PER_HOUR 20 MAX_UPDATES_PER_HOUR 10 MAX_CONNECTIONS_PER_HOUR 5 MAX_USER_CONNECTIONS 2
        rxTemp.Expression := '\sWITH\s+(.+)';
        if rxTemp.Exec(rxGrant.Match[11]) then begin
          WithClause := rxTemp.Match[1];
          if ExecRegExpr('\bGRANT\s+OPTION\b', WithClause) then
            P.OrgPrivs.Add('GRANT');
          rxTemp.Expression := '\bMAX_QUERIES_PER_HOUR\s+(\d+)\b';
          if rxTemp.Exec(WithClause) then
            User.MaxQueries := MakeInt(rxTemp.Match[1]);
          rxTemp.Expression := '\bMAX_UPDATES_PER_HOUR\s+(\d+)\b';
          if rxTemp.Exec(WithClause) then
            User.MaxUpdates := MakeInt(rxTemp.Match[1]);
          rxTemp.Expression := '\bMAX_CONNECTIONS_PER_HOUR\s+(\d+)\b';
          if rxTemp.Exec(WithClause) then
            User.MaxConnections := MakeInt(rxTemp.Match[1]);
          rxTemp.Expression := '\bMAX_USER_CONNECTIONS\s+(\d+)\b';
          if rxTemp.Exec(WithClause) then
            User.MaxUserConnections := MakeInt(rxTemp.Match[1]);
          udMaxQueries.Position := User.MaxQueries;
          udMaxUpdates.Position := User.MaxUpdates;
          udMaxConnections.Position := User.MaxConnections;
          udMaxUserConnections.Position := User.MaxUserConnections;
        end;

        if (P.OrgPrivs.Count = 0) and (P.DBObj.NodeType = lntTable) then
          FPrivObjects.Remove(P);
      end;
    end;

    // Generate grant code for column privs by hand
    for Ptmp in FPrivObjects do begin
      if Ptmp.DBObj.NodeType = lntColumn then begin
        Ptmp.GrantCode := 'GRANT ' + Copy(Ptmp.GrantCode, 1, Length(Ptmp.GrantCode)-2) + ' ON ' +
          Ptmp.DBObj.QuotedDatabase + '.' +
          Ptmp.DBObj.QuotedName +
          ' TO ' + UserHost;
      end;
      // Flag all privs as added, so "Save" action applies them
      if Assigned(CloneGrants) then
        Ptmp.AddedPrivs.AddStrings(Ptmp.OrgPrivs);
    end;

    FPrivObjects.Sort;
    rxGrant.Free;
    rxTemp.Free;
    FreeAndNil(Grants);
    FreeAndNil(CloneGrants);
    FreeAndNil(AllPnames);
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
  tabCredentials.Enabled := UserSelected;
  lblMaxQueries.Enabled := UserSelected and (FConnection.ServerVersionInt >= 40002);

  tabLimitations.Enabled := UserSelected;
  editMaxQueries.Enabled := lblMaxQueries.Enabled;
  udMaxQueries.Enabled := lblMaxQueries.Enabled;
  lblMaxUpdates.Enabled := lblMaxQueries.Enabled;
  editMaxUpdates.Enabled := lblMaxQueries.Enabled;
  udMaxUpdates.Enabled := lblMaxQueries.Enabled;
  lblMaxConnections.Enabled := lblMaxQueries.Enabled;
  editMaxConnections.Enabled := lblMaxQueries.Enabled;
  udMaxConnections.Enabled := lblMaxQueries.Enabled;
  lblMaxUserConnections.Enabled := UserSelected and (FConnection.ServerVersionInt >= 50003);
  editMaxUserConnections.Enabled := lblMaxUserConnections.Enabled;
  udMaxUserConnections.Enabled := lblMaxUserConnections.Enabled;

  tabSSL.Enabled := UserSelected;
  comboSSL.Enabled := UserSelected;

  btnAddObject.Enabled := UserSelected;
  btnDeleteUser.Enabled := UserSelected;
  btnCloneUser.Enabled := UserSelected and (not FAdded);

  // Ensure the warning hint is displayed or cleared. This is not done when the dialog shows up.
  listUsers.OnHotChange(Sender, nil, Node);
end;


procedure TUserManagerForm.listUsersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
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
  Mainform.AnyGridHeaderClick(Sender, HitInfo);
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
    Msg := '';
    case User.Problem of
      upEmptyPassword:
        Msg := _('This user has an empty password.');
      upInvalidPasswordLen:
        Msg := f_('This user is inactive due to an invalid length of its encrypted password. Please fix that in the %s table.', ['mysql.user']);
      upSkipNameResolve:
        Msg := f_('This user is inactive due to having a host name, while the server runs with %s.', ['--skip-name-resolve']);
      upUnknown:
        Msg := _('This user is inactive due to some unknown reason.');
    end;
  end;
  lblWarning.Caption := Msg;
end;


procedure TUserManagerForm.listUsersCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
begin
  Mainform.AnyGridCompareNodes(Sender, Node1, Node2, Column, Result);
end;


procedure TUserManagerForm.listUsersInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  User: PUser;
begin
  User := Sender.GetNodeData(Node);
  User^ := FUsers[Node.Index];
  if not (User.Problem in [upNone, upEmptyPassword]) then
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
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
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
          CellText := _('Global privileges');
        lntDb:
          CellText := _('Database')+': '+p.DBObj.Database;
        lntTable, lntView, lntProcedure, lntFunction:
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
  OldUser, NodeUser: PUser;
  Node: PVirtualNode;
  NewHost, NewPassword, NewUsername: String;
begin
  // Create new or clone existing user
  if Sender = btnCloneUser then begin
    CloneGrants := TStringList.Create;
    for P in FPrivObjects do
      CloneGrants.Add(P.GrantCode);
    OldUser := listUsers.GetNodeData(listUsers.FocusedNode);
    NewHost := OldUser.Host;
    NewUsername := OldUser.Username;
    NewPassword := OldUser.Password;
  end else begin
    NewHost := 'localhost';
    NewUsername := _('Unnamed');
    NewPassword := '';
  end;
  // Try to unfocus current user which triggers saving modifications.
  listUsers.FocusedNode := nil;
  if Assigned(listUsers.FocusedNode) then
    Exit;
  User := TUser.Create;
  User.Username := NewUsername;
  User.Host := NewHost;
  User.Password := NewPassword;
  FUsers.Add(User);
  FAdded := True;
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
  PageControlSettings.ActivePage := tabCredentials;
  editUserName.SetFocus;
end;


procedure TUserManagerForm.btnAddObjectClick(Sender: TObject);
var
  DBObjects: TDBObjectList;
  DBObject: TDBObject;
  Priv: TPrivObj;
  Node, Child: PVirtualNode;
  ObjectExists: Boolean;
begin
  // Add new privilege object(s)
  DBObjects := SelectDBObjects;
  if (not Assigned(DBObjects)) or (DBObjects.Count = 0) then
    Exit;
  for DBObject in DBObjects do begin

    // Check for unsupported object type, selectable in tree
    if not (DBObject.NodeType in [lntDb, lntTable, lntView, lntFunction, lntProcedure, lntColumn]) then begin
      ErrorDialog(f_('Objects of type %s cannot be part of privileges.', [_(DBObject.ObjType)]));
      Continue;
    end;
    // Check if this would be a duplicate object
    ObjectExists := False;
    for Priv in FPrivObjects do begin
      if Priv.DBObj.IsSameAs(DBObject) then
        ObjectExists := True;
    end;
    if ObjectExists then begin
      ErrorDialog(_('Selected object is already accessible.'));
      Continue;
    end;

    Priv := TPrivObj.Create;
    Priv.DBObj := DBObject;
    case Priv.DBObj.NodeType of
      lntNone: Priv.AllPrivileges := PrivsGlobal;
      lntDb: Priv.AllPrivileges := PrivsDb;
      lntTable, lntView: Priv.AllPrivileges := PrivsTable;
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
end;


procedure TUserManagerForm.btnSaveClick(Sender: TObject);
var
  UserHost, OrgUserHost, Create, Table, Revoke, Grant, OnObj, RequireClause: String;
  User: TUser;
  FocusedUser: PUser;
  Tables, WithClauses: TStringList;
  P: TPrivObj;
  i: Integer;
  PasswordSet: Boolean;

  function GetObjectType(ObjType: String): String;
  begin
    // Decide if object type can be part of a GRANT or REVOKE query
    Result := '';
    if FConnection.ServerVersionInt >= 50006 then
      Result := UpperCase(ObjType) + ' ';
  end;

begin
  // Save changes
  FocusedUser := listUsers.GetNodeData(listUsers.FocusedNode);
  if FAdded then begin
    FocusedUser.Username := editUsername.Text;
    FocusedUser.Host := editFromHost.Text;
    if IsEmpty(editPassword.Text) then
      FocusedUser.Problem := upEmptyPassword;
  end else begin
    if (FocusedUser.Problem=upNone)
      and editPassword.Modified
      and IsEmpty(editPassword.Text)
      then
      FocusedUser.Problem := upEmptyPassword
  end;

  OrgUserHost := esc(FocusedUser.Username)+'@'+esc(FocusedUser.Host);
  UserHost := esc(editUsername.Text)+'@'+esc(editFromHost.Text);

  try
    // Ensure we have a unique user@host combination
    for User in FUsers do begin
      if User = FocusedUser^ then
        Continue;
      if (User.Username = editUsername.Text) and (User.Host = editFromHost.Text) then
        raise EInputError.CreateFmt('User <%s@%s> already exists.', [editUsername.Text, editFromHost.Text]);
    end;

    // Check input: Ensure we have a unique user@host combination
    if editPassword.Text <> editRepeatPassword.Text then
      raise EInputError.Create(_('Repeated password does not match first one.'));

    // Create added user
    PasswordSet := False;
    if FAdded and (FConnection.ServerVersionInt >= 50002) then begin
      Create := 'CREATE USER '+UserHost;
      if editPassword.Modified then begin
        // Add "PASSWORD" clause when it's a hash already
        if (Copy(editPassword.Text, 1, 1) = '*') and (Length(editPassword.Text) = 41) then
          Create := Create + ' IDENTIFIED BY PASSWORD '+esc(editPassword.Text)
        else
          Create := Create + ' IDENTIFIED BY '+esc(editPassword.Text);
      end;
      FConnection.Query(Create);
      PasswordSet := True;
    end;

    // Grant added privileges and revoke deleted ones
    for P in FPrivObjects do begin

      case P.DBObj.NodeType of
        lntNone:
          OnObj := '*.*';
        lntDb:
          OnObj := P.DBObj.QuotedDatabase + '.*';
        lntTable, lntFunction, lntProcedure:
          OnObj := GetObjectType(P.DBObj.ObjType) + P.DBObj.QuotedDbAndTableName;
        lntView:
          OnObj := GetObjectType('TABLE') + P.DBObj.QuotedDbAndTableName;
        lntColumn:
          OnObj := GetObjectType('TABLE') + P.DBObj.QuotedDbAndTableName;
        else
          raise Exception.CreateFmt(_('Unhandled privilege object: %s'), [_(P.DBObj.ObjType)]);
      end;

      // Revoke privileges
      if (not P.Added) and (P.DeletedPrivs.Count > 0) then begin
        Revoke := '';
        for i:=0 to P.DeletedPrivs.Count-1 do begin
          Revoke := Revoke + P.DeletedPrivs[i];
          if P.DeletedPrivs[i] = 'GRANT' then
            Revoke := Revoke + ' OPTION';
          if P.DBObj.NodeType = lntColumn then
            Revoke := Revoke + '('+P.DBObj.QuotedColumn+')';
          Revoke := Revoke + ', ';
        end;
        Delete(Revoke, Length(Revoke)-1, 1);
        Revoke := 'REVOKE ' + Revoke + ' ON ' + OnObj + ' FROM ' + OrgUserHost;
        FConnection.Query(Revoke);
      end;

      // Grant privileges. Must be applied with USAGE for added users without specific privs.
      Grant := '';
      for i:=0 to P.AddedPrivs.Count-1 do begin
        if P.AddedPrivs[i] = 'GRANT' then
          Continue;
        Grant := Grant + P.AddedPrivs[i];
        if P.DBObj.NodeType = lntColumn then
          Grant := Grant + '('+P.DBObj.QuotedColumn+')';
        Grant := Grant + ', ';
      end;
      Delete(Grant, Length(Grant)-1, 1);
      if Grant = '' then
        Grant := 'USAGE';
      Grant := 'GRANT ' + Grant + ' ON ' + OnObj + ' TO ' + OrgUserHost;

      // SSL options
      if P.DBObj.NodeType = lntNone then begin
        RequireClause := ' REQUIRE ';
        case comboSSL.ItemIndex of
          0: RequireClause := RequireClause + 'NONE';
          1: RequireClause := RequireClause + 'SSL';
          2: RequireClause := RequireClause + 'X509';
          3: RequireClause := RequireClause + 'CIPHER '+esc(editCipher.Text)+' ISSUER '+esc(editIssuer.Text)+' SUBJECT '+esc(editSubject.Text);
        end;
        if (FocusedUser.SSL = comboSSL.ItemIndex)
          and (FocusedUser.Cipher = editCipher.Text)
          and (FocusedUser.Issuer = editIssuer.Text)
          and (FocusedUser.Subject = editSubject.Text)
          then
          RequireClause := '';
        Grant := Grant + RequireClause;
      end;

      WithClauses := TStringList.Create;
      if P.AddedPrivs.IndexOf('GRANT') > -1 then
        WithClauses.Add('GRANT OPTION');
      if P.DBObj.NodeType = lntNone then begin
        // Apply resource limits only to global privilege
        if udMaxQueries.Position <> FocusedUser.MaxQueries then
          WithClauses.Add('MAX_QUERIES_PER_HOUR '+IntToStr(udMaxQueries.Position));
        if udMaxUpdates.Position <> FocusedUser.MaxUpdates then
          WithClauses.Add('MAX_UPDATES_PER_HOUR '+IntToStr(udMaxUpdates.Position));
        if udMaxConnections.Position <> FocusedUser.MaxConnections then
          WithClauses.Add('MAX_CONNECTIONS_PER_HOUR '+IntToStr(udMaxConnections.Position));
        if udMaxUserConnections.Position <> FocusedUser.MaxUserConnections then
          WithClauses.Add('MAX_USER_CONNECTIONS '+IntToStr(udMaxUserConnections.Position));
      end;
      if WithClauses.Count > 0 then
        Grant := Grant + ' WITH ' + ImplodeStr(' ', WithClauses);

      if P.Added or (P.AddedPrivs.Count > 0) or (WithClauses.Count > 0) or (RequireClause <> '') then
        FConnection.Query(Grant);

      WithClauses.Free;
    end;

    // Set password
    if editPassword.Modified and (not PasswordSet) then begin
      if (not FConnection.Parameters.IsMariaDB) and (FConnection.ServerVersionInt >= 50706) then
        FConnection.Query('SET PASSWORD FOR ' + OrgUserHost + ' = '+esc(editPassword.Text))
      else
        FConnection.Query('SET PASSWORD FOR ' + OrgUserHost + ' = PASSWORD('+esc(editPassword.Text)+')');
    end;

    // Rename user
    if (FocusedUser.Username <> editUsername.Text) or (FocusedUser.Host <> editFromHost.Text) then begin
      if FConnection.ServerVersionInt >= 50002 then
        FConnection.Query('RENAME USER '+OrgUserHost+' TO '+UserHost)
      else begin
        Tables := Explode(',', 'user,db,tables_priv,columns_priv');
        for Table in Tables do begin
          FConnection.Query('UPDATE '+FConnection.QuoteIdent('mysql')+'.'+FConnection.QuoteIdent(Table)+
            ' SET User='+esc(editUsername.Text)+', Host='+esc(editFromHost.Text)+
            ' WHERE User='+esc(FocusedUser.Username)+' AND Host='+esc(FocusedUser.Host)
            );
        end;
        FreeAndNil(Tables);
      end;
    end;

    FConnection.Query('FLUSH PRIVILEGES');
    Modified := False;
    FAdded := False;
    FocusedUser.Username := editUsername.Text;
    FocusedUser.Host := editFromHost.Text;
    if editPassword.Modified then
      FocusedUser.Password := editPassword.Text;
    FocusedUser.SSL := comboSSL.ItemIndex;
    FocusedUser.Cipher := editCipher.Text;
    FocusedUser.Issuer := editIssuer.Text;
    FocusedUser.Subject := editSubject.Text;
    listUsers.OnFocusChanged(listUsers, listUsers.FocusedNode, listUsers.FocusedColumn);
  except
    on E:EDbError do
      ErrorDialog(E.Message);
    on E:EInputError do
      ErrorDialog(E.Message);
  end;

end;


procedure TUserManagerForm.comboSSLChange(Sender: TObject);
begin
  // Enable custom SSL settings
  lblCipher.Enabled := (comboSSL.ItemIndex = 3) and Assigned(listUsers.FocusedNode);
  editCipher.Enabled := lblCipher.Enabled;
  lblIssuer.Enabled := lblCipher.Enabled;
  editIssuer.Enabled := lblCipher.Enabled;
  lblSubject.Enabled := lblCipher.Enabled;
  editSubject.Enabled := lblCipher.Enabled;
  Modification(Sender);
end;


procedure TUserManagerForm.btnDeleteUserClick(Sender: TObject);
var
  UserHost: String;
  User: PUser;
begin
  // Delete user
  User := listUsers.GetNodeData(listUsers.FocusedNode);
  if FAdded then begin
    FUsers.Remove(User^);
    listUsers.DeleteNode(listUsers.FocusedNode);
    FAdded := False;
  end else if MessageDialog(f_('Delete user %s@%s?', [User.Username, User.Host]), mtConfirmation, [mbYes, mbCancel]) = mrYes then begin
    UserHost := esc(User.Username)+'@'+esc(User.Host);
    try
      // Revoke privs explicitly, required on old servers.
      // Newer servers only require one DROP USER query
      if FConnection.ServerVersionInt < 50002 then begin
        FConnection.Query('REVOKE ALL PRIVILEGES ON *.* FROM '+UserHost);
        FConnection.Query('REVOKE GRANT OPTION ON *.* FROM '+UserHost);
      end;
      if FConnection.ServerVersionInt < 40101 then
        FConnection.Query('DELETE FROM mysql.user WHERE User='+esc(User.Username)+' AND Host='+esc(User.Host))
      else
        FConnection.Query('DROP USER '+UserHost);
      FConnection.Query('FLUSH PRIVILEGES');
      FUsers.Remove(User^);
      listUsers.DeleteNode(listUsers.FocusedNode);
    except on E:EDbError do
      ErrorDialog(E.Message);
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


function TUser.HostRequiresNameResolve: Boolean;
var
  rx: TRegExpr;
begin
  rx := TRegExpr.Create;
  rx.Expression := '^(localhost|[\d\.\/\:]+|.*%.*|[\w\d]{4}\:.*)$';
  Result := not rx.Exec(Host);
  rx.Free;
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
