unit usermanager;


// -------------------------------------
// Usermanager
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, CheckLst, ImgList, ExtCtrls, Registry, ZDataset;
  // winsock

type
  TUserManagerForm = class(TForm)
    PageControl1: TPageControl;
    TabSheetAddUser: TTabSheet;
    DBUserTree: TTreeView;
    EditUser: TEdit;
    EditPassword: TEdit;
    EditHost: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    CheckListBoxPrivileges: TCheckListBox;
    CheckBoxAllPrivileges: TCheckBox;
    Label8: TLabel;
    CheckBoxWithGrant: TCheckBox;
    Label9: TLabel;
    Bevel1: TBevel;
    CheckBoxCreateAccount: TCheckBox;
    EditDescription: TEdit;
    Label1: TLabel;
    TabSheetEditUsers: TTabSheet;
    Panel1: TPanel;
    CheckListBoxPrivs: TCheckListBox;
    Label13: TLabel;
    LabelColumn: TLabel;
    LabelTable: TLabel;
    LabelDB: TLabel;
    Label12: TLabel;
    Label11: TLabel;
    LabelPrivileges: TLabel;
    Panel2: TPanel;
    Label2: TLabel;
    TreeViewUsers: TTreeView;
    ButtonSelectAll: TButton;
    ButtonSelectNone: TButton;
    ButtonSet: TButton;
    LabelPleaseSelect: TLabel;
    LabelNoPrivs: TLabel;
    ButtonSelectPrivileges: TButton;
    Panel3: TPanel;
    ButtonClose: TButton;
    ButtonAddUser: TButton;
    LabelUser: TLabel;
    ButtonRevoke: TButton;
    Panel4: TPanel;
    ButtonEditUser: TButton;
    Button1: TButton;
    procedure DBUserTreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBoxAllPrivilegesClick(Sender: TObject);
    procedure ButtonAddUserClick(Sender: TObject);
    procedure CheckBoxCreateAccountClick(Sender: TObject);
    procedure TreeViewUsersDblClick(Sender: TObject);
    procedure TreeViewUsersChange(Sender: TObject; Node: TTreeNode);
    procedure PageControl1Change(Sender: TObject);
    procedure ButtonSelectAllClick(Sender: TObject);
    procedure ButtonSelectNoneClick(Sender: TObject);
    procedure ShowPrivilegesControls(v,w,y: Boolean);
    procedure ButtonSelectPrivilegesClick(Sender: TObject);
    procedure ShowPrivs(node: TTreeNode);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonSetClick(Sender: TObject);
    procedure CheckListBoxPrivsClickCheck(Sender: TObject);
    procedure ButtonRevokeClick(Sender: TObject);
    procedure ButtonEditUserClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure GetResUsers;
    procedure GetResDBs;
    procedure GetResTables;
    procedure GetResColumns;
    function getColumnNamesOrValues( which: String = 'columns' ): TStringList;


  private
    { Private declarations }
    editcurrent           : Boolean;
  public
    { Public declarations }
    User, Host            : String; // Remember for setting privileges
    ZQueryDBs, ZQueryTables, ZQueryColumns, ZQueryUsers,
    ZQueryColumnNames : TZReadOnlyQuery;
  end;

  function UserManagerWindow (AOwner : TComponent; Flags : String = '') : Boolean;

var
  UserManagerForm: TUserManagerForm;


implementation


uses
  main, childwin, helpers, edituser;

{$I const.inc}
{$R *.DFM}

function UserManagerWindow (AOwner : TComponent; Flags : String = '') : Boolean;
var
  f : TUserManagerForm;
  test_result : String;
  cwin : TMDIChild;
begin
  // Test if we can access the privileges database and tables by
  // A. Using the mysql-DB
  cwin := Mainform.Childwin;
  try
    cwin.ExecUseQuery( DBNAME_MYSQL, false );
  except
    MessageDlg('You have no access to the privileges database.', mtError, [mbOK], 0);
    Result := false;
    exit;
  end;

  // B. retrieving a count of all users.
  test_result := cwin.GetVar( 'SELECT COUNT(*) FROM '+mainform.mask(PRIVTABLE_USERS), 0, true, false );
  if test_result = '' then
  begin
    MessageDlg('You have no access to the privileges tables.', mtError, [mbOK], 0);
    Result := false;
    if cwin.ActualDatabase <> '' then
    begin
      cwin.ExecUseQuery( cwin.ActualDatabase );
    end;
    exit;
  end;

  f := TUserManagerForm.Create(AOwner);

  // use this dirty trick to overcome limitations
  UsermanagerForm := f;

  // set flags ...
  Result := (f.ShowModal = mrOK);
  FreeAndNil (f);

  UsermanagerForm := nil; // uhhh 
end;

procedure TUserManagerForm.ButtonCloseClick(Sender: TObject);
begin
  close;
end;


procedure TUserManagerForm.FormShow(Sender: TObject);
var
  i        : Integer;
  tntop, tn1, tnu1, tnu2 : TTreeNode;

//  wsadat : WSAData;
//  host : String;
//  ph : PHostEnt;
begin
  Screen.Cursor := crHourglass;
  DBUserTree.Items.Clear;
  TreeViewUsers.Items.Clear;
  PageControl1.ActivePageIndex := 0;
  ShowPrivilegesControls(false, true, false);
  CheckBoxCreateAccount.Caption := 'Create connection account for ' + APPNAME;

  tnu1 := DBUserTree.Items.Add(nil, 'Global Access');
  tnu1.ImageIndex := 41;
  tnu1.SelectedIndex := tnu1.ImageIndex;
  tntop := Mainform.Childwin.tnodehost;  // tnodehost on childwin
  tn1 := tntop.GetFirstChild;
  for i:=0 to tntop.Count-1 do
  begin
    tnu2 := DBUserTree.Items.AddChild(tnu1, tn1.Text);
    tnu2.ImageIndex := tn1.ImageIndex;
    tnu2.SelectedIndex := tn1.SelectedIndex;
    DBUserTree.Items.AddChild( tnu2, DUMMY_NODE_TEXT );
    tn1 := tntop.getNextChild(tn1);

  end;
  EditUser.Text := Mainform.Childwin.ZQuery3.Connection.User;

  tnu1.Expand(false);
  tnu1.Selected := true;

  with CheckListBoxPrivileges do
    for i:=0 to 8 do
      Checked[i] := true;

  PageControl1.OnChange(self);

{
  // get users hostname
  WSAStartup(MAKEWORD(1, 1), wsadat);
  gethostname(pchar(host), 80);
  ph := gethostbyname(pchar(host));
  EditHost.Text := ph.h_name;
  WSACleanup();
}
  ZQueryDBs := TZReadOnlyQuery.Create(self);
  ZQueryDBs.Connection := Mainform.Childwin.ZQuery3.Connection;

  ZQueryTables := TZReadOnlyQuery.Create(self);
  ZQueryTables.Connection := Mainform.Childwin.ZQuery3.Connection;

  ZQueryColumns := TZReadOnlyQuery.Create(self);
  ZQueryColumns.Connection := Mainform.Childwin.ZQuery3.Connection;

  ZQueryUsers := TZReadOnlyQuery.Create(self);
  ZQueryUsers.Connection := Mainform.Childwin.ZQuery3.Connection;

  ZQueryColumnNames := TZReadOnlyQuery.Create(self);
  ZQueryColumnNames.Connection := Mainform.Childwin.ZQuery3.Connection;

  Screen.Cursor := crDefault;
end;

procedure TUserManagerForm.CheckBoxAllPrivilegesClick(Sender: TObject);
begin
  CheckListBoxPrivileges.Enabled := not CheckBoxAllPrivileges.Checked;
end;

procedure TUserManagerForm.ButtonAddUserClick(Sender: TObject);
var
  i : Integer;
  priv, query, access, fromhost, pass, grant : String;
begin
  // Account
  if CheckBoxCreateAccount.Checked then
  with TRegistry.Create do
  begin
    OpenKey(REGPATH + '\Servers\', false);
    if KeyExists(EditDescription.Text) then
    begin
      MessageDlg('This Description (' + EditDescription.Text + ') is already used.' + CRLF + 'Please specify another description!', mtError, [mbOK], 0);
      EditDescription.SetFocus;
      exit;
    end
    else with TRegistry.Create do
    begin
      OpenKey(REGPATH + '\Servers\' + EditDescription.Text, true);
      WriteString('Host', Mainform.Childwin.ZQuery3.Connection.HostName);
      WriteString('User', EditUser.Text);
      WriteString('Password', encrypt(EditPassword.Text));
      WriteString('Port', IntToStr(Mainform.Childwin.ZQuery3.Connection.Port));
      WriteString('Timeout', '30');
      WriteBool('Compressed', false);
      WriteString('OnlyDBs', '');
      CloseKey;
    end;
  end;

  priv := '';
  if CheckBoxAllPrivileges.Checked then
    priv := 'ALL PRIVILEGES'
  else
    with CheckListBoxPrivileges do
      for i:=0 to Items.Count - 1 do
        if Checked[i] then
        begin
          if priv <> '' then
            priv := priv + ', ';
          priv := priv + Items[i];
        end;
  if priv = '' then
    priv := 'USAGE';

  case DBUserTree.Selected.Level of
    0 : access := '*.*';
    1 : access := MainForm.Mask( DBUserTree.Selected.Text ) + '.*';
    2 : access := MainForm.Mask( DBUserTree.Selected.Text );
  end;

  if EditHost.Text = '' then
    fromhost := '%'
  else
    fromhost := EditHost.Text;

  if EditPassWord.Text <> '' then
    pass := ' IDENTIFIED BY ''' + EditPassWord.Text + '''';

  if CheckBoxWithGrant.Checked then
    grant := ' WITH GRANT OPTION';

  query := 'GRANT ' + priv + ' ON ' + access + ' TO ''' + EditUser.Text + '''@''' + fromhost + '''' + pass + grant;
  Mainform.Childwin.ExecUpdateQuery(query);
  Mainform.Childwin.ExecUpdateQuery('FLUSH PRIVILEGES');
  ShowMessage('User succesfully created.');
end;


procedure TUserManagerForm.CheckBoxCreateAccountClick(Sender: TObject);
begin
  Label1.Enabled := CheckBoxCreateAccount.Checked;
  EditDescription.Enabled := CheckBoxCreateAccount.Checked;
  if (CheckBoxCreateAccount.Checked) and (EditDescription.Text = '') then
  begin
    EditDescription.Text := Mainform.Childwin.Caption;
    EditDescription.SetFocus;
  end;
end;


procedure TUserManagerForm.TreeViewUsersDblClick(Sender: TObject);
var
  tnu, tndb             : TTreeNode;
  i                     : Integer;
  TableNames            : TStringList;
begin
  // Add subitems to TreeNode:

  Screen.Cursor := crHourGlass;

  if not TreeViewUsers.Selected.HasChildren then
  case TreeViewUsers.Selected.Level of

    0 : // add dbs to user-node...
      begin
        tndb := Mainform.Childwin.tnodehost.GetFirstChild;
        for i:=0 to Mainform.Childwin.tnodehost.Count-1 do
        begin
          tnu := TreeViewUsers.Items.AddChild(TreeViewUsers.Selected, tndb.Text);
          tnu.ImageIndex := tndb.ImageIndex;
          tnu.SelectedIndex := tndb.SelectedIndex;
          tndb := Mainform.Childwin.tnodehost.getNextChild(tndb);
        end;
      end;

    1 : // add tables to user-node...
      begin
        TableNames := Mainform.Childwin.GetCol( 'SHOW TABLES FROM ' + MainForm.mask(TreeViewUsers.Selected.Text) );
        for i:=0 to TableNames.Count-1 do
        begin
          with TreeViewUsers.Items.AddChild( TreeViewUsers.Selected, TableNames[i] ) do
          begin
            ImageIndex := 39;
            SelectedIndex := 40;
          end;
        end;
      end;


    2 : // add columns to user-node...
      begin
        // find fields from table
        Mainform.Childwin.GetResults( 'SHOW COLUMNS FROM ' + mainform.mask(TreeViewUsers.Selected.Parent.Text) + '.' + mainform.mask(TreeViewUsers.Selected.Text), ZQueryColumnNames );
        for i:=1 to ZQueryColumnNames.RecordCount do
        begin
          tnu := TreeViewUsers.Items.AddChild(TreeViewUsers.Selected, ZQueryColumnNames.Fields[0].AsString);
          tnu.ImageIndex := 62;
          tnu.SelectedIndex := 62;
          ZQueryColumnNames.Next;
        end;
      end;


  end;

  if not TreeViewUsers.Selected.Expanded then
    TreeViewUsers.Selected.Expand(false);

  Screen.Cursor := crDefault;
end;


procedure TUserManagerForm.TreeViewUsersChange(Sender: TObject;
  Node: TTreeNode);
var
  n             : TTreeNode;
  highlight     : TColor;
begin
  // Selecting a User, DB, Table or Column in TreeViewUsers
  n := Node;
  while n.Parent <> nil do
    n := n.Parent;
  // show controls:
  ShowPrivilegesControls(false, false, true);
  User := copy(n.Text, 0, pos('@', n.Text)-1);
  Host := copy(n.Text, pos('@', n.Text)+1, length(n.text));
  LabelUser.Caption := 'User ''' + User + ''' connecting from host ''' + Host + '''';

  highlight := clNavy;
  case Node.level of
    0 : begin
        LabelDB.Caption := '<All Databases>'; LabelDB.Font.Color := highlight;
        LabelTable.Caption := '<All Tables>'; LabelTable.Font.Color := highlight;
        LabelColumn.Caption := '<All Columns>'; LabelColumn.Font.Color := highlight;
        ButtonEditUser.Enabled := true;
      end;
    1 : begin
        LabelDB.Caption := Node.Text; LabelDB.Font.Color := clWindowText;
        LabelTable.Caption := '<All Tables>'; LabelTable.Font.Color := highlight;
        LabelColumn.Caption := '<All Columns>'; LabelColumn.Font.Color := highlight;
        if not ZQueryDBs.Active then
          Mainform.Childwin.GetResults( 'SELECT * FROM '+mainform.mask(PRIVTABLE_DB), ZQueryDBs );
        ButtonEditUser.Enabled := false;
      end;
    2 : begin
        LabelDB.Caption := Node.Parent.Text; LabelDB.Font.Color := clWindowText;
        LabelTable.Caption := Node.Text; LabelTable.Font.Color := clWindowText;
        LabelColumn.Caption := '<All Columns>'; LabelColumn.Font.Color := highlight;
        if not ZQueryTables.Active then
          Mainform.Childwin.GetResults( 'SELECT * FROM '+mainform.mask(PRIVTABLE_TABLES), ZQueryTables );
        ButtonEditUser.Enabled := false;
      end;
    3 : begin
        LabelDB.Caption := Node.Parent.Parent.Text; LabelDB.Font.Color := clWindowText;
        LabelTable.Caption := Node.Parent.Text; LabelTable.Font.Color := clWindowText;
        LabelColumn.Caption := Node.Text; LabelColumn.Font.Color := clWindowText;
        if not ZQueryColumns.Active then
          Mainform.Childwin.GetResults( 'SELECT * FROM '+mainform.mask(PRIVTABLE_COLUMNS), ZQueryColumns );
        ButtonEditUser.Enabled := false;
      end;
  end;

  editcurrent := true;
  ShowPrivs(node);

end;


procedure TUserManagerForm.PageControl1Change(Sender: TObject);
var
  i   : Integer;
  tn  : TTreeNode;
begin
  // Toggle Button Add User
  ButtonAddUser.Visible := PageControl1.ActivePage = TabSheetAddUser;

  // ---------------------
  // Edit Users:

  // Fill Tree with Registered Users:
  if (PageControl1.ActivePage = TabSheetEditUsers) and
    (TreeViewUsers.Items.Count = 0) then
  begin
    Mainform.Childwin.GetResults( 'SELECT * FROM '+mainform.mask(PRIVTABLE_USERS), ZQueryUsers );
    for i:=1 to ZQueryUsers.RecordCount do
    begin
      tn := TreeViewUsers.Items.AddChild(nil, ZQueryUsers.Fields[1].AsString + '@' + ZQueryUsers.Fields[0].AsString );
      tn.ImageIndex := 61;
      tn.SelectedIndex := 60;
      ZQueryUsers.Next;
    end;
  end;
  // ---------------------
end;

procedure TUserManagerForm.ButtonSelectAllClick(Sender: TObject);
begin
  // Select all
  ToggleCheckListBox(CheckListBoxPrivs, true);
  CheckListBoxPrivs.OnClickCheck(self);
end;

procedure TUserManagerForm.ButtonSelectNoneClick(Sender: TObject);
begin
  // Select none
  ToggleCheckListBox(CheckListBoxPrivs, false);
  CheckListBoxPrivs.OnClickCheck(self);
end;


procedure TUserManagerForm.ShowPrivilegesControls(v, w, y: Boolean);
  function getPrivNames( privtable : String ): TStringList;
  var
    i,j         : Byte;
    q           : TZReadOnlyQuery;
    setlist     : TStringList;
    tmpstr      : String;
  begin
    q := TZReadOnlyQuery.Create(self);
    q.Connection := Mainform.Childwin.ZQuery3.Connection;
    Mainform.Childwin.GetResults( 'SHOW COLUMNS FROM '+mainform.mask(privtable), q );
    result := TStringList.Create;
    for i := 0 to q.RecordCount-1 do
    begin
      if pos( '_priv', q.FieldByName('Field').AsString ) > 0 then
      begin
        // user + db are enum(Y,N)
        if StrCmpBegin( 'enum', q.FieldByName('Type').AsString ) then
        begin
          result.add( q.FieldByName('Field').AsString );
          result[result.count-1] := stringreplace( result[result.count-1], '_priv', '', [] );
        end
        // table + column are set(x,y,z)
        else if StrCmpBegin( 'set', q.FieldByName('Type').AsString ) then
        begin
          tmpstr := getEnumValues(q.FieldByName('Type').AsString);
          tmpstr := StringReplace( tmpstr, '''', '', [rfReplaceAll] );
          setlist := explode(',', tmpstr );
          for j:=0 to setlist.Count-1 do
          begin
            result.Add(q.FieldByName('Field').AsString+': '+setlist[j]);
            result[result.count-1] := stringreplace( result[result.count-1], '_priv', '', [] );
          end;
        end
        //result[i] := uppercase( result[i] );
      end;
      q.next;
    end;
  end;
begin
  // show/hide Privileges-Controls
  // v : some privileges set?
  // w : a user selected?
  // y : no privileges set
  LabelPrivileges.Visible := v;
  Label11.Visible := v;
  Label12.Visible := v;
  Label13.Visible := v;
  LabelUser.Visible := v;
  LabelDB.Visible := v;
  LabelTable.Visible := v;
  LabelColumn.Visible := v;
  CheckListBoxPrivs.Visible := v;
  ButtonSelectAll.Visible := v;
  ButtonSelectNone.Visible := v;
  ButtonSet.Visible := v;
  if v then
    ButtonSet.Enabled := false;
  ButtonRevoke.Visible := v;
  if v then
    ButtonRevoke.Enabled := editcurrent;
  LabelPleaseSelect.Visible := w;
  LabelNoPrivs.Visible := y;
  ButtonSelectPrivileges.Visible := y;

  if TreeViewUsers.Selected <> nil then
  begin
    CheckListBoxPrivs.Clear;
    case TreeViewUsers.Selected.Level of
      0 : begin // General
          CheckListBoxPrivs.Items := getPrivNames( PRIVTABLE_USERS );
          ButtonRevoke.Caption := 'Delete User';
        end;
      1 : begin // DB
          CheckListBoxPrivs.Items := getPrivNames( PRIVTABLE_DB );
          ButtonRevoke.Caption := 'Revoke Privileges';
        end;
      2 : begin // Table
          CheckListBoxPrivs.Items := getPrivNames( PRIVTABLE_TABLES );
          ButtonRevoke.Caption := 'Revoke Privileges';
        end;
      3 : begin // Column
          CheckListBoxPrivs.Items := getPrivNames( PRIVTABLE_COLUMNS );
          ButtonRevoke.Caption := 'Revoke Privileges';
        end;
    end;
  end;
end;


procedure TUserManagerForm.ButtonSelectPrivilegesClick(Sender: TObject);
begin
  // Specify some privileges
  editcurrent := false;
  ShowPrivilegesControls(true, false, false);
end;



procedure TUserManagerForm.ShowPrivs(node: TTreeNode);
var
  i,j       : Integer;
  ColumnName, value : String;
  set_values : TStringList;
begin
  // Show user-privileges (general, db, table or column)
  // depending on node.level

  case Node.Level of
    0 : begin // General user-privileges
        ZQueryUsers.First;
        for i:=1 to ZQueryUsers.RecordCount do
        begin
          if (ZQueryUsers.Fields[0].AsString+'' = Host) and (ZQueryUsers.Fields[1].AsString+'' = User) then
          begin // found the according user!
            ShowPrivilegesControls(true, false, false);
            for j := 0 to CheckListBoxPrivs.count -1 do
            begin
              CheckListBoxPrivs.Checked[j] := ZQueryUsers.FieldByName( CheckListBoxPrivs.Items[j] + '_priv' ).AsBoolean;
            end;
            break;
          end;
          ZQueryUsers.Next;
        end;
      end;

    1 : begin // db-privileges
        ZQueryDBs.First;
        for i:=1 to ZQueryDBs.RecordCount do
        begin
          if (ZQueryDBs.Fields[0].AsString = Host)
            and (ZQueryDBs.Fields[1].AsString = Node.Text)
            and (ZQueryDBs.Fields[2].AsString = User) then
          begin
            // some privs are set:
            ShowPrivilegesControls(true, false, false);
            for j := 0 to CheckListBoxPrivs.count -1 do
            begin
              CheckListBoxPrivs.Checked[j] := ZQueryDbs.FieldByName( CheckListBoxPrivs.Items[j] + '_priv' ).AsBoolean;
            end;
          end;
          ZQueryDBs.Next;
        end;
      end;

    2 : begin // table-privileges
        ZQueryTables.First;
        for i:=1 to ZQueryTables.RecordCount do
        begin
          if (ZQueryTables.Fields[0].AsString = Host)
            and (ZQueryTables.Fields[1].AsString = Node.Parent.Text)
            and (ZQueryTables.Fields[2].AsString = User)
            and (ZQueryTables.Fields[3].AsString = Node.Text) then
          begin // found the according record!
            // some privs are set:
            ShowPrivilegesControls(true, false, false);
            for j := 0 to CheckListBoxPrivs.count -1 do
            begin
              if pos( ':', CheckListBoxPrivs.Items[j] ) > 0 then
              begin
                ColumnName := copy( CheckListBoxPrivs.Items[j], 0, pos( ':', CheckListBoxPrivs.Items[j] )-1 ) + '_priv';
                set_values := Explode( ',', ZQueryTables.FieldByName( ColumnName ).AsString );
                value := copy(CheckListBoxPrivs.Items[j], pos( ':', CheckListBoxPrivs.Items[j] )+2, length(CheckListBoxPrivs.Items[j]));
                CheckListBoxPrivs.Checked[j] := (set_values.IndexOf( value )>-1 );
              end
              else
                CheckListBoxPrivs.Checked[j] := ZQueryTables.FieldByName( CheckListBoxPrivs.Items[j] + '_priv' ).AsBoolean;
            end;
          end;
          ZQueryTables.Next;
        end;
      end;

    3 : begin // column-privileges
        ZQueryColumns.First;
        for i:=1 to ZQueryColumns.RecordCount do
        begin
          if (ZQueryColumns.Fields[0].AsString = Host)
            and (ZQueryColumns.Fields[1].AsString = Node.Parent.Parent.Text)
            and (ZQueryColumns.Fields[2].AsString = User)
            and (ZQueryColumns.Fields[3].AsString = Node.Parent.Text)
            and (ZQueryColumns.Fields[4].AsString = Node.Text) then
          begin
            // some privs are set:
            ShowPrivilegesControls(true, false, false);
            for j := 0 to CheckListBoxPrivs.count -1 do
            begin
              if pos( ':', CheckListBoxPrivs.Items[j] ) > 0 then
              begin
                ColumnName := copy( CheckListBoxPrivs.Items[j], 0, pos( ':', CheckListBoxPrivs.Items[j] )-1 ) + '_priv';
                set_values := Explode( ',', ZQueryColumns.FieldByName( ColumnName ).AsString );
                value := copy(CheckListBoxPrivs.Items[j], pos( ':', CheckListBoxPrivs.Items[j] )+2, length(CheckListBoxPrivs.Items[j]));
                CheckListBoxPrivs.Checked[j] := (set_values.IndexOf( value )>-1 );
              end
              else
              CheckListBoxPrivs.Checked[j] := ZQueryColumns.FieldByName( CheckListBoxPrivs.Items[j] + '_priv' ).AsBoolean;
            end;
          end;
          ZQueryColumns.Next;
        end;
      end;
  end;

end;



// Generate column-names for INSERT
function TUserManagerForm.getColumnNamesOrValues( which: String = 'columns' ): TStringList;
var
  i : Integer;
  ColumnNames, Values, set_values : TStringList;
  ColumnName, privobject : String;
begin
  ColumnNames := TStringList.Create;
  Values := TStringList.Create;
  i := 0;
  while i < CheckListBoxPrivs.Items.Count do
  begin
    if pos( ':', CheckListBoxPrivs.Items[i] ) = 0 then
    begin // user- and db-privs, which are stored in enumns
      ColumnNames.Add( Mainform.Mask( CheckListBoxPrivs.Items[i] + '_priv' ) );
      Values.add( bool2str(CheckListBoxPrivs.Checked[i] ) );
    end
    else
    begin // table- and columns-privs, which are stored in sets
      ColumnName := copy( CheckListBoxPrivs.Items[i], 0, pos( ':', CheckListBoxPrivs.Items[i] )-1 );
      ColumnNames.Add( Mainform.Mask( ColumnName + '_priv' ) );
      set_values := TStringList.Create;
      while i < CheckListBoxPrivs.Items.Count do
      begin
        if not StrCmpBegin( ColumnName+':', CheckListBoxPrivs.Items[i] ) then
        begin
          dec(i);
          break;
        end;
        privobject := copy( CheckListBoxPrivs.Items[i], length(ColumnName)+3, length(CheckListBoxPrivs.Items[i] ) );
        if CheckListBoxPrivs.Checked[i] then
          set_values.Add( privobject );
        inc(i);
      end;
      Values.add( ImplodeStr( ',', set_values ) );
    end;
    inc(i);
  end;
  if which = 'columns' then
    result := ColumnNames
  else
    result := Values;
  
end;




// Grant specified Privileges
procedure TUserManagerForm.ButtonSetClick(Sender: TObject);

  // Generate names and values for UPDATE
  function getUpdateClause : String;
  var
    i            : Byte;
    columns, values   : TStringList;
  begin
    result := '';
    columns := getColumnNamesOrValues( 'columns' );
    values := getColumnNamesOrValues( 'values' );
    if values.Count <> columns.count then
    begin
      MessageDlg( 'Internal Error: ColumnNames.Count <> Values.Count .', mtError, [mbOK], 0 );
      exit;
    end;
    for i := 0 to columns.Count - 1 do
    begin
      if i > 0 then
        result := result + ', ';
      result := result + columns[i] + ' = ''' + values[i] + '''';
    end;
  end;

var
  sql : String;
begin
  Screen.Cursor := crHourglass;
  case TreeViewUsers.Selected.Level of
    0 : begin // general
        sql := 'UPDATE '+mainform.mask(PRIVTABLE_USERS)+' SET ';
        sql := sql + getUpdateClause;
        sql := sql + ' WHERE Host = ''' + Host + ''' AND User = ''' + User + '''';
        Mainform.Childwin.ExecUpdateQuery(sql);
        GetResUsers;
      end;

    1 : begin // db
        if editcurrent then
        begin
          sql := 'UPDATE '+mainform.mask(PRIVTABLE_DB)+' SET ';
          sql := sql + getUpdateClause;
          sql := sql + ' WHERE Host = ''' + Host + ''' AND Db = ''' + TreeViewUsers.Selected.Text + ''' AND User = ''' + User + '''';
        end
        else
        begin
          sql := 'INSERT INTO '+mainform.mask(PRIVTABLE_DB)+' (Host, Db, User, ';
          sql := sql + ImplodeStr( ', ', getColumnNamesOrValues( 'columns' ) );
          sql := sql + ') VALUES (''' + Host + ''', ''' + TreeViewUsers.Selected.Text + ''', ''' + User + ''', ';
          sql := sql + '''' + ImplodeStr( ''', ''', getColumnNamesOrValues( 'values' ) ) + '''';
          sql := sql + ')';
          editcurrent := true;
        end;
        Mainform.Childwin.ExecUpdateQuery(sql);
        GetResDBs;
      end;

    2 : begin // table
        if editcurrent then
        begin
          sql := 'UPDATE '+mainform.mask(PRIVTABLE_TABLES)+' SET ';
          sql := sql + getUpdateClause;
          sql := sql + ' WHERE Host = ''' + Host + ''' AND Db = ''' + TreeViewUsers.Selected.Parent.Text + ''' AND User = ''' + User + ''' AND Table_name = ''' + TreeViewUsers.Selected.Text + '''';
        end
        else
        begin
          sql := 'INSERT INTO '+mainform.mask(PRIVTABLE_TABLES)+' (Host, Db, User, Table_name, Grantor, ';
          sql := sql + ImplodeStr( ', ', getColumnNamesOrValues( 'columns' ) );
          sql := sql + ') VALUES ('''+Host+''','''+TreeViewUsers.Selected.Parent.Text+''','''+User+''','''+TreeViewUsers.Selected.Text+''','''+Mainform.Childwin.ZQuery3.Connection.User+''', ';
          sql := sql + '''' + ImplodeStr( ''', ''', getColumnNamesOrValues( 'values' ) ) + '''';
          sql := sql + ')';
          editcurrent := true;
        end;
        Mainform.Childwin.ExecUpdateQuery(sql);
        GetResTables;
      end;

    3 : begin // column
        if editcurrent then begin
          sql := 'UPDATE '+mainform.mask(PRIVTABLE_COLUMNS)+' SET ';
          sql := sql + getUpdateClause;
          sql := sql + ' WHERE Host = ''' + Host + ''' AND Db = ''' + TreeViewUsers.Selected.Parent.Parent.Text + ''' AND User = ''' + User + ''' AND Table_name = ''' + TreeViewUsers.Selected.Parent.Text + ''' AND Column_name = ''' + TreeViewUsers.Selected.Text + '''';
        end
        else begin
          sql := 'INSERT INTO '+mainform.mask(PRIVTABLE_COLUMNS)+' (Host, Db, User, Table_name, Column_name, ';
          sql := sql + ImplodeStr( ', ', getColumnNamesOrValues( 'columns' ) );
          sql := sql + ') VALUES ('''+Host+''','''+TreeViewUsers.Selected.Parent.Parent.Text+''','''+User+''','''+TreeViewUsers.Selected.Parent.Text+''','''+TreeViewUsers.Selected.Text+''', ';
          sql := sql + '''' + ImplodeStr( ''', ''', getColumnNamesOrValues( 'values' ) ) + '''';
          sql := sql + ')';
          editcurrent := true;
        end;
        Mainform.Childwin.ExecUpdateQuery(sql);
        GetResColumns;
      end;
  end;
  Mainform.Childwin.ExecUpdateQuery('FLUSH PRIVILEGES');
  ButtonRevoke.Enabled := editcurrent;
  Screen.Cursor := crDefault;
end;




procedure TUserManagerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // free memory
  ZQueryUsers.Active := False;
  ZQueryDBs.Active := False;
  ZQueryTables.Active := False;
  ZQueryColumns.Active := False;
  ZQueryColumnNames.Active := False;
  if Mainform.Childwin.ActualDatabase <> '' then
    Mainform.Childwin.ExecUseQuery( Mainform.Childwin.ActualDatabase );
end;


procedure TUserManagerForm.CheckListBoxPrivsClickCheck(Sender: TObject);
begin
  ButtonSet.Enabled := true;
end;


{***
  A database-node is about to be expanded:
  Drop the dummy-node and add all tables
}
procedure TUserManagerForm.DBUserTreeExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  i : Integer;
  TableNames : TStringList;
begin
  if (Node.getFirstChild <> nil) and (Node.getFirstChild.Text = DUMMY_NODE_TEXT) then
  begin
    // Drop dummynode
    for i := Node.Count-1 downto 0 do
      Node.Item[i].delete;
    // Get all tables into dbtree
    TableNames := Mainform.Childwin.GetCol( 'SHOW TABLES FROM ' + MainForm.mask(Node.Text) );
    for i:=0 to TableNames.Count-1 do
    begin
      with DBUserTree.Items.AddChild( Node, TableNames[i] ) do
      begin
        ImageIndex := 39;
        SelectedIndex := 40;
      end;
    end;

  end;
end;

procedure TUserManagerForm.ButtonRevokeClick(Sender: TObject);
var sql : String;
begin
  // Delete some Privs
  Screen.Cursor := crHourglass;
  case TreeViewUsers.Selected.Level of
    0 : // delete user
      if MessageDLG('Delete User '''+User+''' and all its privileges?', mtConfirmation, [mbNo, mbYes], 0) = mrYes then begin
        sql := 'DELETE FROM '+mainform.mask(PRIVTABLE_USERS)+' WHERE Host='''+Host+''' AND User='''+User+'''';
        Mainform.Childwin.ExecUpdateQuery(sql);
        sql := 'DELETE FROM '+mainform.mask(PRIVTABLE_DB)+' WHERE Host='''+Host+''' AND User='''+User+'''';
        Mainform.Childwin.ExecUpdateQuery(sql);
        sql := 'DELETE FROM '+mainform.mask(PRIVTABLE_TABLES)+' WHERE Host='''+Host+''' AND User='''+User+'''';
        Mainform.Childwin.ExecUpdateQuery(sql);
        sql := 'DELETE FROM '+mainform.mask(PRIVTABLE_COLUMNS)+' WHERE Host='''+Host+''' AND User='''+User+'''';
        Mainform.Childwin.ExecUpdateQuery(sql);
        TreeViewUsers.Selected.Delete;
        ZQueryDBs.Active := False;
        ZQueryTables.Active := False;
        ZQueryColumns.Active := False;
        GetResUsers;
        Mainform.Childwin.ExecUpdateQuery('FLUSH PRIVILEGES');
      end;
    1 : // delete db-privs
      begin
        sql := 'DELETE FROM '+mainform.mask(PRIVTABLE_DB)+' WHERE Host='''+Host+''' AND User='''+User+''' AND Db='''+TreeViewUsers.Selected.Text+'''';
        Mainform.Childwin.ExecUpdateQuery(sql);
        ShowPrivilegesControls(false, false, true);
        GetResDBs;
        Mainform.Childwin.ExecUpdateQuery('FLUSH PRIVILEGES');
      end;
    2 : // delete table-privs
      begin
        sql := 'DELETE FROM '+mainform.mask(PRIVTABLE_TABLES)+' WHERE Host='''+Host+''' AND User='''+User+''' AND Db='''+TreeViewUsers.Selected.Parent.Text+''' AND Table_name='''+TreeViewUsers.Selected.Text+'''';
        Mainform.Childwin.ExecUpdateQuery(sql);
        ShowPrivilegesControls(false, false, true);
        GetResTables;
        Mainform.Childwin.ExecUpdateQuery('FLUSH PRIVILEGES');
      end;
    3 : // delete column-privs
      begin
        sql := 'DELETE FROM '+mainform.mask(PRIVTABLE_COLUMNS)+' WHERE Host='''+Host+''' AND User='''+User+''' AND Db='''+TreeViewUsers.Selected.Parent.Parent.Text+''' AND Table_name='''+TreeViewUsers.Selected.Parent.Text+''' AND Column_name='''+TreeViewUsers.Selected.Text+'''';
        Mainform.Childwin.ExecUpdateQuery(sql);
        ShowPrivilegesControls(false, false, true);
        GetResColumns;
        Mainform.Childwin.ExecUpdateQuery('FLUSH PRIVILEGES');
      end;
  end;
  Screen.Cursor := crDefault;
end;

procedure TUserManagerForm.ButtonEditUserClick(Sender: TObject);
begin
  FormEditUser.ShowModal;
end;

procedure TUserManagerForm.Button1Click(Sender: TObject);
begin
  // free memory
  ZQueryUsers.Active := False;
  ZQueryDBs.Active := False;
  ZQueryTables.Active := False;
  ZQueryColumns.Active := False;
  ZQueryColumnNames.Active := False;
  ShowPrivilegesControls(false, true, false);
  TreeViewUsers.Items.Clear;
  PageControl1.OnChange(self);
end;


procedure TUserManagerForm.GetResUsers;
begin
  Mainform.Childwin.GetResults( 'SELECT * FROM '+mainform.mask(PRIVTABLE_USERS), ZQueryUsers );
end;

procedure TUserManagerForm.GetResDBs;
begin
  Mainform.Childwin.GetResults( 'SELECT * FROM '+mainform.mask(PRIVTABLE_DB), ZQueryDBs );
end;

procedure TUserManagerForm.GetResTables;
begin
  Mainform.Childwin.GetResults( 'SELECT * FROM '+mainform.mask(PRIVTABLE_TABLES), ZQueryTables );
end;

procedure TUserManagerForm.GetResColumns;
begin
  Mainform.Childwin.GetResults( 'SELECT * FROM '+mainform.mask(PRIVTABLE_COLUMNS), ZQueryColumns );
end;

end.
