unit usermanager;


// -------------------------------------
// HeidiSQL
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
    ImageList1: TImageList;
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

  private
    { Private declarations }
    editcurrent           : Boolean;
  public
    { Public declarations }
    User, Host            : String; // Remember for setting privileges
    ZQueryDBs, ZQueryTables, ZQueryColumns, ZQueryUsers, ZQueryF : TZReadOnlyQuery;
  end;

var
  UserManagerForm: TUserManagerForm;


implementation


uses
  main, childwin, helpers, edituser;


const
  crlf = #13#10;
{$R *.DFM}


procedure TUserManagerForm.ButtonCloseClick(Sender: TObject);
begin
  close;
end;


procedure TUserManagerForm.FormShow(Sender: TObject);
var
  i,j        : Integer;
  tntop, tn1, tn2, tnu1, tnu2, tnu3 : TTreeNode;

//  wsadat : WSAData;
//  host : String;
//  ph : PHostEnt;
begin
  Screen.Cursor := crHourglass;
  DBUserTree.Items.Clear;
  TreeViewUsers.Items.Clear;
  PageControl1.ActivePageIndex := 0;
  ShowPrivilegesControls(false, true, false);

  tnu1 := DBUserTree.Items.Add(nil, 'Global Access');
  tnu1.ImageIndex := 13;
  tnu1.SelectedIndex := 6;
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    DBUserTree.Images := ImageList1;
    tntop := tnodehost;  // tnodehost on childwin
    tn1 := tntop.GetFirstChild;
    for i:=0 to tntop.Count-1 do
    begin
      tnu2 := DBUserTree.Items.AddChild(tnu1, tn1.Text);
      tnu2.ImageIndex := 12;
      tnu2.SelectedIndex := 0;
      for j:=0 to tn1.Count-1 do
      begin
        tn2 := tntop.Item[i].Item[j];
        tnu3 := DBUserTree.Items.AddChild(tnu2, tn1.Text + '.' + tn2.Text);
        tnu3.ImageIndex := 1;
        tnu3.SelectedIndex := 11;
      end;
      tn1 := tntop.getNextChild(tn1);

      TreeViewUsers.Images := ImageList1;
    end;
    EditUser.Text := ZConn.User;

  end;
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
  TMDIChild(Application.Mainform.ActiveMDIChild).ExecQuery( 'USE mysql' );

  ZQueryDBs := TZReadOnlyQuery.Create(self);
  ZQueryDBs.Connection := TMDIChild(Application.Mainform.ActiveMDIChild).ZConn;

  ZQueryTables := TZReadOnlyQuery.Create(self);
  ZQueryTables.Connection := TMDIChild(Application.Mainform.ActiveMDIChild).ZConn;

  ZQueryColumns := TZReadOnlyQuery.Create(self);
  ZQueryColumns.Connection := TMDIChild(Application.Mainform.ActiveMDIChild).ZConn;

  ZQueryUsers := TZReadOnlyQuery.Create(self);
  ZQueryUsers.Connection := TMDIChild(Application.Mainform.ActiveMDIChild).ZConn;

  ZQueryF := TZReadOnlyQuery.Create(self);
  ZQueryF.Connection := TMDIChild(Application.Mainform.ActiveMDIChild).ZConn;

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
    OpenKey(regpath + '\Servers\', false);
    if KeyExists(EditDescription.Text) then
    begin
      MessageDlg('This Description (' + EditDescription.Text + ') is already used.' + crlf + 'Please specify another description!', mtError, [mbOK], 0);
      EditDescription.SetFocus;
      exit;
    end
    else with TRegistry.Create do
    begin
      OpenKey(regpath + '\Servers\' + EditDescription.Text, true);
      WriteString('Host', TMDIChild(Application.Mainform.ActiveMDIChild).ZConn.HostName);
      WriteString('User', EditUser.Text);
      WriteString('Password', encrypt(EditPassword.Text));
      WriteString('Port', IntToStr(TMDIChild(Application.Mainform.ActiveMDIChild).ZConn.Port));
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
    1 : access := DBUserTree.Selected.Text + '.*';
    2 : access := DBUserTree.Selected.Text;
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
  TMDIChild(Application.Mainform.ActiveMDIChild).ExecQuery(query);
  TMDIChild(Application.Mainform.ActiveMDIChild).ExecQuery('FLUSH PRIVILEGES');
  ShowMessage('User succesfully created.');
end;


procedure TUserManagerForm.CheckBoxCreateAccountClick(Sender: TObject);
begin
  Label1.Enabled := CheckBoxCreateAccount.Checked;
  EditDescription.Enabled := CheckBoxCreateAccount.Checked;
  if (CheckBoxCreateAccount.Checked) and (EditDescription.Text = '') then
  begin
    EditDescription.Text := TMDIChild(Application.Mainform.ActiveMDIChild).Caption;
    EditDescription.SetFocus;
  end;
end;


procedure TUserManagerForm.TreeViewUsersDblClick(Sender: TObject);
var
  tnu, tndb, tntbl      : TTreeNode;
  i                     : Integer;
begin
  // Add subitems to TreeNode:

  Screen.Cursor := crHourGlass;

  if not TreeViewUsers.Selected.HasChildren then
  case TreeViewUsers.Selected.Level of

    0 : // add dbs to user-node...
      with TMDIChild(Application.Mainform.ActiveMDIChild) do begin
        tndb := tnodehost.GetFirstChild;
        for i:=0 to tnodehost.Count-1 do begin
          tnu := TreeViewUsers.Items.AddChild(TreeViewUsers.Selected, tndb.Text);
          tnu.ImageIndex := 12;
          tnu.SelectedIndex := 0;
          tndb := tnodehost.getNextChild(tndb);
        end;
      end;

    1 : // add tables to user-node...
      with TMDIChild(Application.Mainform.ActiveMDIChild) do begin
        tndb := tnodehost.GetFirstChild;
        // find according db in dbtree
        for i:=0 to tnodehost.Count-1 do begin
          if tndb.Text = TreeViewUsers.Selected.Text then
            break;
          tndb := tnodehost.getNextChild(tndb);
        end;

        tntbl := tndb.GetFirstChild;
        for i:=0 to tndb.Count-1 do begin
          tnu := TreeViewUsers.Items.AddChild(TreeViewUsers.Selected, tntbl.Text);
          tnu.ImageIndex := 1;
          tnu.SelectedIndex := 11;
          tntbl := tndb.getNextChild(tntbl);
        end;
      end;


    2 : // add columns to user-node...
      begin
        // find fields from table
        TMDIChild(Mainform.ActiveMDIChild).GetResults( 'SHOW FIELDS FROM ' + TreeViewUsers.Selected.Parent.Text + '.' + TreeViewUsers.Selected.Text, ZQueryF );
        for i:=1 to ZQueryF.RecordCount do
        begin
          tnu := TreeViewUsers.Items.AddChild(TreeViewUsers.Selected, ZQueryF.Fields[0].AsString);
          tnu.ImageIndex := 17;
          tnu.SelectedIndex := 18;
          ZQueryF.Next;
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
          TMDIChild(Mainform.ActiveMDIChild).GetResults( 'SELECT * FROM mysql.db', ZQueryDBs );
        ButtonEditUser.Enabled := false;
      end;
    2 : begin
        LabelDB.Caption := Node.Parent.Text; LabelDB.Font.Color := clWindowText;
        LabelTable.Caption := Node.Text; LabelTable.Font.Color := clWindowText;
        LabelColumn.Caption := '<All Columns>'; LabelColumn.Font.Color := highlight;
        if not ZQueryTables.Active then
          TMDIChild(Mainform.ActiveMDIChild).GetResults( 'SELECT * FROM mysql.tables_priv', ZQueryTables );
        ButtonEditUser.Enabled := false;
      end;
    3 : begin
        LabelDB.Caption := Node.Parent.Parent.Text; LabelDB.Font.Color := clWindowText;
        LabelTable.Caption := Node.Parent.Text; LabelTable.Font.Color := clWindowText;
        LabelColumn.Caption := Node.Text; LabelColumn.Font.Color := clWindowText;
        if not ZQueryColumns.Active then
          TMDIChild(Mainform.ActiveMDIChild).GetResults( 'SELECT * FROM mysql.columns_priv', ZQueryColumns );
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
    TMDIChild(Mainform.ActiveMDIChild).GetResults( 'SELECT * FROM mysql.user', ZQueryUsers );
    for i:=1 to ZQueryUsers.RecordCount do
    begin
      tn := TreeViewUsers.Items.AddChild(nil, ZQueryUsers.Fields[1].AsString + '@' + ZQueryUsers.Fields[0].AsString );
      tn.ImageIndex := 16;
      tn.SelectedIndex := 15;
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
  case TreeViewUsers.Selected.Level of
    0 : begin // General
        CheckListBoxPrivs.Clear;
        CheckListBoxPrivs.Items.Add('Select');
        CheckListBoxPrivs.Items.Add('Insert');
        CheckListBoxPrivs.Items.Add('Update');
        CheckListBoxPrivs.Items.Add('Delete');
        CheckListBoxPrivs.Items.Add('Create');
        CheckListBoxPrivs.Items.Add('Drop');
        CheckListBoxPrivs.Items.Add('Reload');
        CheckListBoxPrivs.Items.Add('Shutdown');
        CheckListBoxPrivs.Items.Add('Process');
        CheckListBoxPrivs.Items.Add('File');
        CheckListBoxPrivs.Items.Add('Grant');
        CheckListBoxPrivs.Items.Add('References');
        CheckListBoxPrivs.Items.Add('Index');
        CheckListBoxPrivs.Items.Add('Alter');
        ButtonRevoke.Caption := 'Delete User';
      end;
    1,2 : begin // DB, Table
        CheckListBoxPrivs.Clear;
        CheckListBoxPrivs.Items.Add('Select');
        CheckListBoxPrivs.Items.Add('Insert');
        CheckListBoxPrivs.Items.Add('Update');
        CheckListBoxPrivs.Items.Add('Delete');
        CheckListBoxPrivs.Items.Add('Create');
        CheckListBoxPrivs.Items.Add('Drop');
        CheckListBoxPrivs.Items.Add('Grant');
        CheckListBoxPrivs.Items.Add('References');
        CheckListBoxPrivs.Items.Add('Index');
        CheckListBoxPrivs.Items.Add('Alter');
        ButtonRevoke.Caption := 'Revoke Privileges';
      end;
    3 : begin // Column
        CheckListBoxPrivs.Clear;
        CheckListBoxPrivs.Items.Add('Select');
        CheckListBoxPrivs.Items.Add('Insert');
        CheckListBoxPrivs.Items.Add('Update');
        CheckListBoxPrivs.Items.Add('References');
        ButtonRevoke.Caption := 'Revoke Privileges';
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
            CheckListBoxPrivs.Checked[0] := ZQueryUsers.Fields[3].AsString = 'Y'; // Select
            CheckListBoxPrivs.Checked[1] := ZQueryUsers.Fields[4].AsString = 'Y'; // Insert
            CheckListBoxPrivs.Checked[2] := ZQueryUsers.Fields[5].AsString = 'Y'; // Update
            CheckListBoxPrivs.Checked[3] := ZQueryUsers.Fields[6].AsString = 'Y'; // Delete
            CheckListBoxPrivs.Checked[4] := ZQueryUsers.Fields[7].AsString = 'Y'; // Create
            CheckListBoxPrivs.Checked[5] := ZQueryUsers.Fields[8].AsString = 'Y'; // Drop
            CheckListBoxPrivs.Checked[6] := ZQueryUsers.Fields[9].AsString = 'Y'; // Reload
            CheckListBoxPrivs.Checked[7] := ZQueryUsers.Fields[10].AsString = 'Y'; // Shutdown
            CheckListBoxPrivs.Checked[8] := ZQueryUsers.Fields[11].AsString = 'Y'; // Process
            CheckListBoxPrivs.Checked[9] := ZQueryUsers.Fields[12].AsString = 'Y'; // File
            CheckListBoxPrivs.Checked[10] := ZQueryUsers.Fields[13].AsString = 'Y'; // Grant
            CheckListBoxPrivs.Checked[11] := ZQueryUsers.Fields[14].AsString = 'Y'; // References
            CheckListBoxPrivs.Checked[12] := ZQueryUsers.Fields[15].AsString = 'Y'; // Index
            CheckListBoxPrivs.Checked[13] := ZQueryUsers.Fields[16].AsString = 'Y'; // Alter
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
            CheckListBoxPrivs.Checked[0] := ZQueryDBs.Fields[3].AsString = 'Y'; // Select
            CheckListBoxPrivs.Checked[1] := ZQueryDBs.Fields[4].AsString = 'Y'; // Insert
            CheckListBoxPrivs.Checked[2] := ZQueryDBs.Fields[5].AsString = 'Y'; // Update
            CheckListBoxPrivs.Checked[3] := ZQueryDBs.Fields[6].AsString = 'Y'; // Delete
            CheckListBoxPrivs.Checked[4] := ZQueryDBs.Fields[7].AsString = 'Y'; // Create
            CheckListBoxPrivs.Checked[5] := ZQueryDBs.Fields[8].AsString = 'Y'; // Drop
            CheckListBoxPrivs.Checked[6] := ZQueryDBs.Fields[9].AsString = 'Y'; // Grant
            CheckListBoxPrivs.Checked[7] := ZQueryDBs.Fields[10].AsString = 'Y'; // References
            CheckListBoxPrivs.Checked[8] := ZQueryDBs.Fields[11].AsString = 'Y'; // Index
            CheckListBoxPrivs.Checked[9] := ZQueryDBs.Fields[12].AsString = 'Y'; // Alter
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
            // find values in set-field:
            for j:=0 to CheckListBoxPrivs.Items.Count-1 do
              CheckListBoxPrivs.Checked[j] := pos(CheckListBoxPrivs.Items[j], ZQueryTables.Fields[6].AsString) > 0;
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
            // find values in set-field:
            for j:=0 to CheckListBoxPrivs.Items.Count-1 do
              CheckListBoxPrivs.Checked[j] := pos(CheckListBoxPrivs.Items[j], ZQueryColumns.Fields[6].AsString) > 0;
          end;
          ZQueryColumns.Next;
        end;
      end;
  end;

end;




procedure TUserManagerForm.ButtonSetClick(Sender: TObject);

function checked2yn(index: Byte): String;
begin
  if CheckListBoxPrivs.Checked[index] then
    result := 'Y'
  else
    result := 'N';
end;

var
  sql : String;
  i   : Byte;
begin
  // Grant specified Privileges
  Screen.Cursor := crHourglass;
  case TreeViewUsers.Selected.Level of
    0 : begin // general
        sql := 'UPDATE mysql.user SET ';
        for i:=0 to CheckListBoxPrivs.Items.Count-1 do begin
          if i > 0 then
            sql := sql + ', ';
          sql := sql + ' ' + CheckListBoxPrivs.Items[i] + '_priv = ''' + checked2yn(i) + '''';
        end;
        sql := sql + ' WHERE Host = ''' + Host + ''' AND User = ''' + User + '''';
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery(sql);
        GetResUsers;
      end;

    1 : begin // db
        if editcurrent then begin
          sql := 'UPDATE mysql.db SET ';
          for i:=0 to CheckListBoxPrivs.Items.Count-1 do begin
            if i > 0 then
              sql := sql + ', ';
            sql := sql + ' ' + CheckListBoxPrivs.Items[i] + '_priv = ''' + checked2yn(i) + '''';
          end;
          sql := sql + ' WHERE Host = ''' + Host + ''' AND Db = ''' + TreeViewUsers.Selected.Text + ''' AND User = ''' + User + '''';
        end
        else begin
          sql := 'INSERT INTO mysql.db (Host, Db, User';
          for i:=0 to CheckListBoxPrivs.Items.Count-1 do
            sql := sql + ', ' + CheckListBoxPrivs.Items[i] + '_priv';
          sql := sql + ') VALUES (''' + Host + ''', ''' + TreeViewUsers.Selected.Text + ''', ''' + User + '''';
          for i:=0 to CheckListBoxPrivs.Items.Count-1 do
            sql := sql + ', ''' + checked2yn(i) + '''';
          sql := sql + ')';
          editcurrent := true;
        end;
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery(sql);
        GetResDBs;
      end;

    2 : begin // table
        if editcurrent then begin
          sql := 'UPDATE mysql.tables_priv SET Table_priv = ''';
          for i:=0 to CheckListBoxPrivs.Items.Count-1 do begin
            if CheckListBoxPrivs.Checked[i] then
              sql := sql + CheckListBoxPrivs.Items[i] + ',';
          end;
          if sql[length(sql)] = ',' then
            delete(sql, length(sql), 1); // last comma
          sql := sql + ''' WHERE Host = ''' + Host + ''' AND Db = ''' + TreeViewUsers.Selected.Parent.Text + ''' AND User = ''' + User + ''' AND Table_name = ''' + TreeViewUsers.Selected.Text + '''';
        end
        else begin
          sql := 'INSERT INTO mysql.tables_priv (Host, Db, User, Table_name, Grantor, Table_priv) VALUES ('''+Host+''','''+TreeViewUsers.Selected.Parent.Text+''','''+User+''','''+TreeViewUsers.Selected.Text+''','''+TMDIChild(Mainform.ActiveMDIChild).ZConn.User+''',''';
          for i:=0 to CheckListBoxPrivs.Items.Count-1 do begin
            if CheckListBoxPrivs.Checked[i] then
              sql := sql + CheckListBoxPrivs.Items[i] + ',';
          end;
          if sql[length(sql)] = ',' then
            delete(sql, length(sql), 1); // last comma
          sql := sql + ''')';
          editcurrent := true;
        end;
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery(sql);
        GetResTables;
      end;

    3 : begin // column
        if editcurrent then begin
          sql := 'UPDATE mysql.columns_priv SET Column_priv = ''';
          for i:=0 to CheckListBoxPrivs.Items.Count-1 do begin
            if CheckListBoxPrivs.Checked[i] then
              sql := sql + CheckListBoxPrivs.Items[i] + ',';
          end;
          if sql[length(sql)] = ',' then
            delete(sql, length(sql), 1); // last comma
          sql := sql + ''' WHERE Host = ''' + Host + ''' AND Db = ''' + TreeViewUsers.Selected.Parent.Parent.Text + ''' AND User = ''' + User + ''' AND Table_name = ''' + TreeViewUsers.Selected.Parent.Text + ''' AND Column_name = ''' + TreeViewUsers.Selected.Text + '''';
        end
        else begin
          sql := 'INSERT INTO mysql.columns_priv (Host, Db, User, Table_name, Column_name, Column_priv) VALUES ('''+Host+''','''+TreeViewUsers.Selected.Parent.Parent.Text+''','''+User+''','''+TreeViewUsers.Selected.Parent.Text+''','''+TreeViewUsers.Selected.Text+''',''';
          for i:=0 to CheckListBoxPrivs.Items.Count-1 do begin
            if CheckListBoxPrivs.Checked[i] then
              sql := sql + CheckListBoxPrivs.Items[i] + ',';
          end;
          if sql[length(sql)] = ',' then
            delete(sql, length(sql), 1); // last comma
          sql := sql + ''')';
          editcurrent := true;
        end;
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery(sql);
        GetResColumns;
      end;
  end;
  TMDIChild(Mainform.ActiveMDIChild).ExecQuery('FLUSH PRIVILEGES');
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
  ZQueryF.Active := False;
end;


procedure TUserManagerForm.CheckListBoxPrivsClickCheck(Sender: TObject);
begin
  ButtonSet.Enabled := true;
end;


procedure TUserManagerForm.ButtonRevokeClick(Sender: TObject);
var sql : String;
begin
  // Delete some Privs
  Screen.Cursor := crHourglass;
  case TreeViewUsers.Selected.Level of
    0 : // delete user
      if MessageDLG('Delete User '''+User+''' and all its privileges?', mtConfirmation, [mbNo, mbYes], 0) = mrYes then begin
        sql := 'DELETE FROM mysql.user WHERE Host='''+Host+''' AND User='''+User+'''';
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery(sql);
        sql := 'DELETE FROM mysql.db WHERE Host='''+Host+''' AND User='''+User+'''';
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery(sql);
        sql := 'DELETE FROM mysql.tables_priv WHERE Host='''+Host+''' AND User='''+User+'''';
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery(sql);
        sql := 'DELETE FROM mysql.columns_priv WHERE Host='''+Host+''' AND User='''+User+'''';
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery(sql);
        TreeViewUsers.Selected.Delete;
        ZQueryDBs.Active := False;
        ZQueryTables.Active := False;
        ZQueryColumns.Active := False;
        GetResUsers;
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery('FLUSH PRIVILEGES');
      end;
    1 : // delete db-privs
      begin
        sql := 'DELETE FROM mysql.db WHERE Host='''+Host+''' AND User='''+User+''' AND Db='''+TreeViewUsers.Selected.Text+'''';
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery(sql);
        ShowPrivilegesControls(false, false, true);
        GetResDBs;
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery('FLUSH PRIVILEGES');
      end;
    2 : // delete table-privs
      begin
        sql := 'DELETE FROM mysql.tables_priv WHERE Host='''+Host+''' AND User='''+User+''' AND Db='''+TreeViewUsers.Selected.Parent.Text+''' AND Table_name='''+TreeViewUsers.Selected.Text+'''';
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery(sql);
        ShowPrivilegesControls(false, false, true);
        GetResTables;
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery('FLUSH PRIVILEGES');
      end;
    3 : // delete column-privs
      begin
        sql := 'DELETE FROM mysql.columns_priv WHERE Host='''+Host+''' AND User='''+User+''' AND Db='''+TreeViewUsers.Selected.Parent.Parent.Text+''' AND Table_name='''+TreeViewUsers.Selected.Parent.Text+''' AND Column_name='''+TreeViewUsers.Selected.Text+'''';
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery(sql);
        ShowPrivilegesControls(false, false, true);
        GetResColumns;
        TMDIChild(Mainform.ActiveMDIChild).ExecQuery('FLUSH PRIVILEGES');
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
  ZQueryF.Active := False;
  ShowPrivilegesControls(false, true, false);
  TreeViewUsers.Items.Clear;
  PageControl1.OnChange(self);
end;


procedure TUserManagerForm.GetResUsers;
begin
  TMDIChild(Mainform.ActiveMDIChild).GetResults( 'SELECT * FROM mysql.user', ZQueryUsers );
end;

procedure TUserManagerForm.GetResDBs;
begin
  TMDIChild(Mainform.ActiveMDIChild).GetResults( 'SELECT * FROM mysql.db', ZQueryDBs );
end;

procedure TUserManagerForm.GetResTables;
begin
  TMDIChild(Mainform.ActiveMDIChild).GetResults( 'SELECT * FROM mysql.tables_priv', ZQueryTables );
end;

procedure TUserManagerForm.GetResColumns;
begin
  TMDIChild(Mainform.ActiveMDIChild).GetResults( 'SELECT * FROM mysql.columns_priv', ZQueryColumns );
end;

end.
