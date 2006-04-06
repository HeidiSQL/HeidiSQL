unit usermanager;


// -------------------------------------
// HeidiSQL
// Usermanager
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, CheckLst, ImgList, ExtCtrls, Registry, mysql;
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
  private
    { Private declarations }
    mresult               : PMYSQL_RES;
    mrow                  : PMYSQL_ROW;
    editcurrent           : Boolean;
  public
    { Public declarations }
    User, Host            : String; // Remember for setting privileges
    usersresult, dbsresult, tablesresult, columnsresult     : PMYSQL_RES;
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
    EditUser.Text := MyUser;

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
      WriteString('Host', TMDIChild(Application.Mainform.ActiveMDIChild).MyHost);
      WriteString('User', EditUser.Text);
      WriteString('Password', encrypt(EditPassword.Text));
      WriteString('Port', IntToStr(TMDIChild(Application.Mainform.ActiveMDIChild).MyPort));
      WriteString('Timeout', IntToStr(TMDIChild(Application.Mainform.ActiveMDIChild).MyTime));
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
  TMDIChild(Application.Mainform.ActiveMDIChild).q(query);
  TMDIChild(Application.Mainform.ActiveMDIChild).q('FLUSH PRIVILEGES');
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
        mresult := TMDIChild(Mainform.ActiveMDIChild).q('SHOW FIELDS FROM ' + TreeViewUsers.Selected.Parent.Text + '.' + TreeViewUsers.Selected.Text);
        for i:=0 to mysql_num_rows(mresult)-1 do begin
          mrow := mysql_fetch_row(mresult);
          tnu := TreeViewUsers.Items.AddChild(TreeViewUsers.Selected, mrow[0]);
          tnu.ImageIndex := 17;
          tnu.SelectedIndex := 18;
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
        if dbsresult = nil then
          dbsresult := TMDIChild(Mainform.ActiveMDIChild).q('SELECT * FROM mysql.db');
        ButtonEditUser.Enabled := false;
      end;
    2 : begin
        LabelDB.Caption := Node.Parent.Text; LabelDB.Font.Color := clWindowText;
        LabelTable.Caption := Node.Text; LabelTable.Font.Color := clWindowText;
        LabelColumn.Caption := '<All Columns>'; LabelColumn.Font.Color := highlight;
        if tablesresult = nil then
          tablesresult := TMDIChild(Mainform.ActiveMDIChild).q('SELECT * FROM mysql.tables_priv');
        ButtonEditUser.Enabled := false;
      end;
    3 : begin
        LabelDB.Caption := Node.Parent.Parent.Text; LabelDB.Font.Color := clWindowText;
        LabelTable.Caption := Node.Parent.Text; LabelTable.Font.Color := clWindowText;
        LabelColumn.Caption := Node.Text; LabelColumn.Font.Color := clWindowText;
        if columnsresult = nil then
          columnsresult := TMDIChild(Mainform.ActiveMDIChild).q('SELECT * FROM mysql.columns_priv');
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
    (TreeViewUsers.Items.Count = 0) then begin
    usersresult := TMDIChild(Mainform.ActiveMDIChild).q('SELECT * FROM mysql.user');
    for i:=0 to mysql_num_rows(usersresult)-1 do begin
      mrow := mysql_fetch_row(usersresult);
      tn := TreeViewUsers.Items.AddChild(nil, mrow[1] + '@' + mrow[0]);
      tn.ImageIndex := 16;
      tn.SelectedIndex := 15;
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
        mysql_data_seek(usersresult, 0);
        for i:=1 to mysql_num_rows(usersresult) do begin
          mrow := mysql_fetch_row(usersresult);
          if (mrow[0]+'' = Host) and (mrow[1]+'' = User) then begin // found the according user!
            ShowPrivilegesControls(true, false, false);
            CheckListBoxPrivs.Checked[0] := mrow[3] = 'Y'; // Select
            CheckListBoxPrivs.Checked[1] := mrow[4] = 'Y'; // Insert
            CheckListBoxPrivs.Checked[2] := mrow[5] = 'Y'; // Update
            CheckListBoxPrivs.Checked[3] := mrow[6] = 'Y'; // Delete
            CheckListBoxPrivs.Checked[4] := mrow[7] = 'Y'; // Create
            CheckListBoxPrivs.Checked[5] := mrow[8] = 'Y'; // Drop
            CheckListBoxPrivs.Checked[6] := mrow[9] = 'Y'; // Reload
            CheckListBoxPrivs.Checked[7] := mrow[10] = 'Y'; // Shutdown
            CheckListBoxPrivs.Checked[8] := mrow[11] = 'Y'; // Process
            CheckListBoxPrivs.Checked[9] := mrow[12] = 'Y'; // File
            CheckListBoxPrivs.Checked[10] := mrow[13] = 'Y'; // Grant
            CheckListBoxPrivs.Checked[11] := mrow[14] = 'Y'; // References
            CheckListBoxPrivs.Checked[12] := mrow[15] = 'Y'; // Index
            CheckListBoxPrivs.Checked[13] := mrow[16] = 'Y'; // Alter
            break;
          end;
        end;
      end;

    1 : begin // db-privileges
        mysql_data_seek(dbsresult, 0);
        for i:=1 to mysql_num_rows(dbsresult) do begin
          mrow := mysql_fetch_row(dbsresult);
          if (mrow[0]+'' = Host) and (mrow[1]+'' = Node.Text) and (mrow[2]+'' = User) then begin
            // some privs are set:
            ShowPrivilegesControls(true, false, false);
            CheckListBoxPrivs.Checked[0] := mrow[3] = 'Y'; // Select
            CheckListBoxPrivs.Checked[1] := mrow[4] = 'Y'; // Insert
            CheckListBoxPrivs.Checked[2] := mrow[5] = 'Y'; // Update
            CheckListBoxPrivs.Checked[3] := mrow[6] = 'Y'; // Delete
            CheckListBoxPrivs.Checked[4] := mrow[7] = 'Y'; // Create
            CheckListBoxPrivs.Checked[5] := mrow[8] = 'Y'; // Drop
            CheckListBoxPrivs.Checked[6] := mrow[9] = 'Y'; // Grant
            CheckListBoxPrivs.Checked[7] := mrow[10] = 'Y'; // References
            CheckListBoxPrivs.Checked[8] := mrow[11] = 'Y'; // Index
            CheckListBoxPrivs.Checked[9] := mrow[12] = 'Y'; // Alter
            break;
          end;
        end;
      end;

    2 : begin // table-privileges
        mysql_data_seek(tablesresult, 0);
        for i:=1 to mysql_num_rows(tablesresult) do begin
          mrow := mysql_fetch_row(tablesresult);
          if (mrow[0]+'' = Host) and (mrow[1]+'' = Node.Parent.Text) and (mrow[2]+'' = User) and (mrow[3]+'' = Node.Text) then begin // found the according record!
            // some privs are set:
            ShowPrivilegesControls(true, false, false);
            // find values in set-field:
            for j:=0 to CheckListBoxPrivs.Items.Count-1 do
              CheckListBoxPrivs.Checked[j] := pos(CheckListBoxPrivs.Items[j], mrow[6]) > 0;
            break;
          end;
        end;
      end;

    3 : begin // column-privileges
        mysql_data_seek(columnsresult, 0);
        for i:=1 to mysql_num_rows(columnsresult) do begin
          mrow := mysql_fetch_row(columnsresult);
          if (mrow[0]+'' = Host) and (mrow[1]+'' = Node.Parent.Parent.Text) and (mrow[2]+'' = User)
            and (mrow[3]+'' = Node.Parent.Text) and (mrow[4]+'' = Node.Text) then begin
            // some privs are set:
            ShowPrivilegesControls(true, false, false);
            // find values in set-field:
            for j:=0 to CheckListBoxPrivs.Items.Count-1 do
              CheckListBoxPrivs.Checked[j] := pos(CheckListBoxPrivs.Items[j], mrow[6]) > 0;
            break;
          end;
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
        TMDIChild(Mainform.ActiveMDIChild).q(sql);
        mysql_free_result(usersresult);
        usersresult := TMDIChild(Mainform.ActiveMDIChild).q('SELECT * FROM mysql.user');
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
        TMDIChild(Mainform.ActiveMDIChild).q(sql);
        mysql_free_result(dbsresult);
        dbsresult := TMDIChild(Mainform.ActiveMDIChild).q('SELECT * FROM mysql.db');
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
          sql := 'INSERT INTO mysql.tables_priv (Host, Db, User, Table_name, Grantor, Table_priv) VALUES ('''+Host+''','''+TreeViewUsers.Selected.Parent.Text+''','''+User+''','''+TreeViewUsers.Selected.Text+''','''+TMDIChild(Mainform.ActiveMDIChild).MyUser+''',''';
          for i:=0 to CheckListBoxPrivs.Items.Count-1 do begin
            if CheckListBoxPrivs.Checked[i] then
              sql := sql + CheckListBoxPrivs.Items[i] + ',';
          end;
          if sql[length(sql)] = ',' then
            delete(sql, length(sql), 1); // last comma
          sql := sql + ''')';
          editcurrent := true;
        end;
        TMDIChild(Mainform.ActiveMDIChild).q(sql);
        mysql_free_result(tablesresult);
        tablesresult := TMDIChild(Mainform.ActiveMDIChild).q('SELECT * FROM mysql.tables_priv');
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
        TMDIChild(Mainform.ActiveMDIChild).q(sql);
        mysql_free_result(columnsresult);
        columnsresult := TMDIChild(Mainform.ActiveMDIChild).q('SELECT * FROM mysql.columns_priv');
      end;
  end;
  TMDIChild(Mainform.ActiveMDIChild).q('FLUSH PRIVILEGES');
  ButtonRevoke.Enabled := editcurrent;
  Screen.Cursor := crDefault;
end;




procedure TUserManagerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // free memory
  if UsersResult <> nil then
    mysql_free_result(UsersResult);
    UsersResult := nil;
  if DBsResult <> nil then begin
    mysql_free_result(DBsResult);
    dbsresult := nil;
  end;
  if TablesResult <> nil then begin
    mysql_free_result(TablesResult);
    TablesResult := nil;
  end;
  if ColumnsResult <> nil then begin
    mysql_free_result(ColumnsResult);
    ColumnsResult := nil;
  end;
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
        TMDIChild(Mainform.ActiveMDIChild).q(sql);
        sql := 'DELETE FROM mysql.db WHERE Host='''+Host+''' AND User='''+User+'''';
        TMDIChild(Mainform.ActiveMDIChild).q(sql);
        sql := 'DELETE FROM mysql.tables_priv WHERE Host='''+Host+''' AND User='''+User+'''';
        TMDIChild(Mainform.ActiveMDIChild).q(sql);
        sql := 'DELETE FROM mysql.columns_priv WHERE Host='''+Host+''' AND User='''+User+'''';
        TMDIChild(Mainform.ActiveMDIChild).q(sql);
        TreeViewUsers.Selected.Delete;
        mysql_free_result(UsersResult);
        mysql_free_result(DBsResult);
        DBsResult := nil;
        mysql_free_result(TablesResult);
        TablesResult := nil;
        mysql_free_result(ColumnsResult);
        ColumnsResult := nil;
        usersresult := TMDIChild(Mainform.ActiveMDIChild).q('SELECT * FROM mysql.user');
        TMDIChild(Mainform.ActiveMDIChild).q('FLUSH PRIVILEGES');
      end;
    1 : // delete db-privs
      begin
        sql := 'DELETE FROM mysql.db WHERE Host='''+Host+''' AND User='''+User+''' AND Db='''+TreeViewUsers.Selected.Text+'''';
        TMDIChild(Mainform.ActiveMDIChild).q(sql);
        ShowPrivilegesControls(false, false, true);
        mysql_free_result(DBsResult);
        dbsresult := TMDIChild(Mainform.ActiveMDIChild).q('SELECT * FROM mysql.db');
        TMDIChild(Mainform.ActiveMDIChild).q('FLUSH PRIVILEGES');
      end;
    2 : // delete table-privs
      begin
        sql := 'DELETE FROM mysql.tables_priv WHERE Host='''+Host+''' AND User='''+User+''' AND Db='''+TreeViewUsers.Selected.Parent.Text+''' AND Table_name='''+TreeViewUsers.Selected.Text+'''';
        TMDIChild(Mainform.ActiveMDIChild).q(sql);
        ShowPrivilegesControls(false, false, true);
        mysql_free_result(TablesResult);
        TablesResult := TMDIChild(Mainform.ActiveMDIChild).q('SELECT * FROM mysql.tables_priv');
        TMDIChild(Mainform.ActiveMDIChild).q('FLUSH PRIVILEGES');
      end;
    3 : // delete column-privs
      begin
        sql := 'DELETE FROM mysql.columns_priv WHERE Host='''+Host+''' AND User='''+User+''' AND Db='''+TreeViewUsers.Selected.Parent.Parent.Text+''' AND Table_name='''+TreeViewUsers.Selected.Parent.Text+''' AND Column_name='''+TreeViewUsers.Selected.Text+'''';
        TMDIChild(Mainform.ActiveMDIChild).q(sql);
        ShowPrivilegesControls(false, false, true);
        mysql_free_result(ColumnsResult);
        ColumnsResult := TMDIChild(Mainform.ActiveMDIChild).q('SELECT * FROM mysql.columns_priv');
        TMDIChild(Mainform.ActiveMDIChild).q('FLUSH PRIVILEGES');
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
  if UsersResult <> nil then
    mysql_free_result(UsersResult);
    UsersResult := nil;
  if DBsResult <> nil then begin
    mysql_free_result(DBsResult);
    dbsresult := nil;
  end;
  if TablesResult <> nil then begin
    mysql_free_result(TablesResult);
    TablesResult := nil;
  end;
  if ColumnsResult <> nil then begin
    mysql_free_result(ColumnsResult);
    ColumnsResult := nil;
  end;
  ShowPrivilegesControls(false, true, false);
  TreeViewUsers.Items.Clear;
  PageControl1.OnChange(self);
end;

end.
