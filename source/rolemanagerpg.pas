unit rolemanagerpg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Spin, Grids, Buttons, Contnrs,
  main, dbconnection, dbstructures;

type
  TPasswordMode = (pmUnchanged, pmClear, pmSet);
  TDBObjectKind = (okDatabase, okSchema, okTableView, okSequence, okRoutine);
  TDefaultPrivKind = (dpTables, dpSequences, dpRoutines);

  TMembershipItem = class
  public
    RoleName: String;
    IsMember: Boolean;
    AdminOption: Boolean;
    InheritOption: Boolean;
    SetOption: Boolean;
    OriginalIsMember: Boolean;
    OriginalAdminOption: Boolean;
    OriginalInheritOption: Boolean;
    OriginalSetOption: Boolean;
  end;

  TObjectPrivItem = class
  public
    ObjectKind: TDBObjectKind;
    SchemaName: String;
    ObjectName: String;
    ObjectIdentity: String;
    Privileges: TStringList;
    OriginalPrivileges: TStringList;
    Grantable: TStringList;
    OriginalGrantable: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  TDefaultPrivItem = class
  public
    OwnerName: String;
    SchemaName: String;
    Kind: TDefaultPrivKind;
    Grantee: String;
    Privileges: TStringList;
    OriginalPrivileges: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  TRoleState = class
  public
    RoleName: String;
    OriginalRoleName: String;
    CanLogin: Boolean;
    SuperUser: Boolean;
    CreateDB: Boolean;
    CreateRole: Boolean;
    InheritRights: Boolean;
    Replication: Boolean;
    BypassRLS: Boolean;
    ConnectionLimit: Integer;
    PasswordMode: TPasswordMode;
    PasswordValue: String;
    IsNewRole: Boolean;
    IsCloneRole: Boolean;
    Memberships: TObjectList;
    DatabasePrivs: TObjectList;
    SchemaPrivs: TObjectList;
    TableViewPrivs: TObjectList;
    SequencePrivs: TObjectList;
    RoutinePrivs: TObjectList;
    DefaultPrivs: TObjectList;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  end;

  TRoleManagerPgForm = class(TForm)
    btnCancel: TBitBtn;
    btnClone: TBitBtn;
    btnDrop: TBitBtn;
    btnNew: TBitBtn;
    btnSave: TBitBtn;
    cbBypassRLS: TCheckBox;
    cbCanLogin: TCheckBox;
    cbCreateDB: TCheckBox;
    cbCreateRole: TCheckBox;
    cbInheritRights: TCheckBox;
    cbPasswordMode: TComboBox;
    cbReplication: TCheckBox;
    cbSuperUser: TCheckBox;
    cmbRole: TComboBox;
    edPassword: TEdit;
    edRoleName: TEdit;
    gbPassword: TGroupBox;
    gbRoleAttrs: TGroupBox;
    lblConnLimit: TLabel;
    lblPasswordMode: TLabel;
    lblRole: TLabel;
    lblRoleName: TLabel;
    memoLog: TMemo;
    PageControl1: TPageControl;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    seConnLimit: TSpinEdit;
    sgDatabasePrivs: TStringGrid;
    sgDefaultPrivs: TStringGrid;
    sgRoutinePrivs: TStringGrid;
    sgSchemaPrivs: TStringGrid;
    sgSequencesPrivs: TStringGrid;
    sgTableViewPrivs: TStringGrid;
    tabDefaultPrivs: TTabSheet;
    tabMemberships: TTabSheet;
    tabObjectPrivs: TTabSheet;
    tabRoles: TTabSheet;
    tabObjDatabases: TTabSheet;
    tabObjRoutines: TTabSheet;
    tabObjSchemas: TTabSheet;
    tabObjSequences: TTabSheet;
    tabObjTablesViews: TTabSheet;
    tabsObjectKinds: TPageControl;
    vlMemberships: TStringGrid;
    procedure btnCancelClick(Sender: TObject);
    procedure btnCloneClick(Sender: TObject);
    procedure btnDropClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbPasswordModeChange(Sender: TObject);
    procedure cmbRoleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure vlMembershipsCheckboxToggled(Sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
  private
    FConn: TDBConnection;
    FState: TRoleState;
    FLoading: Boolean;
    FModified: Boolean;
    procedure InitGrids;
    procedure ConfigureMembershipColumns;
    procedure EnsureConnected;
    procedure LoadRolesList;
    procedure LoadRole(const ARoleName: String);
    procedure LoadRoleAttributes(const ARoleName: String);
    procedure LoadMemberships(const ARoleName: String);
    procedure LoadObjectPrivileges(const ARoleName: String);
    procedure LoadDefaultPrivileges(const ARoleName: String);
    procedure LoadPrivilegeList(AId: TQueryId; const RoleName: String; AKind: TDBObjectKind; AList: TObjectList);
    procedure PopulateFormFromState;
    procedure ReadFormToState;
    procedure SetPasswordModeUI;
    procedure FillMembershipGrid;
    procedure FillPrivilegeGrid(AGrid: TStringGrid; AList: TObjectList);
    procedure FillObjectPrivilegeGrids;
    procedure FillDefaultPrivilegesGrid;
    procedure ReadPrivilegeGrid(AGrid: TStringGrid; AList: TObjectList);
    procedure ReadDefaultPrivilegeGrid;
    procedure SaveRole;
    procedure SaveRoleCore;
    procedure SaveMemberships;
    procedure SaveObjectPrivileges;
    procedure SaveDefaultPrivileges;
    procedure SavePrivilegeList(AList: TObjectList; const ATypeToken: String; const AAllPrivs: array of String);
    procedure DropRole;
    procedure CloneRole;
    procedure MarkModified;
    function ConfirmDrop: Boolean;
    function ConfirmRoleSwitch: Boolean;
    function SelectedRole: String;
    function Sql(AId: TQueryId): String; overload;
    function Sql(AId: TQueryId; const Args: array of const): String; overload;
    function QuoteIdent(const S: String): String;
    function QuoteLiteral(const S: String): String;
    function BoolToken(const TrueToken, FalseToken: String; const Value: Boolean): String;
    function PasswordModeFromIndex(AIndex: Integer): TPasswordMode;
    function CsvToList(const S: String): TStringList;
    function NormalizePrivilegeCsv(const S: String): String;
    function DefaultPrivKindToToken(AKind: TDefaultPrivKind): String;
    function DefaultPrivKindFromString(const S: String): TDefaultPrivKind;
    function GridCellChecked(AGrid: TStringGrid; ACol, ARow: Integer): Boolean;
    procedure SetGridCheckbox(AGrid: TStringGrid; ACol, ARow: Integer; AValue: Boolean);
    procedure Log(const S: String);
  public
  end;

implementation

{$R *.lfm}

const
  CDbPrivs: array[0..2] of String = ('CREATE', 'CONNECT', 'TEMPORARY');
  CSchemaPrivs: array[0..1] of String = ('CREATE', 'USAGE');
  CTablePrivs: array[0..6] of String = ('SELECT', 'INSERT', 'UPDATE', 'DELETE', 'TRUNCATE', 'REFERENCES', 'TRIGGER');
  CSequencePrivs: array[0..2] of String = ('USAGE', 'SELECT', 'UPDATE');
  CRoutinePrivs: array[0..0] of String = ('EXECUTE');

{ TObjectPrivItem }

constructor TObjectPrivItem.Create;
begin
  inherited Create;
  Privileges := TStringList.Create;
  OriginalPrivileges := TStringList.Create;
  Grantable := TStringList.Create;
  OriginalGrantable := TStringList.Create;
  Privileges.StrictDelimiter := True;
  OriginalPrivileges.StrictDelimiter := True;
  Grantable.StrictDelimiter := True;
  OriginalGrantable.StrictDelimiter := True;
end;

destructor TObjectPrivItem.Destroy;
begin
  Grantable.Free;
  OriginalGrantable.Free;
  Privileges.Free;
  OriginalPrivileges.Free;
  inherited Destroy;
end;

{ TDefaultPrivItem }

constructor TDefaultPrivItem.Create;
begin
  inherited Create;
  Privileges := TStringList.Create;
  OriginalPrivileges := TStringList.Create;
  Privileges.StrictDelimiter := True;
  OriginalPrivileges.StrictDelimiter := True;
end;

destructor TDefaultPrivItem.Destroy;
begin
  Privileges.Free;
  OriginalPrivileges.Free;
  inherited Destroy;
end;

{ TRoleState }

constructor TRoleState.Create;
begin
  inherited Create;
  Memberships := TObjectList.Create(True);
  DatabasePrivs := TObjectList.Create(True);
  SchemaPrivs := TObjectList.Create(True);
  TableViewPrivs := TObjectList.Create(True);
  SequencePrivs := TObjectList.Create(True);
  RoutinePrivs := TObjectList.Create(True);
  DefaultPrivs := TObjectList.Create(True);
  Clear;
end;

destructor TRoleState.Destroy;
begin
  DefaultPrivs.Free;
  RoutinePrivs.Free;
  SequencePrivs.Free;
  TableViewPrivs.Free;
  SchemaPrivs.Free;
  DatabasePrivs.Free;
  Memberships.Free;
  inherited Destroy;
end;

procedure TRoleState.Clear;
begin
  RoleName := '';
  OriginalRoleName := '';
  CanLogin := True;
  SuperUser := False;
  CreateDB := False;
  CreateRole := False;
  InheritRights := True;
  Replication := False;
  BypassRLS := False;
  ConnectionLimit := -1;
  PasswordMode := pmUnchanged;
  PasswordValue := '';
  IsNewRole := False;
  IsCloneRole := False;
  Memberships.Clear;
  DatabasePrivs.Clear;
  SchemaPrivs.Clear;
  TableViewPrivs.Clear;
  SequencePrivs.Clear;
  RoutinePrivs.Clear;
  DefaultPrivs.Clear;
end;

{ TRoleManagerPgForm }


procedure TRoleManagerPgForm.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  memoLog.Visible := True;
  {$ENDIF}
  FConn := nil;
  FState := TRoleState.Create;
  cbPasswordMode.Items.Text := 'Unchanged' + LineEnding + 'Clear password' + LineEnding + 'Set password';
  cbPasswordMode.ItemIndex := 0;
  InitGrids;
  FModified := False;
end;

procedure TRoleManagerPgForm.FormShow(Sender: TObject);
begin
  FConn := MainForm.ActiveConnection;
  EnsureConnected;
  ConfigureMembershipColumns;
  LoadRolesList;
  if cmbRole.Items.Count > 0 then
  begin
    cmbRole.ItemIndex := 0;
    LoadRole(cmbRole.Text);
  end else
    btnNewClick(nil);
end;

procedure TRoleManagerPgForm.EnsureConnected;
begin
  if FConn = nil then
    raise Exception.Create('No active connection.');
  if FConn.ServerVersionInt < 120000 then
    raise Exception.Create('PostgreSQL 12.0 or newer required.');
end;

procedure TRoleManagerPgForm.InitGrids;
  procedure InitPrivGrid(AGrid: TStringGrid; const FirstColCaption: String);
  begin
    AGrid.ColCount := 4;
    AGrid.FixedCols := 0;
    AGrid.RowCount := 1;
    AGrid.Cells[0,0] := FirstColCaption;
    AGrid.Cells[1,0] := 'Object';
    AGrid.Cells[2,0] := 'Privileges';
    AGrid.Cells[3,0] := 'Grantable';
    AGrid.Options := AGrid.Options + [goEditing];
  end;
begin
  vlMemberships.ColCount := 5;
  vlMemberships.FixedCols := 0;
  vlMemberships.RowCount := 1;
  vlMemberships.Cells[0,0] := 'Role';
  vlMemberships.Cells[1,0] := 'Member';
  vlMemberships.Cells[2,0] := 'Admin';
  vlMemberships.Cells[3,0] := 'Inherit';
  vlMemberships.Cells[4,0] := 'Set';
  vlMemberships.Options := vlMemberships.Options + [goEditing];

  InitPrivGrid(sgDatabasePrivs, 'Database');
  InitPrivGrid(sgSchemaPrivs, 'Schema');
  InitPrivGrid(sgTableViewPrivs, 'Schema');
  InitPrivGrid(sgSequencesPrivs, 'Schema');
  InitPrivGrid(sgRoutinePrivs, 'Schema');

  sgDefaultPrivs.ColCount := 5;
  sgDefaultPrivs.FixedCols := 0;
  sgDefaultPrivs.RowCount := 1;
  sgDefaultPrivs.Cells[0,0] := 'Owner';
  sgDefaultPrivs.Cells[1,0] := 'Schema';
  sgDefaultPrivs.Cells[2,0] := 'Kind';
  sgDefaultPrivs.Cells[3,0] := 'Grantee';
  sgDefaultPrivs.Cells[4,0] := 'Privileges';
  sgDefaultPrivs.Options := sgDefaultPrivs.Options + [goEditing];
end;

procedure TRoleManagerPgForm.ConfigureMembershipColumns;
var
  i: Integer;
begin
  while vlMemberships.Columns.Count < 5 do
    vlMemberships.Columns.Add;

  vlMemberships.Columns[0].Title.Caption := 'Role';
  vlMemberships.Columns[1].Title.Caption := 'Member';
  vlMemberships.Columns[2].Title.Caption := 'Admin';
  vlMemberships.Columns[3].Title.Caption := 'Inherit';
  vlMemberships.Columns[4].Title.Caption := 'Set';

  for i := 1 to 4 do
  begin
    vlMemberships.Columns[i].ButtonStyle := cbsCheckboxColumn;
    vlMemberships.Columns[i].ValueChecked := '1';
    vlMemberships.Columns[i].ValueUnchecked := '0';
  end;

  vlMemberships.Columns[2].Visible := True;
  vlMemberships.Columns[3].Visible := FConn.ServerVersionInt >= 160000;
  vlMemberships.Columns[4].Visible := FConn.ServerVersionInt >= 160000;
end;

procedure TRoleManagerPgForm.LoadRolesList;
var
  Res: TDBQuery;
  i: Integer;
begin
  cmbRole.Items.BeginUpdate;
  try
    cmbRole.Items.Clear;
    Res := FConn.GetResults(Sql(qPgRolemanRolesList));
    try
      for i := 0 to Res.RecordCount - 1 do
      begin
        Res.RecNo := i + 1;
        cmbRole.Items.Add(Res.Col('rolname'));
      end;
    finally
      Res.Free;
    end;
  finally
    cmbRole.Items.EndUpdate;
  end;
end;

procedure TRoleManagerPgForm.LoadRole(const ARoleName: String);
begin
  FLoading := True;
  try
    FState.Clear;
    FState.RoleName := ARoleName;
    FState.OriginalRoleName := ARoleName;
    LoadRoleAttributes(ARoleName);
    LoadMemberships(ARoleName);
    LoadObjectPrivileges(ARoleName);
    LoadDefaultPrivileges(ARoleName);
    PopulateFormFromState;
    FModified := False;
  finally
    FLoading := False;
  end;
end;

procedure TRoleManagerPgForm.LoadRoleAttributes(const ARoleName: String);
var
  Res: TDBQuery;
begin
  Res := FConn.GetResults(Sql(qPgRolemanRoleLoad, [QuoteLiteral(ARoleName)]));
  try
    if Res.RecordCount = 0 then Exit;
    Res.RecNo := 1;
    FState.RoleName := Res.Col('rolname');
    FState.OriginalRoleName := Res.Col('rolname');
    FState.CanLogin := Res.Col('rolcanlogin') = 't';
    FState.SuperUser := Res.Col('rolsuper') = 't';
    FState.CreateDB := Res.Col('rolcreatedb') = 't';
    FState.CreateRole := Res.Col('rolcreaterole') = 't';
    FState.InheritRights := Res.Col('rolinherit') = 't';
    FState.Replication := Res.Col('rolreplication') = 't';
    FState.BypassRLS := Res.Col('rolbypassrls') = 't';
    FState.ConnectionLimit := StrToIntDef(Res.Col('rolconnlimit'), -1);
    FState.PasswordMode := pmUnchanged;
    FState.PasswordValue := '';
  finally
    Res.Free;
  end;
end;

procedure TRoleManagerPgForm.LoadMemberships(const ARoleName: String);
var
  Res: TDBQuery;
  Item: TMembershipItem;
  i: Integer;
begin
  Res := FConn.GetResults(Sql(qPgRolemanMembershipsLoad, [QuoteLiteral(ARoleName)]));
  try
    for i := 0 to Res.RecordCount - 1 do
    begin
      Res.RecNo := i + 1;
      Item := TMembershipItem.Create;
      Item.RoleName := Res.Col('member_of');
      Item.IsMember := Res.Col('is_member') = 't';
      Item.AdminOption := Res.Col('admin_option') = 't';
      Item.InheritOption := not SameText(Res.Col('inherit_option'), 'f');
      Item.SetOption := not SameText(Res.Col('set_option'), 'f');
      Item.OriginalIsMember := Item.IsMember;
      Item.OriginalAdminOption := Item.AdminOption;
      Item.OriginalInheritOption := Item.InheritOption;
      Item.OriginalSetOption := Item.SetOption;
      FState.Memberships.Add(Item);
    end;
  finally
    Res.Free;
  end;
end;

procedure TRoleManagerPgForm.LoadPrivilegeList(AId: TQueryId; const RoleName: String;
  AKind: TDBObjectKind; AList: TObjectList);
var
  Res: TDBQuery;
  Item: TObjectPrivItem;
  i: Integer;
begin
  AList.Clear;
  Res := FConn.GetResults(Sql(AId, [QuoteLiteral(RoleName)]));
  try
    for i := 0 to Res.RecordCount - 1 do
    begin
      Res.RecNo := i + 1;
      Item := TObjectPrivItem.Create;
      Item.ObjectKind := AKind;
      Item.SchemaName := Res.Col('schema_name');
      Item.ObjectName := Res.Col('object_name');
      Item.ObjectIdentity := Res.Col('object_identity');
      Item.Privileges.Assign(CsvToList(Res.Col('privileges')));
      Item.OriginalPrivileges.Assign(Item.Privileges);
      Item.Grantable.Assign(CsvToList(Res.Col('grantable')));
      Item.OriginalGrantable.Assign(Item.Grantable);
      AList.Add(Item);
    end;
  finally
    Res.Free;
  end;
end;

procedure TRoleManagerPgForm.LoadObjectPrivileges(const ARoleName: String);
begin
  LoadPrivilegeList(qPgRolemanObjPrivsDatabasesLoad, ARoleName, okDatabase, FState.DatabasePrivs);
  LoadPrivilegeList(qPgRolemanObjPrivsSchemasLoad, ARoleName, okSchema, FState.SchemaPrivs);
  LoadPrivilegeList(qPgRolemanObjPrivsTablesViewsLoad, ARoleName, okTableView, FState.TableViewPrivs);
  LoadPrivilegeList(qPgRolemanObjPrivsSequencesLoad, ARoleName, okSequence, FState.SequencePrivs);
  LoadPrivilegeList(qPgRolemanObjPrivsRoutinesLoad, ARoleName, okRoutine, FState.RoutinePrivs);
end;

procedure TRoleManagerPgForm.LoadDefaultPrivileges(const ARoleName: String);
var
  Res: TDBQuery;
  Item: TDefaultPrivItem;
  i: Integer;
begin
  FState.DefaultPrivs.Clear;
  Res := FConn.GetResults(Sql(qPgRolemanDefaultPrivsLoad, [QuoteLiteral(ARoleName)]));
  try
    for i := 0 to Res.RecordCount - 1 do
    begin
      Res.RecNo := i + 1;
      Item := TDefaultPrivItem.Create;
      Item.OwnerName := Res.Col('owner_name');
      Item.SchemaName := Res.Col('schema_name');
      Item.Kind := DefaultPrivKindFromString(Res.Col('kind'));
      Item.Grantee := Res.Col('grantee');
      Item.Privileges.Assign(CsvToList(Res.Col('privileges')));
      Item.OriginalPrivileges.Assign(Item.Privileges);
      FState.DefaultPrivs.Add(Item);
    end;
  finally
    Res.Free;
  end;
end;

procedure TRoleManagerPgForm.PopulateFormFromState;
begin
  edRoleName.Text := FState.RoleName;
  cbCanLogin.Checked := FState.CanLogin;
  cbSuperUser.Checked := FState.SuperUser;
  cbCreateDB.Checked := FState.CreateDB;
  cbCreateRole.Checked := FState.CreateRole;
  cbInheritRights.Checked := FState.InheritRights;
  cbReplication.Checked := FState.Replication;
  cbBypassRLS.Checked := FState.BypassRLS;
  seConnLimit.Value := FState.ConnectionLimit;
  cbPasswordMode.ItemIndex := Ord(FState.PasswordMode);
  SetPasswordModeUI;
  FillMembershipGrid;
  FillObjectPrivilegeGrids;
  FillDefaultPrivilegesGrid;
end;

procedure TRoleManagerPgForm.ReadFormToState;
var
  i: Integer;
  Item: TMembershipItem;
begin
  FState.RoleName := Trim(edRoleName.Text);
  FState.CanLogin := cbCanLogin.Checked;
  FState.SuperUser := cbSuperUser.Checked;
  FState.CreateDB := cbCreateDB.Checked;
  FState.CreateRole := cbCreateRole.Checked;
  FState.InheritRights := cbInheritRights.Checked;
  FState.Replication := cbReplication.Checked;
  FState.BypassRLS := cbBypassRLS.Checked;
  FState.ConnectionLimit := seConnLimit.Value;
  FState.PasswordMode := PasswordModeFromIndex(cbPasswordMode.ItemIndex);
  FState.PasswordValue := edPassword.Text;

  for i := 1 to vlMemberships.RowCount - 1 do
  begin
    if i-1 >= FState.Memberships.Count then Break;
    Item := TMembershipItem(FState.Memberships[i-1]);
    Item.IsMember := GridCellChecked(vlMemberships, 1, i);
    Item.AdminOption := GridCellChecked(vlMemberships, 2, i);
    if FConn.ServerVersionInt >= 160000 then
    begin
      Item.InheritOption := GridCellChecked(vlMemberships, 3, i);
      Item.SetOption := GridCellChecked(vlMemberships, 4, i);
    end
    else
    begin
      Item.InheritOption := True;
      Item.SetOption := True;
    end;
  end;

  ReadPrivilegeGrid(sgDatabasePrivs, FState.DatabasePrivs);
  ReadPrivilegeGrid(sgSchemaPrivs, FState.SchemaPrivs);
  ReadPrivilegeGrid(sgTableViewPrivs, FState.TableViewPrivs);
  ReadPrivilegeGrid(sgSequencesPrivs, FState.SequencePrivs);
  ReadPrivilegeGrid(sgRoutinePrivs, FState.RoutinePrivs);
  ReadDefaultPrivilegeGrid;
end;

procedure TRoleManagerPgForm.SetPasswordModeUI;
begin
  edPassword.Enabled := cbPasswordMode.ItemIndex = Ord(pmSet);
  if not edPassword.Enabled then
    edPassword.Clear;
end;

procedure TRoleManagerPgForm.FillMembershipGrid;
var
  i: Integer;
  Item: TMembershipItem;
begin
  vlMemberships.RowCount := FState.Memberships.Count + 1;
  for i := 0 to FState.Memberships.Count - 1 do
  begin
    Item := TMembershipItem(FState.Memberships[i]);
    vlMemberships.Cells[0, i+1] := Item.RoleName;
    SetGridCheckbox(vlMemberships, 1, i+1, Item.IsMember);
    SetGridCheckbox(vlMemberships, 2, i+1, Item.AdminOption);
    SetGridCheckbox(vlMemberships, 3, i+1, Item.InheritOption);
    SetGridCheckbox(vlMemberships, 4, i+1, Item.SetOption);
  end;
end;

procedure TRoleManagerPgForm.FillPrivilegeGrid(AGrid: TStringGrid; AList: TObjectList);
var
  i: Integer;
  Item: TObjectPrivItem;
begin
  AGrid.RowCount := AList.Count + 1;
  for i := 0 to AList.Count - 1 do
  begin
    Item := TObjectPrivItem(AList[i]);
    AGrid.Cells[0, i+1] := Item.SchemaName;
    AGrid.Cells[1, i+1] := Item.ObjectName;
    AGrid.Cells[2, i+1] := Item.Privileges.CommaText;
    AGrid.Cells[3, i+1] := Item.Grantable.CommaText;
  end;
end;

procedure TRoleManagerPgForm.FillObjectPrivilegeGrids;
begin
  FillPrivilegeGrid(sgDatabasePrivs, FState.DatabasePrivs);
  FillPrivilegeGrid(sgSchemaPrivs, FState.SchemaPrivs);
  FillPrivilegeGrid(sgTableViewPrivs, FState.TableViewPrivs);
  FillPrivilegeGrid(sgSequencesPrivs, FState.SequencePrivs);
  FillPrivilegeGrid(sgRoutinePrivs, FState.RoutinePrivs);
end;

procedure TRoleManagerPgForm.FillDefaultPrivilegesGrid;
var
  i: Integer;
  Item: TDefaultPrivItem;
begin
  sgDefaultPrivs.RowCount := FState.DefaultPrivs.Count + 1;
  for i := 0 to FState.DefaultPrivs.Count - 1 do
  begin
    Item := TDefaultPrivItem(FState.DefaultPrivs[i]);
    sgDefaultPrivs.Cells[0, i+1] := Item.OwnerName;
    sgDefaultPrivs.Cells[1, i+1] := Item.SchemaName;
    sgDefaultPrivs.Cells[2, i+1] := DefaultPrivKindToToken(Item.Kind);
    sgDefaultPrivs.Cells[3, i+1] := Item.Grantee;
    sgDefaultPrivs.Cells[4, i+1] := Item.Privileges.CommaText;
  end;
end;

procedure TRoleManagerPgForm.ReadPrivilegeGrid(AGrid: TStringGrid; AList: TObjectList);
var
  i: Integer;
  Item: TObjectPrivItem;
  L: TStringList;
begin
  for i := 1 to AGrid.RowCount - 1 do
  begin
    if i-1 >= AList.Count then Break;
    Item := TObjectPrivItem(AList[i-1]);
    L := CsvToList(AGrid.Cells[2, i]);
    try
      Item.Privileges.Assign(L);
    finally
      L.Free;
    end;
    L := CsvToList(AGrid.Cells[3, i]);
    try
      Item.Grantable.Assign(L);
    finally
      L.Free;
    end;
  end;
end;

procedure TRoleManagerPgForm.ReadDefaultPrivilegeGrid;
var
  i: Integer;
  Item: TDefaultPrivItem;
  L: TStringList;
begin
  for i := 1 to sgDefaultPrivs.RowCount - 1 do
  begin
    if i-1 >= FState.DefaultPrivs.Count then Break;
    Item := TDefaultPrivItem(FState.DefaultPrivs[i-1]);
    Item.OwnerName := Trim(sgDefaultPrivs.Cells[0, i]);
    Item.SchemaName := Trim(sgDefaultPrivs.Cells[1, i]);
    Item.Kind := DefaultPrivKindFromString(sgDefaultPrivs.Cells[2, i]);
    Item.Grantee := Trim(sgDefaultPrivs.Cells[3, i]);
    L := CsvToList(sgDefaultPrivs.Cells[4, i]);
    try
      Item.Privileges.Assign(L);
    finally
      L.Free;
    end;
  end;
end;

procedure TRoleManagerPgForm.SaveRole;
begin
  ReadFormToState;
  SaveRoleCore;
  SaveMemberships;
  SaveObjectPrivileges;
  SaveDefaultPrivileges;
  LoadRolesList;
  cmbRole.ItemIndex := cmbRole.Items.IndexOf(FState.RoleName);
  LoadRole(FState.RoleName);
  FModified := False;
end;

procedure TRoleManagerPgForm.SaveRoleCore;
var
  OldName, NewName: String;
begin
  OldName := FState.OriginalRoleName;
  NewName := FState.RoleName;

  if FState.IsNewRole then
    FConn.Query(Sql(qPgRolemanRoleCreate, [
      QuoteIdent(NewName),
      BoolToken('LOGIN', 'NOLOGIN', FState.CanLogin),
      BoolToken('SUPERUSER', 'NOSUPERUSER', FState.SuperUser),
      BoolToken('CREATEDB', 'NOCREATEDB', FState.CreateDB),
      BoolToken('CREATEROLE', 'NOCREATEROLE', FState.CreateRole),
      BoolToken('INHERIT', 'NOINHERIT', FState.InheritRights),
      BoolToken('REPLICATION', 'NOREPLICATION', FState.Replication),
      BoolToken('BYPASSRLS', 'NOBYPASSRLS', FState.BypassRLS),
      FState.ConnectionLimit
    ]))
  else
    FConn.Query(Sql(qPgRolemanRoleAlter, [
      QuoteLiteral(OldName),                 // IF %s <> %s
      QuoteLiteral(NewName),
      QuoteIdent(OldName),                   // dynamic ALTER ROLE old RENAME TO new
      QuoteIdent(NewName),
      QuoteIdent(NewName),                   // final ALTER ROLE %s WITH ...
      BoolToken('LOGIN', 'NOLOGIN', FState.CanLogin),
      BoolToken('SUPERUSER', 'NOSUPERUSER', FState.SuperUser),
      BoolToken('CREATEDB', 'NOCREATEDB', FState.CreateDB),
      BoolToken('CREATEROLE', 'NOCREATEROLE', FState.CreateRole),
      BoolToken('INHERIT', 'NOINHERIT', FState.InheritRights),
      BoolToken('REPLICATION', 'NOREPLICATION', FState.Replication),
      BoolToken('BYPASSRLS', 'NOBYPASSRLS', FState.BypassRLS),
      FState.ConnectionLimit
    ]));

  case FState.PasswordMode of
    pmClear:
      FConn.Query(Sql(qPgRolemanRolePasswordClear, [QuoteIdent(NewName)]));
    pmSet:
      FConn.Query(Sql(qPgRolemanRolePasswordSet, [QuoteIdent(NewName), QuoteLiteral(FState.PasswordValue)]));
  end;
end;

procedure TRoleManagerPgForm.SaveMemberships;
var
  i: Integer;
  Item: TMembershipItem;
  AdminClause: String;
begin
  for i := 0 to FState.Memberships.Count - 1 do
  begin
    Item := TMembershipItem(FState.Memberships[i]);
    AdminClause := IfThen(Item.AdminOption, ' WITH ADMIN OPTION', '');

    if FConn.ServerVersionInt >= 160000 then
    begin
      if Item.IsMember <> Item.OriginalIsMember then
      begin
        if Item.IsMember then
          FConn.Query(Sql(qPgRolemanMembershipGrant, [
            QuoteIdent(Item.RoleName), QuoteIdent(FState.RoleName),
            BoolToken('ADMIN TRUE', 'ADMIN FALSE', Item.AdminOption),
            BoolToken('INHERIT TRUE', 'INHERIT FALSE', Item.InheritOption),
            BoolToken('SET TRUE', 'SET FALSE', Item.SetOption)
          ]))
        else
          FConn.Query(Sql(qPgRolemanMembershipRevoke, [QuoteIdent(Item.RoleName), QuoteIdent(FState.RoleName)]));
      end
      else if Item.IsMember and (
        (Item.AdminOption <> Item.OriginalAdminOption) or
        (Item.InheritOption <> Item.OriginalInheritOption) or
        (Item.SetOption <> Item.OriginalSetOption)
      ) then
        FConn.Query(Sql(qPgRolemanMembershipRegrant, [
          QuoteIdent(Item.RoleName), QuoteIdent(FState.RoleName),
          QuoteIdent(Item.RoleName), QuoteIdent(FState.RoleName),
          BoolToken('ADMIN TRUE', 'ADMIN FALSE', Item.AdminOption),
          BoolToken('INHERIT TRUE', 'INHERIT FALSE', Item.InheritOption),
          BoolToken('SET TRUE', 'SET FALSE', Item.SetOption)
        ]));
    end
    else
    begin
      if Item.IsMember <> Item.OriginalIsMember then
      begin
        if Item.IsMember then
          FConn.Query(Sql(qPgRolemanMembershipGrant, [
            QuoteIdent(Item.RoleName), QuoteIdent(FState.RoleName), AdminClause
          ]))
        else
          FConn.Query(Sql(qPgRolemanMembershipRevoke, [QuoteIdent(Item.RoleName), QuoteIdent(FState.RoleName)]));
      end
      else if Item.IsMember and (Item.AdminOption <> Item.OriginalAdminOption) then
        FConn.Query(Sql(qPgRolemanMembershipRegrant, [
          QuoteIdent(Item.RoleName), QuoteIdent(FState.RoleName),
          QuoteIdent(Item.RoleName), QuoteIdent(FState.RoleName),
          AdminClause
        ]));
    end;
  end;
end;

procedure TRoleManagerPgForm.SavePrivilegeList(AList: TObjectList; const ATypeToken: String;
  const AAllPrivs: array of String);
var
  i, j: Integer;
  Item: TObjectPrivItem;
  RevokeAll, GrantPrivs, GrantGrantable: String;
begin
  RevokeAll := '';
  for j := Low(AAllPrivs) to High(AAllPrivs) do
  begin
    if RevokeAll <> '' then RevokeAll := RevokeAll + ', ';
    RevokeAll := RevokeAll + AAllPrivs[j];
  end;

  for i := 0 to AList.Count - 1 do
  begin
    Item := TObjectPrivItem(AList[i]);
    if (Item.Privileges.CommaText <> Item.OriginalPrivileges.CommaText) or
       (Item.Grantable.CommaText <> Item.OriginalGrantable.CommaText) then
    begin
      FConn.Query(Sql(qPgRolemanObjPrivRevoke, [RevokeAll, ATypeToken, Item.ObjectIdentity, QuoteIdent(FState.RoleName)]));

      GrantPrivs := NormalizePrivilegeCsv(Item.Privileges.CommaText);
      if GrantPrivs <> '' then
        FConn.Query(Sql(qPgRolemanObjPrivGrant, [GrantPrivs, ATypeToken, Item.ObjectIdentity, QuoteIdent(FState.RoleName), '']));

      GrantGrantable := NormalizePrivilegeCsv(Item.Grantable.CommaText);
      if GrantGrantable <> '' then
        FConn.Query(Sql(qPgRolemanObjPrivGrant, [GrantGrantable, ATypeToken, Item.ObjectIdentity, QuoteIdent(FState.RoleName), ' WITH GRANT OPTION']));
    end;
  end;
end;

procedure TRoleManagerPgForm.SaveObjectPrivileges;
begin
  SavePrivilegeList(FState.DatabasePrivs, 'DATABASE', CDbPrivs);
  SavePrivilegeList(FState.SchemaPrivs, 'SCHEMA', CSchemaPrivs);
  SavePrivilegeList(FState.TableViewPrivs, 'TABLE', CTablePrivs);
  SavePrivilegeList(FState.SequencePrivs, 'SEQUENCE', CSequencePrivs);
  SavePrivilegeList(FState.RoutinePrivs, 'FUNCTION', CRoutinePrivs);
end;

procedure TRoleManagerPgForm.SaveDefaultPrivileges;
var
  i: Integer;
  Item: TDefaultPrivItem;
  AllPrivs, SchemaPart, PrivList: String;
begin
  for i := 0 to FState.DefaultPrivs.Count - 1 do
  begin
    Item := TDefaultPrivItem(FState.DefaultPrivs[i]);
    if Item.Privileges.CommaText = Item.OriginalPrivileges.CommaText then
      Continue;

    case Item.Kind of
      dpTables: AllPrivs := 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER';
      dpSequences: AllPrivs := 'USAGE, SELECT, UPDATE';
    else
      AllPrivs := 'EXECUTE';
    end;

    if Trim(Item.SchemaName) <> '' then
      SchemaPart := ' IN SCHEMA ' + QuoteIdent(Item.SchemaName)
    else
      SchemaPart := '';

    FConn.Query(Sql(qPgRolemanDefaultPrivRevoke, [
      QuoteIdent(Item.OwnerName), SchemaPart, AllPrivs, DefaultPrivKindToToken(Item.Kind), QuoteIdent(FState.RoleName)
    ]));

    PrivList := NormalizePrivilegeCsv(Item.Privileges.CommaText);
    if PrivList <> '' then
      FConn.Query(Sql(qPgRolemanDefaultPrivGrant, [
        QuoteIdent(Item.OwnerName), SchemaPart, PrivList, DefaultPrivKindToToken(Item.Kind), QuoteIdent(FState.RoleName), ''
      ]));
  end;
end;

procedure TRoleManagerPgForm.DropRole;
begin
  if not ConfirmDrop then Exit;
  FConn.Query(Sql(qPgRolemanRoleDrop, [QuoteIdent(FState.RoleName)]));
  LoadRolesList;
  if cmbRole.Items.Count > 0 then
  begin
    cmbRole.ItemIndex := 0;
    LoadRole(cmbRole.Text);
  end else
    btnNewClick(nil);
end;

procedure TRoleManagerPgForm.CloneRole;
var
  SrcName: String;
begin
  ReadFormToState;
  SrcName := FState.RoleName;
  FState.IsNewRole := True;
  FState.IsCloneRole := True;
  FState.OriginalRoleName := '';
  FState.PasswordMode := pmUnchanged;
  FState.PasswordValue := '';
  FState.RoleName := SrcName + '_copy';
  PopulateFormFromState;
  FModified := True;
end;

procedure TRoleManagerPgForm.MarkModified;
begin
  if not FLoading then
    FModified := True;
end;

function TRoleManagerPgForm.ConfirmDrop: Boolean;
begin
  Result := MessageDlg('Drop role', 'Really drop role "' + FState.RoleName + '"?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

function TRoleManagerPgForm.ConfirmRoleSwitch: Boolean;
begin
  if not FModified then Exit(True);
  Result := MessageDlg('Unsaved changes', 'Discard unsaved changes?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

function TRoleManagerPgForm.SelectedRole: String;
begin
  Result := Trim(cmbRole.Text);
end;

function TRoleManagerPgForm.Sql(AId: TQueryId): String;
begin
  Result := FConn.SqlProvider.GetSql(AId);
end;

function TRoleManagerPgForm.Sql(AId: TQueryId; const Args: array of const): String;
begin
  Result := FConn.SqlProvider.GetSql(AId, Args);
end;

function TRoleManagerPgForm.QuoteIdent(const S: String): String;
begin
  Result := FConn.QuoteIdent(S);
end;

function TRoleManagerPgForm.QuoteLiteral(const S: String): String;
begin
  Result := FConn.EscapeString(S);
end;

function TRoleManagerPgForm.BoolToken(const TrueToken, FalseToken: String; const Value: Boolean): String;
begin
  if Value then Result := TrueToken else Result := FalseToken;
end;

function TRoleManagerPgForm.PasswordModeFromIndex(AIndex: Integer): TPasswordMode;
begin
  case AIndex of
    1: Result := pmClear;
    2: Result := pmSet;
  else
    Result := pmUnchanged;
  end;
end;

function TRoleManagerPgForm.CsvToList(const S: String): TStringList;
var
  Tmp: String;
  Parts: TStringArray;
  i: Integer;
begin
  Result := TStringList.Create;
  Result.StrictDelimiter := True;
  Result.CaseSensitive := False;
  Result.Duplicates := dupIgnore;
  Tmp := StringReplace(Trim(S), ', ', ',', [rfReplaceAll]);
  if Tmp = '' then Exit;
  Parts := SplitString(Tmp, ',');
  for i := 0 to High(Parts) do
    if Trim(Parts[i]) <> '' then
      Result.Add(UpperCase(Trim(Parts[i])));
end;

function TRoleManagerPgForm.NormalizePrivilegeCsv(const S: String): String;
var
  L: TStringList;
  i: Integer;
begin
  L := CsvToList(S);
  try
    Result := '';
    for i := 0 to L.Count - 1 do
    begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + L[i];
    end;
  finally
    L.Free;
  end;
end;

function TRoleManagerPgForm.DefaultPrivKindToToken(AKind: TDefaultPrivKind): String;
begin
  case AKind of
    dpTables: Result := 'TABLES';
    dpSequences: Result := 'SEQUENCES';
  else
    Result := 'ROUTINES';
  end;
end;

function TRoleManagerPgForm.DefaultPrivKindFromString(const S: String): TDefaultPrivKind;
begin
  if SameText(Trim(S), 'SEQUENCES') then
    Result := dpSequences
  else if SameText(Trim(S), 'ROUTINES') then
    Result := dpRoutines
  else
    Result := dpTables;
end;

function TRoleManagerPgForm.GridCellChecked(AGrid: TStringGrid; ACol, ARow: Integer): Boolean;
begin
  Result := SameText(AGrid.Cells[ACol, ARow], '1') or
    ((ACol < AGrid.Columns.Count) and (AGrid.Cells[ACol, ARow] = AGrid.Columns[ACol].ValueChecked));
end;

procedure TRoleManagerPgForm.SetGridCheckbox(AGrid: TStringGrid; ACol, ARow: Integer; AValue: Boolean);
begin
  if (ACol < AGrid.Columns.Count) then
  begin
    if AValue then
      AGrid.Cells[ACol, ARow] := AGrid.Columns[ACol].ValueChecked
    else
      AGrid.Cells[ACol, ARow] := AGrid.Columns[ACol].ValueUnchecked;
  end else begin
    if AValue then
      AGrid.Cells[ACol, ARow] := '1'
    else
      AGrid.Cells[ACol, ARow] := '0';
  end;
end;

procedure TRoleManagerPgForm.Log(const S: String);
begin
  {$IFDEF DEBUG}
  memoLog.Lines.Add(S);
  {$ENDIF}
end;

procedure TRoleManagerPgForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TRoleManagerPgForm.btnCloneClick(Sender: TObject);
begin
  CloneRole;
end;

procedure TRoleManagerPgForm.btnDropClick(Sender: TObject);
begin
  DropRole;
end;

procedure TRoleManagerPgForm.btnNewClick(Sender: TObject);
begin
  if not ConfirmRoleSwitch then Exit;
  FState.Clear;
  FState.IsNewRole := True;
  FState.RoleName := 'new_role';
  PopulateFormFromState;
  edRoleName.SetFocus;
  FModified := True;
end;

procedure TRoleManagerPgForm.btnSaveClick(Sender: TObject);
begin
  SaveRole;
end;

procedure TRoleManagerPgForm.cbPasswordModeChange(Sender: TObject);
begin
  if FLoading then Exit;
  SetPasswordModeUI;
  MarkModified;
end;

procedure TRoleManagerPgForm.cmbRoleChange(Sender: TObject);
begin
  if FLoading then Exit;
  if not ConfirmRoleSwitch then Exit;
  if SelectedRole <> '' then
    LoadRole(SelectedRole);
end;

procedure TRoleManagerPgForm.vlMembershipsCheckboxToggled(Sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  if FLoading then Exit;
  if (aRow > 0) and (aCol >= 1) and (aCol <= 4) then
    MarkModified;
end;

end.
