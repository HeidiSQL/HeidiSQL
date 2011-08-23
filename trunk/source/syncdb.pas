unit syncdb;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VirtualTrees;

type
  TfrmSyncDB = class(TForm)
    treeSource: TVirtualStringTree;
    lblSource: TLabel;
    grpTarget: TGroupBox;
    comboTargetServer: TComboBox;
    lblTargetServer: TLabel;
    lblTargetDatabase: TLabel;
    comboTargetDatabase: TComboBox;
    comboTargetTable: TComboBox;
    lblTargetTable: TLabel;
    btnClose: TButton;
    btnApply: TButton;
    btnAnalyze: TButton;
    grpOptions: TGroupBox;
    radioOptionsStructure: TCheckBox;
    radioOptionsData: TCheckBox;
    treeDifferences: TVirtualStringTree;
    lblDifferences: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure treeSourceChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure treeSourceChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure treeSourceChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure treeSourceGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure treeSourceGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure treeSourceGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure treeSourceInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure treeSourceInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure treeSourcePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure comboTargetServerChange(Sender: TObject);
    procedure comboTargetDatabaseChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses main, helpers, dbconnection;

{$R *.dfm}



procedure TfrmSyncDB.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure TfrmSyncDB.FormCreate(Sender: TObject);
var
  SessNode: PVirtualNode;
begin
  InheritFont(Font);
  SetWindowSizeGrip(Self.Handle, True);
  FixVT(treeSource);
  FixVT(treeDifferences);

  // Init session nodes and axpand active connection node
  treeSource.RootNodeCount := Mainform.DBtree.RootNodeCount;
  SessNode := MainForm.GetRootNode(treeSource, MainForm.ActiveConnection);
  if Assigned(SessNode) then
    treeSource.ToggleNode(SessNode);

  MainReg.OpenKey(RegPath + REGKEY_SESSIONS, True);
  MainReg.GetKeyNames(comboTargetServer.Items);
  comboTargetServer.Items.Insert(0, 'Select server session ...');
  comboTargetServer.ItemIndex := 0;
end;


procedure TfrmSyncDB.comboTargetServerChange(Sender: TObject);
var
  Parameters: TConnectionParameters;
  Connection: TDBConnection;
begin
  // Populate database drop down
  comboTargetDatabase.Clear;
  if comboTargetServer.ItemIndex > 0 then begin
    Parameters := LoadConnectionParams(comboTargetServer.Text);
    Connection := Parameters.CreateConnection(Self);
    Connection.OnLog := MainForm.LogSQL;
    Connection.LogPrefix := comboTargetServer.Text;
    Connection.Active := True;
    comboTargetDatabase.Items.Assign(Connection.AllDatabases);
    Connection.Active := False;
    Connection.Free;
  end;
  comboTargetDatabase.Items.Insert(0, '[Same as source]');
  comboTargetDatabase.ItemIndex := 0;
end;


procedure TfrmSyncDB.comboTargetDatabaseChange(Sender: TObject);
var
  Parameters: TConnectionParameters;
  Connection: TDBConnection;
  Objects: TDBObjectList;
  Obj: TDBObject;
begin
  // Populate table drop down
  comboTargetTable.Clear;
  if comboTargetDatabase.ItemIndex > 0 then begin
    Parameters := LoadConnectionParams(comboTargetServer.Text);
    Connection := Parameters.CreateConnection(Self);
    Connection.OnLog := MainForm.LogSQL;
    Connection.LogPrefix := comboTargetServer.Text;
    Connection.Active := True;
    Objects := Connection.GetDBObjects(comboTargetDatabase.Text);
    for Obj in Objects do begin
      if Obj.NodeType = lntTable then
        comboTargetTable.Items.Add(Obj.Name);
    end;
    Connection.Active := False;
    Connection.Free;
  end;
  comboTargetTable.Items.Insert(0, '[Same as source]');
  comboTargetTable.ItemIndex := 0;
end;


procedure TfrmSyncDB.treeSourceChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  // Clone logic of MainForm.DBtree
  MainForm.DBtree.OnChange(Sender, Node);
end;


procedure TfrmSyncDB.treeSourceChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  DB, Table: PVirtualNode;
begin
  // Uncheck table nodes other than current checked one
  if (Sender.GetNodeLevel(Node) = 1) and (Node.CheckState in CheckedStates) then begin
    DB := Sender.GetFirstChild(Node.Parent);
    while Assigned(DB) do begin
      if DB <> Node then
        Sender.CheckState[DB] := csUncheckedNormal;
      DB := Sender.GetNextSibling(DB);
    end;
  end;
end;


procedure TfrmSyncDB.treeSourceChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var NewState: TCheckState; var Allowed: Boolean);
var
  n: PVirtualNode;
begin
  // Init nodes so all children get checked
  Allowed := True;
  n := Sender.GetFirstChild(Node);
  while Assigned(n) do
    n := Sender.GetNextSibling(n);
end;


procedure TfrmSyncDB.treeSourceGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  // Clone logic of MainForm.DBtree
  MainForm.DBtree.OnGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex);
end;


procedure TfrmSyncDB.treeSourceGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  // Clone logic of MainForm.DBtree
  MainForm.DBtree.OnGetNodeDataSize(Sender, NodeDataSize);
end;


procedure TfrmSyncDB.treeSourceGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  // Clone logic of MainForm.DBtree
  MainForm.DBtree.OnGetText(Sender, Node, Column, TextType, CellText);
end;


procedure TfrmSyncDB.treeSourceInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  // Clone logic of MainForm.DBtree
  MainForm.DBtree.OnInitChildren(Sender, Node, ChildCount);
end;


procedure TfrmSyncDB.treeSourceInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  // Clone logic of MainForm.DBtree
  MainForm.DBtree.OnInitNode(Sender, ParentNode, Node, InitialStates);
  // Checkbox for db and table nodes
  if Sender.GetNodeLevel(Node) in [1, 2] then begin
    Node.CheckType := ctTriStateCheckBox;
    Node.CheckState := csUncheckedNormal;
  end;
end;


procedure TfrmSyncDB.treeSourcePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  // Clone logic of MainForm.DBtree
  MainForm.DBtree.OnPaintText(Sender, TargetCanvas, Node, Column, TextType);
end;


end.
