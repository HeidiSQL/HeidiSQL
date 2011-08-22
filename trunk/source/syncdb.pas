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
    procedure treeSourceChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure treeSourceGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure treeSourceGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure treeSourceGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure treeSourceInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure treeSourceInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure treeSourcePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
begin
  InheritFont(Font);
  SetWindowSizeGrip(Self.Handle, True);
  FixVT(treeSource);
  FixVT(treeDifferences);
  treeSource.RootNodeCount := Mainform.DBtree.RootNodeCount;
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
  MainForm.DBtree.OnChange(Sender, Node);
end;


procedure TfrmSyncDB.treeSourceGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  MainForm.DBtree.OnGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex);
end;


procedure TfrmSyncDB.treeSourceGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  MainForm.DBtree.OnGetNodeDataSize(Sender, NodeDataSize);
end;


procedure TfrmSyncDB.treeSourceGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  MainForm.DBtree.OnGetText(Sender, Node, Column, TextType, CellText);
end;


procedure TfrmSyncDB.treeSourceInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  MainForm.DBtree.OnInitChildren(Sender, Node, ChildCount);
end;


procedure TfrmSyncDB.treeSourceInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  MainForm.DBtree.OnInitNode(Sender, ParentNode, Node, InitialStates);
  if Sender.GetNodeLevel(Node) in [1, 2] then begin
    Node.CheckType := ctTriStateCheckBox;
    Node.CheckState := csUncheckedNormal;
  end;
end;


procedure TfrmSyncDB.treeSourcePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  MainForm.DBtree.OnPaintText(Sender, TargetCanvas, Node, Column, TextType);
end;

end.
