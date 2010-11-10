unit selectdbobject;

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, VirtualTrees,
  mysql_connection;

type
  TfrmSelectDBObject = class(TForm)
    TreeDBO: TVirtualStringTree;
    btnOK: TButton;
    btnCancel: TButton;
    lblSelect: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeDBOFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex);
    procedure TreeDBOGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var
        ImageIndex: Integer);
    procedure TreeDBOGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize:
        Integer);
    procedure TreeDBOGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure TreeDBOInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var
        ChildCount: Cardinal);
    procedure TreeDBOInitNode(Sender: TBaseVirtualTree; ParentNode, Node:
        PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    { Private declarations }
    FColumns: Array of Array of TStringList;
    function GetSelectedObject: TDBObject;
  public
    { Public declarations }
    property SelectedObject: TDBObject read GetSelectedObject;
  end;

function SelectDBO: TDBObject;

implementation

uses main, helpers;

{$R *.dfm}

function SelectDBO: TDBObject;
begin
  if Mainform.SelectDBObjectForm = nil then
    Mainform.SelectDBObjectForm := TfrmSelectDBObject.Create(Mainform);
  Result := nil;
  if Mainform.SelectDBObjectForm.ShowModal = mrOK then
    Result := Mainform.SelectDBObjectForm.SelectedObject;
end;


procedure TfrmSelectDBObject.FormCreate(Sender: TObject);
begin
  Width := GetRegValue(REGNAME_SELECTDBO_WINWIDTH, Width);
  Height := GetRegValue(REGNAME_SELECTDBO_WINHEIGHT, Height);
  SetWindowSizeGrip( Self.Handle, True );
  InheritFont(Font);
  TreeDBO.TreeOptions := MainForm.DBtree.TreeOptions;
  FixVT(TreeDBO);
end;

procedure TfrmSelectDBObject.FormDestroy(Sender: TObject);
begin
  OpenRegistry;
  MainReg.WriteInteger( REGNAME_SELECTDBO_WINWIDTH, Width );
  MainReg.WriteInteger( REGNAME_SELECTDBO_WINHEIGHT, Height );
end;

procedure TfrmSelectDBObject.FormShow(Sender: TObject);
begin
  TreeDBO.Clear;
  TreeDBO.RootNodeCount := Mainform.DBtree.RootNodeCount;
  SetLength(FColumns, Mainform.ActiveConnection.AllDatabases.Count);
//  TreeDBO.OnFocusChanged(TreeDBO, TreeDBO.FocusedNode, 0);
end;


function TfrmSelectDBObject.GetSelectedObject: TDBObject;
var
  DBObj: PDBObject;
begin
  Result := nil;
  if Assigned(TreeDBO.FocusedNode) then begin
    DBObj := TreeDBO.GetNodeData(TreeDBO.FocusedNode);
    Result := TDBObject.Create(DBObj.Connection);
    Result.Assign(DBObj^);
  end;
  // Let the result be nil to indicate we have no selected node
end;

procedure TfrmSelectDBObject.TreeDBOFocusChanged(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex);
begin
  btnOK.Enabled := Assigned(Node);
end;


procedure TfrmSelectDBObject.TreeDBOGetImageIndex(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted:
    Boolean; var ImageIndex: Integer);
begin
  Mainform.DBtreeGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex);
end;


procedure TfrmSelectDBObject.TreeDBOGetNodeDataSize(Sender: TBaseVirtualTree;
    var NodeDataSize: Integer);
begin
  MainForm.DBtreeGetNodeDataSize(Sender, NodeDataSize);
end;


procedure TfrmSelectDBObject.TreeDBOInitChildren(Sender: TBaseVirtualTree;
    Node: PVirtualNode; var ChildCount: Cardinal);
begin
  // Fetch sub nodes
  Mainform.DBtreeInitChildren(Sender, Node, ChildCount);
end;


procedure TfrmSelectDBObject.TreeDBOGetText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText:
    String);
begin
  Mainform.DBtreeGetText(Sender, Node, Column, TextType, CellText);
end;


procedure TfrmSelectDBObject.TreeDBOInitNode(Sender: TBaseVirtualTree;
    ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  DBObj: PDBObject;
begin
  Mainform.DBtreeInitNode(Sender, ParentNode, Node, InitialStates);
  DBObj := Sender.GetNodeData(Node);
  if DBObj.Connection <> MainForm.ActiveConnection then begin
    Include(InitialStates, ivsDisabled);
    Exclude(InitialStates, ivsHasChildren);
  end else if DBObj.NodeType = lntNone then
    Include(InitialStates, ivsExpanded);
end;

end.
