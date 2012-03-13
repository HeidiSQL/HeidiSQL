unit selectdbobject;

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, VirtualTrees, Graphics,
  dbconnection;

type
  TfrmSelectDBObject = class(TForm)
    TreeDBO: TVirtualStringTree;
    btnOK: TButton;
    btnCancel: TButton;
    lblSelect: TLabel;
    lblCustom: TLabel;
    editDb: TEdit;
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
    procedure ValidateControls(Sender: TObject);
  private
    { Private declarations }
    FConnection: TDBConnection;
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
var
  Dialog: TfrmSelectDBObject;
begin
  Dialog := TfrmSelectDBObject.Create(Mainform);
  Result := nil;
  if Dialog.ShowModal = mrOK then
    Result := Dialog.SelectedObject;
  Dialog.Free;
end;


procedure TfrmSelectDBObject.FormCreate(Sender: TObject);
begin
  Width := GetRegValue(REGNAME_SELECTDBO_WINWIDTH, Width);
  Height := GetRegValue(REGNAME_SELECTDBO_WINHEIGHT, Height);
  SetWindowSizeGrip( Self.Handle, True );
  InheritFont(Font);
  TreeDBO.TreeOptions := MainForm.DBtree.TreeOptions;
  FixVT(TreeDBO);
  FConnection := MainForm.ActiveConnection;
end;

procedure TfrmSelectDBObject.FormDestroy(Sender: TObject);
begin
  OpenRegistry;
  MainReg.WriteInteger( REGNAME_SELECTDBO_WINWIDTH, Width );
  MainReg.WriteInteger( REGNAME_SELECTDBO_WINHEIGHT, Height );
end;


procedure TfrmSelectDBObject.ValidateControls(Sender: TObject);
begin
  // Signalize if tree or edit box is used
  if editDb.Modified then begin
    TreeDBO.Color := clBtnFace;
    editDb.Color := clWindow;
    btnOK.Enabled := editDb.GetTextLen > 0;
  end else begin
    TreeDBO.Color := clWindow;
    editDb.Color := clBtnFace;
    btnOK.Enabled := Assigned(TreeDBO.FocusedNode);
  end;
end;


procedure TfrmSelectDBObject.FormShow(Sender: TObject);
begin
  TreeDBO.Clear;
  TreeDBO.RootNodeCount := Mainform.DBtree.RootNodeCount;
  TreeDBO.OnFocusChanged(TreeDBO, TreeDBO.FocusedNode, 0);
end;


function TfrmSelectDBObject.GetSelectedObject: TDBObject;
var
  DBObj: PDBObject;
begin
  // Return currently selected object, either from tree node or from edit boxes
  Result := nil;
  if editDb.Modified then begin
    Result := TDBObject.Create(FConnection);
    Result.Database := editDb.Text;
    Result.NodeType := lntDb;
  end else if Assigned(TreeDBO.FocusedNode) then begin
    DBObj := TreeDBO.GetNodeData(TreeDBO.FocusedNode);
    Result := TDBObject.Create(DBObj.Connection);
    Result.Assign(DBObj^);
    // Database privileges can be wildcarded. Tables/columns not so.
    if Result.NodeType = lntDb then
      Result.Database := esc(Result.Database, True, False);
    if Result.NodeType = lntNone then begin
      Result.NodeType := lntDb;
      Result.Database := '%';
    end;
  end;
  // Let the result be nil to indicate we have no selected node
end;

procedure TfrmSelectDBObject.TreeDBOFocusChanged(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex);
var
  Tree: TVirtualStringTree;
begin
  // Overtake node text into lower edit boxes
  editDb.Clear;
  Tree := Sender as TVirtualStringTree;
  if Assigned(Node) then begin
    case Sender.GetNodeLevel(Node) of
      0: editDb.Text := '%';
      1: editDb.Text := esc(Tree.Text[Node, 0], True, False);
      2: editDb.Text := esc(Tree.Text[Node.Parent, 0], True, False);
      3: editDb.Text := esc(Tree.Text[Node.Parent.Parent, 0], True, False);
    end;
  end;
  // Indicate automatic changes only
  editDb.Modified := False;
  ValidateControls(Sender);
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
  if DBObj.Connection <> FConnection then begin
    Include(InitialStates, ivsDisabled);
    Exclude(InitialStates, ivsHasChildren);
  end else if DBObj.NodeType = lntNone then
    Include(InitialStates, ivsExpanded);
end;

end.
