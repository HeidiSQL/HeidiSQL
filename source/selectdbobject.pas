unit selectdbobject;

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, VirtualTrees, Graphics, extra_controls,
  dbconnection, gnugettext;

type
  TfrmSelectDBObject = class(TExtForm)
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
        ImageIndex: TImageIndex);
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
    function GetSelectedObjects: TDBObjectList;
  public
    { Public declarations }
    property SelectedObjects: TDBObjectList read GetSelectedObjects;
  end;

function SelectDBObjects: TDBObjectList;

implementation

uses main, apphelpers;

{$R *.dfm}

function SelectDBObjects: TDBObjectList;
var
  Dialog: TfrmSelectDBObject;
begin
  Dialog := TfrmSelectDBObject.Create(Mainform);
  Result := nil;
  if Dialog.ShowModal = mrOK then
    Result := Dialog.SelectedObjects;
  Dialog.Free;
end;


procedure TfrmSelectDBObject.FormCreate(Sender: TObject);
begin
  HasSizeGrip := True;
  Width := AppSettings.ReadInt(asSelectDBOWindowWidth);
  Height := AppSettings.ReadInt(asSelectDBOWindowHeight);
  TreeDBO.TreeOptions := MainForm.DBtree.TreeOptions;
  TreeDBO.TreeOptions.SelectionOptions := TreeDBO.TreeOptions.SelectionOptions + [toMultiSelect];
  FixVT(TreeDBO);
  FConnection := MainForm.ActiveConnection;
end;

procedure TfrmSelectDBObject.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteInt(asSelectDBOWindowWidth, Width);
  AppSettings.WriteInt(asSelectDBOWindowHeight, Height);
end;


procedure TfrmSelectDBObject.ValidateControls(Sender: TObject);
begin
  // Signalize if tree or edit box is used
  if editDb.Modified then begin
    TreeDBO.Color := GetThemeColor(clBtnFace);
    editDb.Color := GetThemeColor(clWindow);
    btnOK.Enabled := editDb.GetTextLen > 0;
  end else begin
    TreeDBO.Color := GetThemeColor(clWindow);
    editDb.Color := GetThemeColor(clBtnFace);
    btnOK.Enabled := Assigned(TreeDBO.FocusedNode);
  end;
end;


procedure TfrmSelectDBObject.FormShow(Sender: TObject);
begin
  TreeDBO.Clear;
  TreeDBO.RootNodeCount := Mainform.DBtree.RootNodeCount;
  TreeDBO.OnFocusChanged(TreeDBO, TreeDBO.FocusedNode, 0);
end;


function TfrmSelectDBObject.GetSelectedObjects: TDBObjectList;
var
  Obj: TDBObject;
  DBObj: PDBObject;
  Node: PVirtualNode;
begin
  // Return currently selected object, either from tree node or from edit boxes
  Result := TDBObjectList.Create;
  if editDb.Modified then begin
    Obj := TDBObject.Create(FConnection);
    Obj.Database := editDb.Text;
    Obj.NodeType := lntDb;
    Result.Add(Obj);
  end else if TreeDBO.SelectedCount > 0 then begin
    Node := GetNextNode(TreeDBO, nil, True);
    while Assigned(Node) do begin
      DBObj := TreeDBO.GetNodeData(Node);
      Obj := TDBObject.Create(DBObj.Connection);
      Obj.Assign(DBObj^);
      // Database privileges can be wildcarded. Tables/columns not so.
      if Obj.NodeType = lntDb then
        Obj.Database := esc(Obj.Database, True, False);
      if Obj.NodeType = lntNone then begin
        Obj.NodeType := lntDb;
        Obj.Database := '%';
      end;
      Result.Add(Obj);
      Node := GetNextNode(TreeDBO, Node, True);
    end;
  end;
  // Let the result be empty to indicate we have no selected node
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
    Boolean; var ImageIndex: TImageIndex);
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
