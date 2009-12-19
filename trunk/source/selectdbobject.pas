unit selectdbobject;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VirtualTrees, mysql_connection, WideStrings,
  TntStdCtrls;

type
  TfrmSelectDBObject = class(TForm)
    TreeDBO: TVirtualStringTree;
    btnOK: TButton;
    btnCancel: TButton;
    lblSelect: TLabel;
    editDB: TTnTEdit;
    editTable: TTnTEdit;
    editCol: TTnTEdit;
    lblDB: TLabel;
    lblTable: TLabel;
    lblCol: TLabel;
    lblHint: TLabel;
    procedure editChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeDBOFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex);
    procedure TreeDBOGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var
        ImageIndex: Integer);
    procedure TreeDBOGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize:
        Integer);
    procedure TreeDBOGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure TreeDBOInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var
        ChildCount: Cardinal);
    procedure TreeDBOInitNode(Sender: TBaseVirtualTree; ParentNode, Node:
        PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    { Private declarations }
    FColumns: Array of Array of TWideStringList;
    function GetSelectedObject: TWideStringList;
  public
    { Public declarations }
    property SelectedObject: TWideStringList read GetSelectedObject;
  end;

function SelectDBO: TWideStringList;

implementation

uses main, helpers;

{$R *.dfm}

function SelectDBO: TWideStringList;
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
  FixVT(TreeDBO);
end;

procedure TfrmSelectDBObject.FormDestroy(Sender: TObject);
begin
  OpenRegistry;
  MainReg.WriteInteger( REGNAME_SELECTDBO_WINWIDTH, Width );
  MainReg.WriteInteger( REGNAME_SELECTDBO_WINHEIGHT, Height );
end;

procedure TfrmSelectDBObject.FormResize(Sender: TObject);
var
  EditWidth: Integer;
const
  space = 3;
begin
  // Calculate width for 1 TEdit
  EditWidth := (TreeDBO.Width - 2*space) div 3;
  // Set widths
  editDb.Width := EditWidth;
  editTable.Width := EditWidth;
  editCol.Width := EditWidth;
  // Set position of TEdits
  editDb.Left := TreeDBO.Left;
  editTable.Left := TreeDBO.Left + EditWidth + space;
  editCol.Left := TreeDBO.Left + 2*(EditWidth + space);
  // Set position of TLabels
  lblDB.Left := editDB.Left;
  lblTable.Left := editTable.Left;
  lblCol.Left := editCol.Left;
end;

procedure TfrmSelectDBObject.FormShow(Sender: TObject);
begin
  TreeDBO.Clear;
  TreeDBO.RootNodeCount := Mainform.Databases.Count;
  SetLength(FColumns, Mainform.Databases.Count);
//  TreeDBO.OnFocusChanged(TreeDBO, TreeDBO.FocusedNode, 0);
  editDB.Clear;
  editTable.Clear;
  editCol.Clear;
  editChange(Sender);
end;


function TfrmSelectDBObject.GetSelectedObject: TWideStringList;
begin
  Result := nil;
  if editDb.Text <> '' then begin
    Result := TWideStringList.Create;
    Result.Add(editDb.Text);
    if editTable.Text <> '' then Result.Add(editTable.Text);
    if editCol.Text <> '' then Result.Add(editCol.Text);
  end;
  // Let the result be nil to indicate we have no selected node
end;

procedure TfrmSelectDBObject.TreeDBOFocusChanged(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex);
begin
  editDB.Clear;
  editTable.Clear;
  editCol.Clear;
  if Assigned(Node) then case TreeDBO.GetNodeLevel(Node) of
    0: editDB.Text := TreeDBO.Text[Node, 0];
    1: begin
      editDB.Text := TreeDBO.Text[Node.Parent, 0];
      editTable.Text := TreeDBO.Text[Node, 0];
    end;
    2: begin
      editDB.Text := TreeDBO.Text[Node.Parent.Parent, 0];
      editTable.Text := TreeDBO.Text[Node.Parent, 0];
      editCol.Text := TreeDBO.Text[Node, 0];
    end;
  end;
end;


procedure TfrmSelectDBObject.editChange(Sender: TObject);
begin
  // DB must be filled
  btnOK.Enabled := editDB.Text <> '';
  // If col given, check if table is also given
  if editCol.Text <> '' then
    btnOK.Enabled := editTable.Text <> '';
end;


procedure TfrmSelectDBObject.TreeDBOGetImageIndex(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted:
    Boolean; var ImageIndex: Integer);
var
  DBObjects: TDBObjectList;
begin
  case Sender.GetNodeLevel(Node) of
    0: ImageIndex := ICONINDEX_DB;
    1: begin
        DBObjects := Mainform.Connection.GetDBObjects(Mainform.Databases[Node.Parent.Index]);
        case DBObjects[Node.Index].NodeType of
          lntCrashedTable: ImageIndex := ICONINDEX_CRASHED_TABLE;
          lntTable: ImageIndex := ICONINDEX_TABLE;
          lntView: ImageIndex := ICONINDEX_VIEW;
        end;
      end;
    2: ImageIndex := ICONINDEX_FIELD;
  end;
end;


procedure TfrmSelectDBObject.TreeDBOGetNodeDataSize(Sender: TBaseVirtualTree;
    var NodeDataSize: Integer);
begin
  NodeDataSize := 0;
end;


procedure TfrmSelectDBObject.TreeDBOInitChildren(Sender: TBaseVirtualTree;
    Node: PVirtualNode; var ChildCount: Cardinal);
var
  DBObjects: TDBObjectList;
  cols: TWideStringList;
begin
  // Fetch sub nodes
  case Sender.GetNodeLevel(Node) of
    0: begin // DB expanding
        DBObjects := Mainform.Connection.GetDBObjects(Mainform.Databases[Node.Index]);
        ChildCount := DBObjects.Count;
        SetLength(FColumns[Node.Index], DBObjects.Count);
      end;
    1: begin // Table expanding
        DBObjects := Mainform.Connection.GetDBObjects(Mainform.Databases[Node.Parent.Index]);
        cols := Mainform.Connection.GetCol('SHOW COLUMNS FROM '
          + Mainform.mask(Mainform.Databases[Node.Parent.Index])+'.'
          + Mainform.Mask(DBObjects[Node.Index].Name));
        FColumns[Node.Parent.Index][Node.Index] := cols;
        ChildCount := cols.Count;
      end;
  end;

end;


procedure TfrmSelectDBObject.TreeDBOGetText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText:
    WideString);
var
  DBObjects: TDBObjectList;
begin
  case Sender.GetNodeLevel(Node) of
    0: CellText := Mainform.Databases[Node.Index];
    1: begin
        DBObjects := Mainform.Connection.GetDBObjects(Mainform.Databases[Node.Parent.Index]);
        CellText := DBObjects[Node.Index].Name
      end;
    2: CellText := FColumns[Node.Parent.Parent.Index][Node.Parent.Index][Node.Index];
  end;
end;


procedure TfrmSelectDBObject.TreeDBOInitNode(Sender: TBaseVirtualTree;
    ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  // Ensure plus sign is visible for dbs and tables
  if Sender.GetNodeLevel(Node) in [0,1] then
    InitialStates := InitialStates + [ivsHasChildren];
end;

end.
