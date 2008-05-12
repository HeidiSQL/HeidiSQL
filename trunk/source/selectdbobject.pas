unit selectdbobject;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VirtualTrees, Childwin, DB, Registry;

type
  TfrmSelectDBObject = class(TForm)
    TreeDBO: TVirtualStringTree;
    btnOK: TButton;
    btnCancel: TButton;
    lblSelect: TLabel;
    editDB: TEdit;
    editTable: TEdit;
    editCol: TEdit;
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
    FDatabases: TStringList;
    FColumns: Array of Array of TStringList;
    function GetSelectedObject: TStringList;
  public
    { Public declarations }
    property SelectedObject: TStringList read GetSelectedObject;
  end;

function SelectDBO: TStringList;

var
  CWin: TMDIChild;

implementation

uses main, helpers;

{$R *.dfm}

function SelectDBO: TStringList;
begin
  if Mainform.SelectDBObjectForm = nil then
    Mainform.SelectDBObjectForm := TfrmSelectDBObject.Create(Mainform);
  Result := nil;
  if Mainform.SelectDBObjectForm.ShowModal = mrOK then
    Result := Mainform.SelectDBObjectForm.SelectedObject;
end;


procedure TfrmSelectDBObject.FormCreate(Sender: TObject);
begin
  Width := Mainform.GetRegValue(REGNAME_SELECTDBO_WINWIDTH, Width);
  Height := Mainform.GetRegValue(REGNAME_SELECTDBO_WINHEIGHT, Height);
  SetWindowSizeGrip( Self.Handle, True );
end;

procedure TfrmSelectDBObject.FormDestroy(Sender: TObject);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  if reg.OpenKey(REGPATH, False) then begin
    reg.WriteInteger( REGNAME_SELECTDBO_WINWIDTH, Width );
    reg.WriteInteger( REGNAME_SELECTDBO_WINHEIGHT, Height );
    reg.CloseKey;
  end;
  reg.Free;
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
var
  i: Integer;
begin
  CWin := Mainform.Childwin;
  TreeDBO.Clear;
  FDatabases := TStringList.Create;
  for i := 0 to CWin.DBtree.Items.Count - 1 do begin
    if CWin.DBtree.Items[i].Level <> 1 then
      continue;
    FDatabases.Add(CWin.DBtree.Items[i].Text);
  end;
  TreeDBO.RootNodeCount := FDatabases.Count;
  SetLength(FColumns, FDatabases.Count);
//  TreeDBO.OnFocusChanged(TreeDBO, TreeDBO.FocusedNode, 0);
  editDB.Clear;
  editTable.Clear;
  editCol.Clear;
  editChange(Sender);
end;


function TfrmSelectDBObject.GetSelectedObject: TStringList;
begin
  Result := nil;
  if editDb.Text <> '' then begin
    Result := TStringList.Create;
    Result.Add(editDb.Text);
    if editTable.Text <> '' then Result.Add(editTable.Text);
    if editCol.Text <> '' then Result.Add(editCol.Text);
  end;
  // Let the result be nil to indicate we have no selected node
end;

procedure TfrmSelectDBObject.TreeDBOFocusChanged(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex);
var
  s: TStringList;
begin
  editDB.Clear;
  editTable.Clear;
  editCol.Clear;
  if Assigned(TreeDBO.FocusedNode) then begin
    s := TStringList.Create;
    s.Delimiter := '.';
    s.DelimitedText := TreeDBO.Path(TreeDBO.FocusedNode, -1, ttStatic, '.');
    // Tree.Path is buggy, has mostly one superflous empty item at the end. Cut that.
    while s.Count > 3 do
      s.Delete(s.Count-1);
    if s.Count >= 1 then editDB.Text := s[0];
    if s.Count >= 2 then editTable.Text := s[1];
    if s.Count >= 3 then editCol.Text := s[2];
    s.Free;
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
  ds: TDataset;
begin
  case Sender.GetNodeLevel(Node) of
    0: ImageIndex := ICONINDEX_DB;
    1: begin
        ds := CWin.FetchDbTableList(FDatabases[Node.Parent.Index]);
        ds.RecNo := Node.Index+1;
        case GetDBObjectType(ds.Fields) of
          NODETYPE_BASETABLE: ImageIndex := ICONINDEX_TABLE;
          NODETYPE_VIEW: ImageIndex := ICONINDEX_VIEW;
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
  ds: TDataset;
  cols: TStringList;
begin
  // Fetch sub nodes
  case Sender.GetNodeLevel(Node) of
    0: begin // DB expanding
        ds := CWin.FetchDbTableList(FDatabases[Node.Index]);
        ChildCount := ds.RecordCount;
        SetLength(FColumns[Node.Index], ds.RecordCount);
      end;
    1: begin // Table expanding
        ds := CWin.FetchDbTableList(FDatabases[Node.Parent.Index]);
        ds.RecNo := Node.Index+1;
        cols := CWin.GetCol('SHOW COLUMNS FROM '
          + Mainform.mask(FDatabases[Node.Parent.Index])+'.'
          + Mainform.Mask(ds.Fields[0].AsString));
        FColumns[Node.Parent.Index][Node.Index] := cols;
        ChildCount := cols.Count;
      end;
  end;

end;


procedure TfrmSelectDBObject.TreeDBOGetText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText:
    WideString);
var
  ds: TDataset;
begin
  case Sender.GetNodeLevel(Node) of
    0: CellText := FDatabases[Node.Index];
    1: begin
        ds := CWin.FetchDbTableList(FDatabases[Node.Parent.Index]);
        ds.RecNo := Node.Index+1;
        CellText := ds.Fields[0].AsString;
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
