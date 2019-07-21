unit syncdb;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Generics.Collections, VirtualTrees, extra_controls,
  dbconnection, gnugettext;

type
  TfrmSyncDB = class(TExtForm)
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
    procedure treeSourceGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure treeSourceGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure treeSourceGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure treeSourceInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure treeSourceInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure treeSourcePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure comboTargetServerChange(Sender: TObject);
    procedure comboTargetDatabaseChange(Sender: TObject);
    procedure btnAnalyzeClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure treeDifferencesGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure treeDifferencesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure treeDifferencesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure treeDifferencesInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure treeDifferencesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
  private
    { Private declarations }
    function CreateTargetConnection: TDBConnection;
  public
    { Public declarations }
  end;

  TDiffType = (diCreate, diAlter, diInsert);
  TDiffItem = class(TObject)
    private
      FTitle: String;
      FSQL: String;
      FDiffType: TDiffType;
    public
      property Title: String read FTitle write FTitle;
      property SQL: String read FSQL write FSQL;
      property DiffType: TDiffType read FDiffType write FDiffType;
  end;
  TDiffObject = class(TObjectList<TDiffItem>)
    private
      FDBObject: TDBObject;
    public
      function AddItem(Title, SQL: String; DiffType: TDiffType): TDiffItem;
      property DBObject: TDBObject read FDBObject write FDBObject;
  end;
  PDiffObject = ^TDiffObject;

implementation

uses main, apphelpers;

{$R *.dfm}


{ TDiffObject }

function TDiffObject.AddItem(Title, SQL: String; DiffType: TDiffType): TDiffItem;
begin
  // Add ALTER, INSERT or whatever difference to diff object
  Result := TDiffItem.Create;
  Result.Title := Title;
  Result.SQL := SQL;
  Result.DiffType := DiffType;
  Add(Result);
end;


{ TfrmSyncDB }

procedure TfrmSyncDB.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure TfrmSyncDB.FormCreate(Sender: TObject);
var
  SessNode: PVirtualNode;
  SessionPaths: TStringList;
begin
  Caption := MainForm.actSynchronizeDatabase.Caption;
  HasSizeGrip := True;
  FixVT(treeSource);
  FixVT(treeDifferences);

  // Init session nodes and axpand active connection node
  treeSource.RootNodeCount := Mainform.DBtree.RootNodeCount;
  SessNode := MainForm.GetRootNode(treeSource, MainForm.ActiveConnection);
  if Assigned(SessNode) then
    treeSource.ToggleNode(SessNode);

  SessionPaths := TStringList.Create;
  AppSettings.GetSessionPaths('', SessionPaths);
  comboTargetServer.Items.Assign(SessionPaths);
  SessionPaths.Free;
  comboTargetServer.Items.Insert(0, _('Select server session ...'));
  comboTargetServer.ItemIndex := 0;
  comboTargetServer.OnChange(Sender);
end;


procedure TfrmSyncDB.comboTargetServerChange(Sender: TObject);
var
  Connection: TDBConnection;
  SessionSelected: Boolean;
begin
  // Populate database drop down, or disable remaining controls if no session selected
  comboTargetDatabase.Clear;
  SessionSelected := comboTargetServer.ItemIndex > 0;
  lblTargetDatabase.Enabled := SessionSelected;
  comboTargetDatabase.Enabled := SessionSelected;
  lblTargetTable.Enabled := SessionSelected;
  comboTargetTable.Enabled := SessionSelected;
  radioOptionsStructure.Enabled := SessionSelected;
  radioOptionsData.Enabled := SessionSelected;
  grpOptions.Enabled := SessionSelected;
  btnAnalyze.Enabled := SessionSelected;
  if lblTargetDatabase.Enabled then begin
    Connection := CreateTargetConnection;
    comboTargetDatabase.Items.Assign(Connection.AllDatabases);
    Connection.Active := False;
    Connection.Free;
    comboTargetDatabase.Items.Insert(0, '['+_('Same as source')+']');
    comboTargetDatabase.ItemIndex := 0;
    comboTargetDatabase.OnChange(Sender);
  end;
end;


procedure TfrmSyncDB.comboTargetDatabaseChange(Sender: TObject);
var
  Connection: TDBConnection;
  Objects: TDBObjectList;
  Obj: TDBObject;
  DBSelected: Boolean;
begin
  // Populate table drop down
  comboTargetTable.Clear;
  DBSelected := comboTargetDatabase.ItemIndex > 0;
  lblTargetTable.Enabled := DBSelected;
  comboTargetTable.Enabled := DBSelected;
  if DBSelected then begin
    Connection := CreateTargetConnection;
    Objects := Connection.GetDBObjects(comboTargetDatabase.Text);
    for Obj in Objects do begin
      if Obj.NodeType = lntTable then
        comboTargetTable.Items.Add(Obj.Name);
    end;
    Connection.Active := False;
    Connection.Free;
  end;
  comboTargetTable.Items.Insert(0, '['+_('Same as source')+']');
  comboTargetTable.ItemIndex := 0;
end;


procedure TfrmSyncDB.treeDifferencesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  Diff: PDiffObject;
begin
  if Kind in [ikNormal, ikSelected] then begin
    case Sender.GetNodeLevel(Node) of
      0: begin
        Diff := Sender.GetNodeData(Node);
        ImageIndex := Diff.DBObject.ImageIndex;
        Ghosted := Diff.Count = 0;
      end;
      1: begin
        Diff := Sender.GetNodeData(Node.Parent);
        case Diff^[Node.Index].DiffType of
          diCreate: ImageIndex := 45;
          diAlter: ImageIndex := 42;
          diInsert: ImageIndex := 41;
        end;
      end;
    end;
  end;
end;


procedure TfrmSyncDB.treeDifferencesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(PDiffObject);
end;


procedure TfrmSyncDB.treeDifferencesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Diff: PDiffObject;
begin
  case Sender.GetNodeLevel(Node) of
    0: begin
      Diff := Sender.GetNodeData(Node);
      CellText := Diff.DBObject.Name;
    end;
    1: begin
      Diff := Sender.GetNodeData(Node.Parent);
      CellText := Diff^[Node.Index].Title;
    end;
  end;
end;


procedure TfrmSyncDB.treeDifferencesInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
var
  Diff: PDiffObject;
begin
  case Sender.GetNodeLevel(Node) of
    0: begin
      Diff := Sender.GetNodeData(Node);
      ChildCount := Diff.Count;
    end;
    1: ChildCount := 0;
  end;
end;


procedure TfrmSyncDB.treeDifferencesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Diff: PDiffObject;
begin
  if Sender.GetNodeLevel(Node) = 0 then begin
    Diff := Sender.GetNodeData(Node);
    if Diff.Count > 0 then
      Include(InitialStates, ivsHasChildren);
  end;
  Node.CheckType := ctTriStateCheckBox;
  Node.CheckState := csUncheckedNormal;
end;


procedure TfrmSyncDB.treeSourceChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  // Clone logic of MainForm.DBtree
  MainForm.DBtree.OnChange(Sender, Node);
end;


procedure TfrmSyncDB.treeSourceChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Sess, DB: PVirtualNode;
begin
  // Uncheck table nodes other than current checked one
  if (Sender.GetNodeLevel(Node) = 1) and (Node.CheckState in CheckedStates) then begin
    Sess := Sender.GetFirstChild(nil);
    while Assigned(Sess) do begin
      DB := Sender.GetFirstChild(Sess);
      while Assigned(DB) do begin
        if DB <> Node then
          Sender.CheckState[DB] := csUncheckedNormal;
        DB := Sender.GetNextSibling(DB);
      end;
      Sess := Sender.GetNextSibling(Sess);
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
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
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


function TfrmSyncDB.CreateTargetConnection: TDBConnection;
var
  Parameters: TConnectionParameters;
begin
  // Create target connection
  Parameters := TConnectionParameters.Create(comboTargetServer.Text);
  Result := Parameters.CreateConnection(Self);
  Result.OnLog := MainForm.LogSQL;
  Result.LogPrefix := comboTargetServer.Text;
  Result.Active := True;
end;


procedure TfrmSyncDB.btnAnalyzeClick(Sender: TObject);
var
  Sess, DB, Table: PVirtualNode;
  Connection: TDBConnection;
  Objects, TargetObjects: TDBObjectList;
  SourceObj, TargetObj: TDBObject;
  DBObj: PDBObject;
  ObjExists: Boolean;
  TargetDB: String;
  Diff: TDiffObject;
begin
  // Analyze differences, display these in diff tree
  Objects := TDBObjectList.Create(False);
  Sess := treeSource.GetFirstChild(nil);
  while Assigned(Sess) do begin
    DB := treeSource.GetFirstChild(Sess);
    while Assigned(DB) do begin
      if treeSource.ChildrenInitialized[DB] then begin
        Table := treeSource.GetFirstChild(DB);
        while Assigned(Table) do begin
          if treeSource.CheckState[Table] in CheckedStates then begin
            DBObj := treeSource.GetNodeData(Table);
            Objects.Add(DBObj^);
          end;
          Table := treeSource.GetNextSibling(Table);
        end;
      end;
      DB := treeSource.GetNextSibling(DB);
    end;
    Sess := treeSource.GetNextSibling(Sess);
  end;

  Connection := CreateTargetConnection;
  if comboTargetDatabase.ItemIndex = 0 then
    TargetDB := Objects[0].Database
  else
    TargetDB := comboTargetDatabase.Text;

  // Check for existance in target db
  TargetObjects := Connection.GetDBObjects(TargetDB);
  for SourceObj in Objects do begin
    Diff := TDiffObject.Create(True);
    Diff.DBObject := SourceObj;
    treeDifferences.BeginUpdate;
    treeDifferences.AddChild(nil, PDiffObject(Diff));
    ObjExists := False;
    for TargetObj in TargetObjects do begin
      if (TargetObj.Name = SourceObj.Name) and (TargetObj.NodeType = SourceObj.NodeType) then begin
        ObjExists := True;
        break;
      end;
    end;
    if not ObjExists then begin
      Diff.AddItem(f_('Create missing %s', [_(LowerCase(SourceObj.ObjType))]), SourceObj.CreateCode, diCreate);
    end;
    treeDifferences.EndUpdate;
  end;


  // Throw target connection away
  Connection.Active := False;
  Connection.Free;
end;


procedure TfrmSyncDB.btnApplyClick(Sender: TObject);
begin
  // Apply selected differences on target database
end;


end.
