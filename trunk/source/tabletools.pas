unit tabletools;


// -------------------------------------
// Table-diagnostics
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls, Buttons,
  WideStrings, WideStrUtils, VirtualTrees, ExtCtrls, Db, Contnrs, Graphics;

type
  TfrmTableTools = class(TForm)
    btnClose: TButton;
    pnlTop: TPanel;
    TreeObjects: TVirtualStringTree;
    spltHorizontally: TSplitter;
    pnlRight: TPanel;
    ResultGrid: TVirtualStringTree;
    lblResults: TLabel;
    PageControlTools: TPageControl;
    Maintenance: TTabSheet;
    comboOperation: TComboBox;
    lblOperation: TLabel;
    chkQuick: TCheckBox;
    chkFast: TCheckBox;
    chkMedium: TCheckBox;
    chkExtended: TCheckBox;
    chkChanged: TCheckBox;
    btnExecuteMaintenance: TButton;
    chkUseFrm: TCheckBox;
    lblOptions: TLabel;
    btnHelp: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure TreeObjectsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure TreeObjectsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeObjectsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeObjectsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure comboOperationChange(Sender: TObject);
    procedure ExecuteOperation(Sender: TObject);
    procedure ResultGridInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure ResultGridGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure ResultGridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure TreeObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure MaintenanceOptionClick(Sender: TObject);
    procedure ResultGridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure ResultGridCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure ResultGridPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
  private
    { Private declarations }
    FResults: TObjectList;
    procedure ValidateControls(Sender: TObject);
    procedure ProcessTableNode(Sender: TObject; Node: PVirtualNode);
    procedure AddResults(SQL: WideString);
  public
    { Public declarations }
  end;


implementation

uses main, helpers;

{$R *.DFM}


procedure TfrmTableTools.FormCreate(Sender: TObject);
begin
  // Restore GUI setup
  Width := GetRegValue(REGNAME_TOOLSWINWIDTH, Width);
  Height := GetRegValue(REGNAME_TOOLSWINHEIGHT, Height);
  TreeObjects.Width := GetRegValue(REGNAME_TOOLSTREEWIDTH, TreeObjects.Width);
  SetWindowSizeGrip( Self.Handle, True );
  InheritFont(Font);
  FixVT(TreeObjects);
  FixVT(ResultGrid);
  TreeObjects.RootNodeCount := Mainform.DBtree.RootNodeCount;
  FResults := TObjectList.Create;
end;


procedure TfrmTableTools.FormDestroy(Sender: TObject);
begin
  // Save GUI setup
  OpenRegistry;
  MainReg.WriteInteger( REGNAME_TOOLSWINWIDTH, Width );
  MainReg.WriteInteger( REGNAME_TOOLSWINHEIGHT, Height );
  MainReg.WriteInteger( REGNAME_TOOLSTREEWIDTH, TreeObjects.Width);
end;


procedure TfrmTableTools.FormShow(Sender: TObject);
begin
  // When this form is displayed the second time, databases may be deleted or filtered
  treeObjects.ReinitChildren(treeObjects.GetFirst, False);
  // CHECKSUM available since MySQL 4.1.1
  if Mainform.mysql_version < 40101 then
    comboOperation.Items[comboOperation.Items.IndexOf('Checksum')] := 'Checksum ('+STR_NOTSUPPORTED+')';
  comboOperation.OnChange(Sender);
end;


procedure TfrmTableTools.comboOperationChange(Sender: TObject);
var
  op: String;
begin
  // Only enable available options
  op := LowerCase(comboOperation.Text);
  chkQuick.Enabled := (op = 'check') or (op = 'checksum') or (op = 'repair');
  chkFast.Enabled := op = 'check';
  chkMedium.Enabled := op = 'check';
  chkExtended.Enabled := (op = 'check') or (op = 'checksum') or (op = 'repair');
  chkChanged.Enabled := op = 'check';
  chkUseFrm.Enabled := op = 'repair';
  ValidateControls(Sender);
end;


procedure TfrmTableTools.ValidateControls(Sender: TObject);
begin
  btnExecuteMaintenance.Enabled := (Pos(STR_NOTSUPPORTED, comboOperation.Text) = 0) and
    (TreeObjects.CheckedCount > 0);
  // CHECKSUM's options are mutually exclusive
  if comboOperation.Text = 'Checksum' then begin
    if (Sender = chkExtended) and chkExtended.Checked then chkQuick.Checked := False
    else if chkQuick.Checked then chkExtended.Checked := False;
  end;
end;


procedure TfrmTableTools.TreeObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  ValidateControls(Sender);
end;


procedure TfrmTableTools.TreeObjectsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  Mainform.DBtreeGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex);
end;


procedure TfrmTableTools.TreeObjectsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
begin
  Mainform.DBtreeGetText(Sender, Node, Column, TextType, CellText);
end;


procedure TfrmTableTools.TreeObjectsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
begin
  Mainform.DBtreeInitChildren(Sender, Node, ChildCount);
end;


procedure TfrmTableTools.TreeObjectsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  ds: TDataset;
begin
  // Attach a checkbox to all nodes
  Mainform.DBtreeInitNode(Sender, ParentNode, Node, InitialStates);
  Node.CheckType := ctTriStateCheckBox;
  Node.CheckState := csUncheckedNormal;
  case Sender.GetNodeLevel(Node) of
    1: begin
      // Preselect active database
      if Mainform.Databases[Node.Index] = Mainform.ActiveDatabase then begin
        Node.CheckState := csCheckedNormal;
        TreeObjects.ReinitChildren(Node, False);
      end;
      ValidateControls(Sender);
    end;
    2: begin
      // Disable stored routines
      ds := Mainform.FetchDbTableList(Mainform.Databases[ParentNode.Index]);
      ds.RecNo := Node.Index+1;
      if not (GetDBObjectType(ds.Fields) in [lntTable, lntCrashedTable, lntView]) then
        Node.CheckType := ctNone
      else if Node.Parent.CheckState in [csCheckedNormal, csCheckedPressed] then
        Node.CheckState := csCheckedNormal;
    end;
  end;
end;


procedure TfrmTableTools.btnHelpClick(Sender: TObject);
begin
  Mainform.CallSQLHelpWithKeyword(UpperCase(comboOperation.Text) + ' TABLE');
end;


procedure TfrmTableTools.MaintenanceOptionClick(Sender: TObject);
begin
  ValidateControls(Sender);
end;

procedure TfrmTableTools.ExecuteOperation(Sender: TObject);
var
  DBNode, TableNode: PVirtualNode;
begin
  Screen.Cursor := crHourGlass;
  ResultGrid.Clear;
  FResults.Clear;
  TreeObjects.SetFocus;
  DBNode := TreeObjects.GetFirstChild(TreeObjects.GetFirst);
  while Assigned(DBNode) do begin
    if not (DBNode.CheckState in [csUncheckedNormal, csUncheckedPressed]) then begin
      TableNode := TreeObjects.GetFirstChild(DBNode);
      while Assigned(TableNode) do begin
        ProcessTableNode(Sender, TableNode);
        TableNode := TreeObjects.GetNextSibling(TableNode);
      end;
    end;
    DBNode := TreeObjects.GetNextSibling(DBNode);
  end;
  Screen.Cursor := crDefault;
end;


procedure TfrmTableTools.ProcessTableNode(Sender: TObject; Node: PVirtualNode);
var
  SQL: WideString;
begin
  // Prepare SQL for one table node
  if (csCheckedNormal in [Node.CheckState, Node.Parent.CheckState]) and
    (Node.CheckType <> ctNone) then begin
    SQL := UpperCase(comboOperation.Text) + ' TABLE ' +
      Mainform.mask(TreeObjects.Text[Node.Parent, 0])+'.'+Mainform.mask(TreeObjects.Text[Node, 0]);
    if chkQuick.Enabled and chkQuick.Checked then SQL := SQL + ' QUICK';
    if chkFast.Enabled and chkFast.Checked then SQL := SQL + ' FAST';
    if chkMedium.Enabled and chkMedium.Checked then SQL := SQL + ' MEDIUM';
    if chkExtended.Enabled and chkExtended.Checked then SQL := SQL + ' EXTENDED';
    if chkChanged.Enabled and chkChanged.Checked then SQL := SQL + ' CHANGED';
    if chkUseFrm.Enabled and chkUseFrm.Checked then SQL := SQL + ' USE_FRM';
    AddResults(SQL);
  end;
end;


procedure TfrmTableTools.AddResults(SQL: WideString);
var
  i: Integer;
  Col: TVirtualTreeColumn;
  Row: TWideStringlist;
  ds: TDataset;
begin
  // Execute query and append results into grid
  ds := Mainform.GetResults(SQL);
  if ds = nil then
    Exit;

  // Add missing columns
  for i:=ResultGrid.Header.Columns.Count to ds.FieldCount-1 do begin
    Col := ResultGrid.Header.Columns.Add;
    Col.Width := 100;
  end;
  // Remove superfluous columns
  for i:=ResultGrid.Header.Columns.Count-1 downto ds.FieldCount do
    ResultGrid.Header.Columns[i].Free;
  // Set column header names
  for i:=0 to ds.FieldCount-1 do begin
    Col := ResultGrid.Header.Columns[i];
    Col.Text := ds.Fields[i].FieldName;
    if ds.Fields[i].DataType in [ftSmallint, ftInteger, ftWord, ftLargeint, ftFloat] then
      Col.Alignment := taRightJustify;
  end;
  ds.First;
  while not ds.Eof do begin
    Row := TWideStringlist.Create;
    for i:=0 to ds.FieldCount-1 do begin
      Row.Add(ds.Fields[i].AsString);
    end;
    FResults.Add(Row);
    ds.Next;
  end;

  ResultGrid.RootNodeCount := FResults.Count;
  ResultGrid.FocusedNode := ResultGrid.GetLast;
  ResultGrid.Selected[ResultGrid.FocusedNode] := True;
  ResultGrid.Repaint;
end;


procedure TfrmTableTools.ResultGridCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
begin
  Mainform.vstCompareNodes(Sender, Node1, Node2, Column, Result);
end;

procedure TfrmTableTools.ResultGridGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TWideStringList);
end;


procedure TfrmTableTools.ResultGridInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: ^TWideStringList;
begin
  // Bind string list to node
  Data := Sender.GetNodeData(Node);
  Data^ := FResults[Node.Index] as TWideStringList;
end;


procedure TfrmTableTools.ResultGridPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  VT: TVirtualStringTree;
  Msg: WideString;
begin
  // Red text color for errors, purple for nodes
  VT := Sender as TVirtualStringTree;
  if VT.Header.Columns.Count >= 3 then begin
    Msg := LowerCase(VT.Text[Node, 2]);
    if Msg = 'note' then
      TargetCanvas.Font.Color := clPurple
    else if Msg = 'error' then
      TargetCanvas.Font.Color := clRed;
  end;
end;

procedure TfrmTableTools.ResultGridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
var
  Data: ^TWideStringList;
begin
  if Column > NoColumn then begin
    Data := Sender.GetNodeData(Node);
    CellText := Data^[Column];
  end;
end;


procedure TfrmTableTools.ResultGridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  // Header column clicked to sort
  Mainform.vstHeaderClick(Sender, HitInfo);
end;

end.
