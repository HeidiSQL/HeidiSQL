unit copytable;


interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  mysql_connection, VirtualTrees, SynEdit, SynMemo;

type
  TCopyTableForm = class(TForm)
    editNewTablename: TEdit;
    lblNewTablename: TLabel;
    btnCancel: TButton;
    comboDatabase: TComboBox;
    btnOK: TButton;
    TreeElements: TVirtualStringTree;
    MemoWhereClause: TSynMemo;
    lblItems: TLabel;
    lblWhere: TLabel;
    procedure editNewTablenameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeElementsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeElementsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeElementsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeElementsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure FormDestroy(Sender: TObject);
    procedure TreeElementsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FOrgTableName: String;
    FColumns: TTableColumnList;
    FKeys: TTableKeyList;
    FForeignKeys: TForeignKeyList;
  public
    { Public declarations }
  end;


implementation

uses helpers, main;

const
  nColumns = 0;
  nKeys = 1;
  nForeignKeys = 2;
  nData = 3;

{$R *.DFM}
{$I const.inc}



procedure TCopyTableForm.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
  Width := GetRegValue(REGNAME_COPYTABLE_WINWIDTH, Width);
  Height := GetRegValue(REGNAME_COPYTABLE_WINHEIGHT, Height);
  SetWindowSizeGrip(Handle, True);
  MainForm.SetupSynEditors;
  FixVT(TreeElements);
  FColumns := TTableColumnList.Create;
  FKeys := TTableKeyList.Create;
  FForeignKeys := TForeignKeyList.Create;
end;


procedure TCopyTableForm.FormDestroy(Sender: TObject);
begin
  // Save GUI stuff
  MainReg.WriteInteger(REGNAME_COPYTABLE_WINWIDTH, Width);
  MainReg.WriteInteger(REGNAME_COPYTABLE_WINHEIGHT, Height);
end;


procedure TCopyTableForm.FormShow(Sender: TObject);
var
  CreateCode: String;
begin
  if Mainform.DBtree.Focused then
    FOrgTableName := Mainform.SelectedTable.Name
  else
    FOrgTableName := Mainform.ListTables.Text[Mainform.ListTables.FocusedNode, 0];
  editNewTablename.Text := FOrgTableName + '_copy';
  editNewTablename.SetFocus;
  lblNewTablename.Caption := 'Copy ''' + FOrgTableName + ''' to new db.table:';
  editNewTablename.SetFocus;

	// Select TargetDatabase
  comboDatabase.Items.Clear;
  comboDatabase.Items.Assign(Mainform.AllDatabases);
  comboDatabase.ItemIndex := comboDatabase.Items.IndexOf(Mainform.ActiveDatabase);
  if comboDatabase.ItemIndex = -1 then
    comboDatabase.ItemIndex := 0;

  CreateCode := MainForm.Connection.GetVar('SHOW CREATE TABLE '+MainForm.mask(FOrgTableName), 1);
  FColumns.Clear;
  FKeys.Clear;
  FForeignKeys.Clear;
  ParseTableStructure(CreateCode, FColumns, FKeys, FForeignKeys);

  TreeElements.Clear;
  TreeElements.RootNodeCount := 4;
end;



procedure TCopyTableForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Node: PVirtualNode;
  Option: String;
begin
  // Save first level node check options
  Node := TreeElements.GetFirst;
  while Assigned(Node) do begin
    case Node.Index of
      nColumns:      Option := REGNAME_COPYTABLE_COLUMNS;
      nKeys:         Option := REGNAME_COPYTABLE_KEYS;
      nForeignKeys:  Option := REGNAME_COPYTABLE_FOREIGN;
      nData:         Option := REGNAME_COPYTABLE_DATA;
      else raise Exception.Create(SUnhandledNodeIndex);
    end;
    MainReg.WriteBool(Option, Node.CheckState in [csCheckedNormal, csCheckedPressed]);
    Node := TreeElements.GetNextSibling(Node);
  end;
end;


procedure TCopyTableForm.TreeElementsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  // Disable WHERE memo if "Data" was unselected
  if (Node.Index = nData) then begin
    MemoWhereClause.Enabled := Node.CheckState = csCheckedNormal;
    if MemoWhereClause.Enabled then begin
      MemoWhereClause.Highlighter := MainForm.SynSQLSyn1;
      MemoWhereClause.Color := clWindow;
    end else begin
      MemoWhereClause.Highlighter := nil;
      MemoWhereClause.Color := clBtnFace;
    end;
  end;
end;


procedure TCopyTableForm.TreeElementsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  // Get node index
  if not (Kind in [ikNormal, ikSelected]) then Exit;
  case Sender.GetNodeLevel(Node) of
    0: case Node.Index of
         nColumns:      ImageIndex := ICONINDEX_FIELD;
         nKeys:         ImageIndex := 13;
         nForeignKeys:  ImageIndex := ICONINDEX_FOREIGNKEY;
         nData:         ImageIndex := 41;
         else raise Exception.Create(SUnhandledNodeIndex);
      end;
    1: case Node.Parent.Index of
         nColumns:      ImageIndex := ICONINDEX_FIELD;
         nKeys:         ImageIndex := GetIndexIcon(FKeys[Node.Index].IndexType);
         nForeignKeys:  ImageIndex := ICONINDEX_FOREIGNKEY;
         else raise Exception.Create(SUnhandledNodeIndex);
      end;
  end;
end;


procedure TCopyTableForm.TreeElementsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  // Get node text
  case Sender.GetNodeLevel(Node) of
    0: case Node.Index of
         nColumns:      CellText := 'Columns';
         nKeys:         CellText := 'Indexes';
         nForeignKeys:  CellText := 'Foreign keys';
         nData:         CellText := 'Data';
         else raise Exception.Create(SUnhandledNodeIndex);
       end;
    1: case Node.Parent.Index of
         nColumns:      CellText := FColumns[Node.Index].Name;
         nKeys:         CellText := FKeys[Node.Index].Name;
         nForeignKeys:  CellText := FForeignKeys[Node.Index].KeyName;
         else raise Exception.Create(SUnhandledNodeIndex);
      end;
  end;
end;


procedure TCopyTableForm.TreeElementsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
begin
  // Set child node count
  case Sender.GetNodeLevel(Node) of
    0: case Node.Index of
        nColumns:      ChildCount := FColumns.Count;
        nKeys:         ChildCount := FKeys.Count;
        nForeignKeys:  ChildCount := FForeignKeys.Count;
        nData:         ChildCount := 0;
        else raise Exception.Create(SUnhandledNodeIndex);
      end;
    else ChildCount := 0;
  end;
end;


procedure TCopyTableForm.TreeElementsInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Option: String;
begin
  // First three upper nodes mostly have child nodes
  Node.CheckType := ctTriStateCheckBox;
  case Sender.GetNodeLevel(Node) of
    0: begin
      if Node.Index in [nColumns, nKeys, nForeignKeys] then
        Include(InitialStates, ivsHasChildren);
      case Node.Index of
        nColumns:      Option := REGNAME_COPYTABLE_COLUMNS;
        nKeys:         Option := REGNAME_COPYTABLE_KEYS;
        nForeignKeys:  Option := REGNAME_COPYTABLE_FOREIGN;
        nData:         Option := REGNAME_COPYTABLE_DATA;
        else raise Exception.Create(SUnhandledNodeIndex);
      end;
      if GetRegValue(Option, True) then
        Node.CheckState := csCheckedNormal;
      (Sender as TVirtualStringTree).OnChecked(Sender, Node);
    end;

    1: if Node.Parent.CheckState in [csCheckedNormal, csCheckedPressed, csMixedNormal, csMixedPressed] then
      Node.CheckState := csCheckedNormal;
  end;
end;


procedure TCopyTableForm.editNewTablenameChange(Sender: TObject);
begin
  // Disable OK button as long as table name is empty
  btnOK.Enabled := editNewTablename.Text <> '';
end;


procedure TCopyTableForm.btnOKClick(Sender: TObject);
var
  CreateCode, Clause, DataCols: String;
  ParentNode, Node: PVirtualNode;
  DBObjects: TDBObjectList;
  Obj: TDBObject;
  DoData: Boolean;
begin
  // Compose and run CREATE query
  Screen.Cursor := crHourglass;
  MainForm.ShowStatusMsg('Generating SQL code ...');
  DataCols := '';
  DoData := False;
  ParentNode := TreeElements.GetFirst;
  while Assigned(ParentNode) do begin
    Node := TreeElements.GetFirstChild(ParentNode);
    while Assigned(Node) do begin
      if Node.CheckState in [csCheckedNormal, csCheckedPressed] then begin
        case ParentNode.Index of
          nColumns: begin
            Clause := FColumns[Node.Index].SQLCode;
            DataCols := DataCols + MainForm.mask(FColumns[Node.Index].Name) + ', ';
          end;
          nKeys:         Clause := FKeys[Node.Index].SQLCode;
          nForeignkeys:  Clause := FForeignKeys[Node.Index].SQLCode;
          else raise Exception.Create(SUnhandledNodeIndex);
        end;
        CreateCode := CreateCode + #9 + Clause + ',' + CRLF;
      end;
      Node := TreeElements.GetNextSibling(Node);
    end;
    if (ParentNode.Index = nData) then
      DoData := ParentNode.CheckState in [csCheckedNormal, csCheckedPressed];
    ParentNode := TreeElements.GetNextSibling(ParentNode);
  end;
  Delete(CreateCode, Length(CreateCode)-2, 3);
  CreateCode := 'CREATE TABLE '+Mainform.mask(comboDatabase.Text)+'.'+Mainform.mask(editNewTablename.Text)+' ('+CRLF+CreateCode+CRLF+')'+CRLF;

  // Add collation and engine clauses
  DBObjects := Mainform.Connection.GetDBObjects(Mainform.ActiveDatabase);
  for Obj in DBObjects do begin
    if Obj.Name = FOrgTableName then begin
      if Obj.Collation <> '' then
        CreateCode := CreateCode + ' COLLATE ''' + Obj.Collation + '''';
      if Obj.Engine <> '' then begin
        if Mainform.Connection.ServerVersionInt < 40018 then
          CreateCode := CreateCode + ' TYPE=' + Obj.Engine
        else
          CreateCode := CreateCode + ' ENGINE=' + Obj.Engine;
      end;
      if Obj.RowFormat <> '' then
        CreateCode := CreateCode + ' ROW_FORMAT=' + Obj.RowFormat;
      if Obj.AutoInc > -1 then
        CreateCode := CreateCode + ' AUTO_INCREMENT=' + IntToStr(Obj.AutoInc);
      CreateCode := CreateCode + ' COMMENT=' + esc(Obj.Comment);
      break;
    end;
  end;

  // Append SELECT .. FROM OrgTable clause
  if DoData and (DataCols <> '') then begin
    DataCols := Trim(DataCols);
    Delete(DataCols, Length(DataCols), 1);
    CreateCode := CreateCode + ' SELECT ' + DataCols + ' FROM ' + MainForm.mask(FOrgTableName);
    if MemoWhereClause.GetTextLen > 0 then
      CreateCode := CreateCode + ' WHERE ' + MemoWhereClause.Text;
  end;

  // Run query and refresh list
  try
    MainForm.ShowStatusMsg('Creating table ...');
    MainForm.Connection.Query(CreateCode);
    MainForm.actRefresh.Execute;
  except
    on E:EDatabaseError do begin
      Screen.Cursor := crDefault;
      MessageDlg(E.Message, mtError, [mbOk], 0);
      ModalResult := mrNone;
    end;
  end;
  MainForm.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;

end.
