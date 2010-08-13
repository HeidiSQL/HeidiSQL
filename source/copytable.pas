unit copytable;


interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Contnrs,
  mysql_connection, VirtualTrees, SynEdit, SynMemo, Menus;

type
  TCopyTableForm = class(TForm)
    editNewTablename: TEdit;
    lblNewTablename: TLabel;
    btnCancel: TButton;
    comboDatabase: TComboBox;
    btnOK: TButton;
    TreeElements: TVirtualStringTree;
    MemoFilter: TSynMemo;
    lblItems: TLabel;
    lblWhere: TLabel;
    btnRecentFilters: TButton;
    popupRecentFilters: TPopupMenu;
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
    procedure btnRecentFiltersClick(Sender: TObject);
    procedure RecentFilterClick(Sender: TObject);
  private
    { Private declarations }
    FDBObj: TDBObject;
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
  CreateCode, Table, Filter: String;
  DBObjects: TDBObjectList;
  Obj: TDBObject;
  Values: TStringList;
  i, j: Integer;
  Item: TMenuItem;
begin
  if Mainform.DBtree.Focused then
    Table := Mainform.SelectedTable.Name
  else
    Table := Mainform.ListTables.Text[Mainform.ListTables.FocusedNode, 0];
  DBObjects := Mainform.Connection.GetDBObjects(Mainform.ActiveDatabase);
  FDBObj := nil;
  for Obj in DBObjects do begin
    if Obj.Name = Table then begin
      FDBObj := Obj;
      break;
    end;
  end;
  if FDBObj = nil then
    Exception.Create('Database object "'+Table+'" not found.');
  editNewTablename.Text := FDBObj.Name + '_copy';
  editNewTablename.SetFocus;
  lblNewTablename.Caption := 'Copy ''' + FDBObj.Name + ''' to new db.table:';
  editNewTablename.SetFocus;

	// Select TargetDatabase
  comboDatabase.Items.Clear;
  comboDatabase.Items.Assign(Mainform.AllDatabases);
  comboDatabase.ItemIndex := comboDatabase.Items.IndexOf(Mainform.ActiveDatabase);
  if comboDatabase.ItemIndex = -1 then
    comboDatabase.ItemIndex := 0;

  // Fetch columns and key structures from table or view
  FColumns.Clear;
  FKeys.Clear;
  FForeignKeys.Clear;
  case FDBObj.NodeType of
    lntTable: begin
      CreateCode := MainForm.Connection.GetVar('SHOW CREATE TABLE '+MainForm.mask(FDBObj.Name), 1);
      ParseTableStructure(CreateCode, FColumns, FKeys, FForeignKeys);
    end;
    lntView: ParseViewStructure(FDBObj.Name, FColumns);
    else raise Exception.Create('Neither table nor view: '+FDBObj.Name);
  end;

  // Reset options tree
  TreeElements.Clear;
  TreeElements.RootNodeCount := 4;

  // Load recent WHERE clauses from registry into dropdown menu
  popupRecentFilters.Items.Clear;
  OpenRegistry;
  Values := TStringList.Create;
  MainReg.GetValueNames(Values);
  j := 0;
  for i:=0 to Values.Count-1 do begin
    if Pos(REGPREFIX_COPYTABLE_FILTERS, Values[i]) <> 1 then
      continue;
    Inc(j);
    Filter := MainReg.ReadString(Values[i]);
    Item := TMenuItem.Create(popupRecentFilters);
    Item.Caption := IntToStr(j) + '  ' + sstr(Filter, 100);
    Item.Hint := Filter;
    Item.OnClick := RecentFilterClick;
    popupRecentFilters.Items.Add(Item);
  end;

end;



procedure TCopyTableForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Node: PVirtualNode;
  Option, Filter: String;
  Values, NewValues: TStringList;
  i: Integer;
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
    if not (vsDisabled in Node.States) then
      MainReg.WriteBool(Option, Node.CheckState in CheckedStates);
    Node := TreeElements.GetNextSibling(Node);
  end;
  // Store recent filters
  if MemoFilter.Enabled and (MemoFilter.GetTextLen > 0) then begin
    OpenRegistry;
    Values := TStringList.Create;
    NewValues := TStringList.Create;
    NewValues.Add(MemoFilter.Text);
    MainReg.GetValueNames(Values);
    for i:=0 to Values.Count-1 do begin
      if Pos(REGPREFIX_COPYTABLE_FILTERS, Values[i]) <> 1 then
        continue;
      Filter := MainReg.ReadString(Values[i]);
      if NewValues.IndexOf(Filter) = -1 then
        NewValues.Add(Filter);
      MainReg.DeleteValue(Values[i]);
    end;
    for i:=0 to NewValues.Count-1 do begin
      if i = 20 then
        break;
      MainReg.WriteString(REGPREFIX_COPYTABLE_FILTERS+IntToStr(i), NewValues[i]);
    end;
  end;
end;


procedure TCopyTableForm.TreeElementsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  // Disable WHERE memo if "Data" was unselected
  if (Sender.GetNodeLevel(Node) = 0) and (Node.Index = nData) then begin
    MemoFilter.Enabled := Node.CheckState = csCheckedNormal;
    btnRecentFilters.Enabled := MemoFilter.Enabled;
    if MemoFilter.Enabled then begin
      MemoFilter.Highlighter := MainForm.SynSQLSyn1;
      MemoFilter.Color := clWindow;
    end else begin
      MemoFilter.Highlighter := nil;
      MemoFilter.Color := clBtnFace;
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
var
  CheckedCount: Integer;
  Child: PVirtualNode;
begin
  // Get node text
  case Sender.GetNodeLevel(Node) of
    0: begin
       case Node.Index of
         nColumns:      CellText := 'Columns';
         nKeys:         CellText := 'Indexes';
         nForeignKeys:  CellText := 'Foreign keys';
         nData:         CellText := 'Data ('+FormatNumber(FDBObj.Rows)+' rows)';
         else raise Exception.Create(SUnhandledNodeIndex);
       end;
       if Node.Index <> nData then begin
         CheckedCount := 0;
         Child := Sender.GetFirstChild(Node);
         while Assigned(Child) do begin
           if Child.CheckState in CheckedStates then
             Inc(CheckedCount);
           Child := Sender.GetNextSibling(Child);
         end;
         CellText := CellText + ' ('+FormatNumber(CheckedCount)+'/'+FormatNumber(Sender.ChildCount[Node])+')';
       end;
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
  ChildCount: Integer;
begin
  // First three upper nodes mostly have child nodes
  Node.CheckType := ctTriStateCheckBox;
  case Sender.GetNodeLevel(Node) of
    0: begin
      case Node.Index of
        nColumns:      begin Option := REGNAME_COPYTABLE_COLUMNS; ChildCount := FColumns.Count; end;
        nKeys:         begin Option := REGNAME_COPYTABLE_KEYS; ChildCount := FKeys.Count; end;
        nForeignKeys:  begin Option := REGNAME_COPYTABLE_FOREIGN; ChildCount := FForeignKeys.Count; end;
        nData:         begin Option := REGNAME_COPYTABLE_DATA; ChildCount := -1; end;
        else raise Exception.Create(SUnhandledNodeIndex);
      end;
      if ChildCount > 0 then
        Include(InitialStates, ivsHasChildren);
      if (ChildCount = 0) or ((Node.Index = nData) and (FDBObj.Rows = 0)) then
        Node.States := Node.States + [vsDisabled]
      else if GetRegValue(Option, True) then
        Node.CheckState := csCheckedNormal;
      (Sender as TVirtualStringTree).OnChecked(Sender, Node);
    end;

    1: if Node.Parent.CheckState in CheckedStates then
      Node.CheckState := csCheckedNormal;
  end;
end;


procedure TCopyTableForm.btnRecentFiltersClick(Sender: TObject);
var
  btn: TButton;
begin
  // A split button does not drop its menu down when the normal button area is clicked. Do that by hand.
  btn := Sender as TButton;
  btn.DropDownMenu.Popup(btn.ClientOrigin.X, btn.ClientOrigin.Y+btn.Height);
end;


procedure TCopyTableForm.RecentFilterClick(Sender: TObject);
var
  Item: TMenuItem;
begin
  // Load recent filter
  Item := Sender as TMenuItem;
  MemoFilter.SelectAll;
  MemoFilter.SelText := Item.Hint;
end;


procedure TCopyTableForm.editNewTablenameChange(Sender: TObject);
begin
  // Disable OK button as long as table name is empty
  btnOK.Enabled := editNewTablename.Text <> '';
end;


procedure TCopyTableForm.btnOKClick(Sender: TObject);
var
  CreateCode, Clause, DataCols, TableExistance: String;
  ParentNode, Node: PVirtualNode;
  DoData: Boolean;
begin
  // Compose and run CREATE query
  TableExistance := MainForm.Connection.GetVar('SHOW TABLES FROM '+MainForm.mask(comboDatabase.Text)+' LIKE '+esc(editNewTablename.Text));
  if TableExistance <> '' then begin
    if MessageDlg('Target table exists. Drop it and overwrite?', mtConfirmation, [mbYes, mbCancel], 0) = mrCancel then begin
      ModalResult := mrNone;
      Exit;
    end;
    MainForm.Connection.Query('DROP TABLE '+MainForm.mask(comboDatabase.Text)+'.'+MainForm.mask(editNewTablename.Text));
  end;

  Screen.Cursor := crHourglass;
  MainForm.ShowStatusMsg('Generating SQL code ...');
  DataCols := '';
  DoData := False;
  ParentNode := TreeElements.GetFirst;
  while Assigned(ParentNode) do begin
    Node := TreeElements.GetFirstChild(ParentNode);
    while Assigned(Node) do begin
      if Node.CheckState in CheckedStates then begin
        case ParentNode.Index of
          nColumns: begin
            Clause := FColumns[Node.Index].SQLCode;
            DataCols := DataCols + MainForm.mask(FColumns[Node.Index].Name) + ', ';
          end;
          nKeys:         Clause := FKeys[Node.Index].SQLCode;
          nForeignkeys:  Clause := FForeignKeys[Node.Index].SQLCode(False);
          else raise Exception.Create(SUnhandledNodeIndex);
        end;
        CreateCode := CreateCode + #9 + Clause + ',' + CRLF;
      end;
      Node := TreeElements.GetNextSibling(Node);
    end;
    if (ParentNode.Index = nData) then
      DoData := ParentNode.CheckState in CheckedStates;
    ParentNode := TreeElements.GetNextSibling(ParentNode);
  end;
  Delete(CreateCode, Length(CreateCode)-2, 3);
  CreateCode := 'CREATE TABLE '+Mainform.mask(comboDatabase.Text)+'.'+Mainform.mask(editNewTablename.Text)+' ('+CRLF+CreateCode+CRLF+')'+CRLF;

  // Add collation and engine clauses
  if FDBObj.Collation <> '' then
    CreateCode := CreateCode + ' COLLATE ''' + FDBObj.Collation + '''';
  if FDBObj.Engine <> '' then begin
    if Mainform.Connection.ServerVersionInt < 40018 then
      CreateCode := CreateCode + ' TYPE=' + FDBObj.Engine
    else
      CreateCode := CreateCode + ' ENGINE=' + FDBObj.Engine;
  end;
  if FDBObj.RowFormat <> '' then
    CreateCode := CreateCode + ' ROW_FORMAT=' + FDBObj.RowFormat;
  if FDBObj.AutoInc > -1 then
    CreateCode := CreateCode + ' AUTO_INCREMENT=' + IntToStr(FDBObj.AutoInc);
  CreateCode := CreateCode + ' COMMENT=' + esc(FDBObj.Comment);

  // Append SELECT .. FROM OrgTable clause
  if DoData and (DataCols <> '') then begin
    DataCols := Trim(DataCols);
    Delete(DataCols, Length(DataCols), 1);
    CreateCode := CreateCode + ' SELECT ' + DataCols + ' FROM ' + MainForm.mask(FDBObj.Name);
    if MemoFilter.GetTextLen > 0 then
      CreateCode := CreateCode + ' WHERE ' + MemoFilter.Text;
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
