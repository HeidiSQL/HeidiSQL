unit copytable;


interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, extra_controls,
  dbconnection, dbstructures, VirtualTrees, SynEdit, SynMemo, Menus, gnugettext;

type
  TCopyTableForm = class(TExtForm)
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
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure TreeElementsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure FormDestroy(Sender: TObject);
    procedure TreeElementsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnRecentFiltersClick(Sender: TObject);
    procedure RecentFilterClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FDBObj: TDBObject;
    FConnection: TDBConnection;
    FColumns: TTableColumnList;
    FKeys: TTableKeyList;
    FForeignKeys: TForeignKeyList;
  public
    { Public declarations }
  end;


implementation

uses apphelpers, main;

const
  nColumns = 0;
  nKeys = 1;
  nForeignKeys = 2;
  nData = 3;

{$R *.DFM}
{$I const.inc}



procedure TCopyTableForm.FormCreate(Sender: TObject);
begin
  HasSizeGrip := True;
  Width := AppSettings.ReadInt(asCopyTableWindowWidth);
  Height := AppSettings.ReadInt(asCopyTableWindowHeight);
  MainForm.SetupSynEditors;
  FixVT(TreeElements);
  FColumns := TTableColumnList.Create;
  FKeys := TTableKeyList.Create;
  FForeignKeys := TForeignKeyList.Create;
end;


procedure TCopyTableForm.FormDestroy(Sender: TObject);
begin
  // Save GUI stuff
  AppSettings.WriteInt(asCopyTableWindowWidth, Width);
  AppSettings.WriteInt(asCopyTableWindowHeight, Height);
end;


procedure TCopyTableForm.FormResize(Sender: TObject);
var
  HalfWidth: Integer;
const
  Space: Integer=8;
begin
  // Give both dropdown and edit box the same width. See http://www.heidisql.com/forum.php?t=11039
  HalfWidth := (ClientWidth - (3*Space)) div 2;
  comboDatabase.Width := HalfWidth;
  editNewTablename.Width := HalfWidth;
  editNewTablename.Left := comboDatabase.Left + comboDatabase.Width + Space;
end;


procedure TCopyTableForm.FormShow(Sender: TObject);
var
  Filter: String;
  Dummy: String;
  Obj: PDBObject;
  i: Integer;
  Item: TMenuItem;
  Tree: TVirtualStringTree;
begin
  if Mainform.DBtree.Focused then
    Tree := Mainform.DBtree
  else
    Tree := Mainform.ListTables;
  Obj := Tree.GetNodeData(Tree.FocusedNode);
  FDBObj := Obj^;
  FConnection := FDBObj.Connection;
  editNewTablename.Text := FDBObj.Name + '_copy';
  editNewTablename.SetFocus;
  lblNewTablename.Caption := f_('Copy "%s" to new db.table:', [FDBObj.Name]);
  editNewTablename.SetFocus;

	// Select TargetDatabase
  comboDatabase.Items.Clear;
  comboDatabase.Items.Assign(FConnection.AllDatabases);
  comboDatabase.ItemIndex := comboDatabase.Items.IndexOf(Mainform.ActiveDatabase);
  if comboDatabase.ItemIndex = -1 then
    comboDatabase.ItemIndex := 0;

  // Fetch columns and key structures from table or view
  FColumns.Clear;
  FKeys.Clear;
  FForeignKeys.Clear;
  case FDBObj.NodeType of
    lntTable: FConnection.ParseTableStructure(FDBObj.CreateCode, FColumns, FKeys, FForeignKeys);
    lntView: FConnection.ParseViewStructure(FDBObj.CreateCode, FDBObj, FColumns, Dummy, Dummy, Dummy, Dummy, Dummy);
    else raise Exception.CreateFmt(_('Neither table nor view: %s'), [FDBObj.Name]);
  end;

  // Reset options tree
  TreeElements.Clear;
  TreeElements.RootNodeCount := 4;

  // Load recent WHERE clauses from registry into dropdown menu
  popupRecentFilters.Items.Clear;
  for i:=1 to 20 do begin
    Filter := AppSettings.ReadString(asCopyTableRecentFilter, IntToStr(i));
    if IsEmpty(Filter) then
      Continue;
    Item := TMenuItem.Create(popupRecentFilters);
    Item.Caption := IntToStr(i) + '  ' + sstr(Filter, 100);
    Item.Hint := Filter;
    Item.OnClick := RecentFilterClick;
    popupRecentFilters.Items.Add(Item);
  end;

end;



procedure TCopyTableForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Node: PVirtualNode;
  Option: TAppSettingIndex;
  Filter: String;
  NewValues: TStringList;
  i: Integer;
begin
  // Save first level node check options
  Node := TreeElements.GetFirst;
  while Assigned(Node) do begin
    case Node.Index of
      nColumns:      Option := asCopyTableColumns;
      nKeys:         Option := asCopyTableKeys;
      nForeignKeys:  Option := asCopyTableForeignKeys;
      nData:         Option := asCopyTableData;
      else raise Exception.Create(_(SUnhandledNodeIndex));
    end;
    if not (vsDisabled in Node.States) then
      AppSettings.WriteBool(Option, Node.CheckState in CheckedStates);
    Node := TreeElements.GetNextSibling(Node);
  end;
  // Store recent filters
  if MemoFilter.Enabled and (MemoFilter.GetTextLen > 0) then begin
    NewValues := TStringList.Create;
    NewValues.Add(MemoFilter.Text);
    for i:=1 to 20 do begin
      Filter := AppSettings.ReadString(asCopyTableRecentFilter, IntToStr(i));
      if IsEmpty(Filter) then
        Continue;
      if NewValues.IndexOf(Filter) = -1 then
        NewValues.Add(Filter);
      AppSettings.DeleteValue(asCopyTableRecentFilter, IntToStr(i));
    end;
    for i:=0 to NewValues.Count-1 do begin
      AppSettings.WriteString(asCopyTableRecentFilter, NewValues[i], IntToStr(i));
    end;
  end;
  Action := caFree;
end;


procedure TCopyTableForm.TreeElementsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  // Disable WHERE memo if "Data" was unselected
  if (Sender.GetNodeLevel(Node) = 0) and (Node.Index = nData) then begin
    MemoFilter.Enabled := Node.CheckState = csCheckedNormal;
    btnRecentFilters.Enabled := MemoFilter.Enabled;
    if MemoFilter.Enabled then begin
      MemoFilter.Highlighter := MainForm.SynSQLSynUsed;
      MemoFilter.Color := GetThemeColor(clWindow);
    end else begin
      MemoFilter.Highlighter := nil;
      MemoFilter.Color := GetThemeColor(clBtnFace);
    end;
  end;
end;


procedure TCopyTableForm.TreeElementsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  // Get node index
  if not (Kind in [ikNormal, ikSelected]) then Exit;
  case Sender.GetNodeLevel(Node) of
    0: case Node.Index of
         nColumns:      ImageIndex := ICONINDEX_FIELD;
         nKeys:         ImageIndex := 13;
         nForeignKeys:  ImageIndex := ICONINDEX_FOREIGNKEY;
         nData:         ImageIndex := 41;
         else raise Exception.Create(_(SUnhandledNodeIndex));
      end;
    1: case Node.Parent.Index of
         nColumns:      ImageIndex := ICONINDEX_FIELD;
         nKeys:         ImageIndex := FKeys[Node.Index].ImageIndex;
         nForeignKeys:  ImageIndex := ICONINDEX_FOREIGNKEY;
         else raise Exception.Create(_(SUnhandledNodeIndex));
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
         nColumns:      CellText := _('Columns');
         nKeys:         CellText := _('Indexes');
         nForeignKeys:  CellText := _('Foreign keys');
         nData:         CellText := f_('Data (%s rows)', [FormatNumber(FDBObj.Rows)]);
         else raise Exception.Create(_(SUnhandledNodeIndex));
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
         else raise Exception.Create(_(SUnhandledNodeIndex));
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
        else raise Exception.Create(_(SUnhandledNodeIndex));
      end;
    else ChildCount := 0;
  end;
end;


procedure TCopyTableForm.TreeElementsInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Option: TAppSettingIndex;
  ChildCount: Integer;
begin
  // First three upper nodes mostly have child nodes
  Node.CheckType := ctTriStateCheckBox;
  case Sender.GetNodeLevel(Node) of
    0: begin
      case Node.Index of
        nColumns:      begin Option := asCopyTableColumns; ChildCount := FColumns.Count; end;
        nKeys:         begin Option := asCopyTableKeys; ChildCount := FKeys.Count; end;
        nForeignKeys:  begin Option := asCopyTableForeignKeys; ChildCount := FForeignKeys.Count; end;
        nData:         begin Option := asCopyTableData; ChildCount := -1; end;
        else raise Exception.Create(_(SUnhandledNodeIndex));
      end;
      if ChildCount > 0 then
        Include(InitialStates, ivsHasChildren);
      if (ChildCount = 0) or ((Node.Index = nData) and (FDBObj.Rows = 0)) then
        Node.States := Node.States + [vsDisabled]
      else if AppSettings.ReadBool(Option) then
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
  CreateCode, InsertCode, TargetTable, DataCols, Msg: String;
  TableExistence: String;
  ParentNode, Node: PVirtualNode;
  DoData, AutoIncGetsKey, AutoIncRemoved, TableHasAutoInc: Boolean;
  SelectedColumns: TTableColumnList;
  SelectedKeys: TTableKeyList;
  SelectedForeignKeys: TForeignKeyList;
  Column: TTableColumn;
  Key: TTableKey;
  ForeignKey: TForeignKey;
const
  ClausePattern: String = #9 + '%s,' + CRLF;
begin
  // Compose and run CREATE query

  TargetTable := FConnection.QuotedDbAndTableName(comboDatabase.Text, editNewTablename.Text);

  // Watch out if target table exists
  TableExistence := FConnection.GetVar('SELECT '+FConnection.QuoteIdent('table_name')+
    ' FROM '+FConnection.QuoteIdent('information_schema')+'.'+FConnection.QuoteIdent('tables')+
    ' WHERE '+FConnection.QuoteIdent(FConnection.GetSQLSpecifity(spISTableSchemaCol))+'='+FConnection.EscapeString(comboDatabase.Text)+
    ' AND '+FConnection.QuoteIdent('table_name')+'='+FConnection.EscapeString(editNewTablename.Text)
    );
  if TableExistence <> '' then begin
    if MessageDialog(_('Target table exists. Drop it and overwrite?'), mtConfirmation, [mbYes, mbCancel]) = mrCancel then begin
      ModalResult := mrNone;
      Exit;
    end;
    FConnection.Query('DROP TABLE '+TargetTable);
  end;

  Screen.Cursor := crHourglass;
  MainForm.ShowStatusMsg(_('Generating SQL code ...'));
  DataCols := '';
  SelectedColumns := TTableColumnList.Create(False);
  SelectedKeys := TTableKeyList.Create(False);
  SelectedForeignKeys := TForeignKeyList.Create(False);
  DoData := False;
  ParentNode := TreeElements.GetFirst;
  while Assigned(ParentNode) do begin
    Node := TreeElements.GetFirstChild(ParentNode);
    while Assigned(Node) do begin
      if Node.CheckState in CheckedStates then begin
        case ParentNode.Index of
          nColumns: SelectedColumns.Add(FColumns[Node.Index]);
          nKeys: SelectedKeys.Add(FKeys[Node.Index]);
          nForeignkeys: SelectedForeignKeys.Add(FForeignKeys[Node.Index]);
          else raise Exception.Create(_(SUnhandledNodeIndex));
        end;
      end;
      Node := TreeElements.GetNextSibling(Node);
    end;
    if (ParentNode.Index = nData) then
      DoData := ParentNode.CheckState in CheckedStates;
    ParentNode := TreeElements.GetNextSibling(ParentNode);
  end;

  CreateCode := '';
  TableHasAutoInc := False;

  // Columns code. Remove auto_increment attribute if pkey was unchecked, to overcome
  // "there can be only one auto column and it must be defined as a key"
  for Column in SelectedColumns do begin
    AutoIncGetsKey := False;
    AutoIncRemoved := False;
    if Column.DefaultType = cdtAutoInc then begin
      for Key in SelectedKeys do begin
        // Don't check index type, MySQL allows auto-increment columns on nearly all indexes
        if Key.Columns.IndexOf(Column.Name) > -1 then begin
          AutoIncGetsKey := True;
          Break;
        end;
      end;
      if not AutoIncGetsKey then begin
        Column.DefaultType := cdtNothing;
        AutoIncRemoved := True;
      end;
    end;
    TableHasAutoInc := TableHasAutoInc or (Column.DefaultType = cdtAutoInc);
    CreateCode := CreateCode + Format(ClausePattern, [Column.SQLCode]);
    if AutoIncRemoved then
      Column.DefaultType := cdtAutoInc;
    // Use this column for the INSERT..SELECT query only if it's not a virtual one
    if Column.Virtuality.IsEmpty then
      DataCols := DataCols + FConnection.QuoteIdent(Column.Name) + ', ';
  end;

  // Indexes code
  for Key in SelectedKeys do
    CreateCode := CreateCode + Format(ClausePattern, [Key.SQLCode]);

  // Foreign keys code
  for ForeignKey in SelectedForeignKeys do
    CreateCode := CreateCode + Format(ClausePattern, [ForeignKey.SQLCode(False)]);

  // Finish code
  Delete(CreateCode, Length(CreateCode)-2, 3);
  CreateCode := 'CREATE TABLE '+TargetTable+' ('+CRLF+CreateCode+CRLF+')'+CRLF;

  // Add collation and engine clauses
  if FDBObj.Collation <> '' then
    CreateCode := CreateCode + ' COLLATE ' + FConnection.EscapeString(FDBObj.Collation);
  if FDBObj.Engine <> '' then begin
    if FConnection.ServerVersionInt < 40018 then
      CreateCode := CreateCode + ' TYPE=' + FDBObj.Engine
    else
      CreateCode := CreateCode + ' ENGINE=' + FDBObj.Engine;
  end;
  if FDBObj.RowFormat <> '' then
    CreateCode := CreateCode + ' ROW_FORMAT=' + FDBObj.RowFormat;
  if (FDBObj.AutoInc > -1) and TableHasAutoInc then
    CreateCode := CreateCode + ' AUTO_INCREMENT=' + IntToStr(FDBObj.AutoInc);
  if FDBObj.Comment <> '' then
    CreateCode := CreateCode + ' COMMENT=' + FConnection.EscapeString(FDBObj.Comment);

  // Add INSERT .. SELECT .. FROM OrgTable clause
  InsertCode := '';
  if DoData and (DataCols <> '') then begin
    DataCols := Trim(DataCols);
    Delete(DataCols, Length(DataCols), 1);
    InsertCode := 'INSERT INTO '+TargetTable+' ('+DataCols+') SELECT ' + DataCols + ' FROM ' + FDBObj.QuotedName;
    if MemoFilter.GetTextLen > 0 then
      InsertCode := InsertCode + ' WHERE ' + MemoFilter.Text;
  end;

  // Run query and refresh list
  try
    MainForm.ShowStatusMsg(_('Creating table ...'));
    FConnection.Query(CreateCode);
    if InsertCode <> '' then
      FConnection.Query(InsertCode);
    // actRefresh takes care of whether the table editor is open
    if FConnection.Database = comboDatabase.Text then
      MainForm.actRefresh.Execute
    else
      FConnection.ClearDbObjects(comboDatabase.Text);
  except
    on E:EDbError do begin
      Screen.Cursor := crDefault;
      Msg := E.Message;
      if FConnection.LastErrorCode = 1075 then
        Msg := Msg + CRLF + CRLF +  f_('Please select the required index for the %s flag.', ['auto_increment']);
      ErrorDialog(Msg);
      ModalResult := mrNone;
    end;
  end;
  MainForm.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;

end.
