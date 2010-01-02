unit tabletools;


// -------------------------------------
// Table-diagnostics
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls, Buttons, Dialogs, StdActns,
  WideStrings, WideStrUtils, VirtualTrees, ExtCtrls, Contnrs, Graphics, TntStdCtrls,
  PngSpeedButton, SynRegExpr,
  mysql_connection, helpers;

type
  TToolMode = (tmMaintenance, tmFind, tmSQLExport, tmBulkTableEdit);
  TfrmTableTools = class(TForm)
    btnClose: TButton;
    pnlTop: TPanel;
    TreeObjects: TVirtualStringTree;
    spltHorizontally: TSplitter;
    pnlRight: TPanel;
    ResultGrid: TVirtualStringTree;
    tabsTools: TPageControl;
    tabMaintenance: TTabSheet;
    comboOperation: TComboBox;
    lblOperation: TLabel;
    chkQuick: TCheckBox;
    chkFast: TCheckBox;
    chkMedium: TCheckBox;
    chkExtended: TCheckBox;
    chkChanged: TCheckBox;
    chkUseFrm: TCheckBox;
    lblOptions: TLabel;
    btnHelp: TButton;
    tabFind: TTabSheet;
    lblFindText: TLabel;
    memoFindText: TTntMemo;
    comboDataTypes: TComboBox;
    lblDataTypes: TLabel;
    tabSQLexport: TTabSheet;
    chkExportDatabasesCreate: TCheckBox;
    chkExportDatabasesDrop: TCheckBox;
    chkExportTablesDrop: TCheckBox;
    chkExportTablesCreate: TCheckBox;
    lblExportData: TLabel;
    comboExportData: TComboBox;
    lblExportOutputType: TLabel;
    comboExportOutputType: TComboBox;
    comboExportOutputTarget: TTntComboBox;
    lblExportDatabases: TLabel;
    lblExportTables: TLabel;
    lblExportOutputTarget: TLabel;
    btnExecute: TButton;
    editSkipLargeTables: TEdit;
    udSkipLargeTables: TUpDown;
    lblSkipLargeTablesMB: TLabel;
    lblSkipLargeTables: TLabel;
    btnExportOutputTargetSelect: TPngSpeedButton;
    tabBulkTableEdit: TTabSheet;
    chkBulkTableEditDatabase: TCheckBox;
    comboBulkTableEditDatabase: TTntComboBox;
    chkBulkTableEditResetAutoinc: TCheckBox;
    chkBulkTableEditCollation: TCheckBox;
    comboBulkTableEditCollation: TComboBox;
    chkBulkTableEditEngine: TCheckBox;
    comboBulkTableEditEngine: TComboBox;
    chkBulkTableEditCharset: TCheckBox;
    comboBulkTableEditCharset: TComboBox;
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
    procedure Execute(Sender: TObject);
    procedure ResultGridInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure ResultGridGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure ResultGridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure TreeObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ResultGridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure ResultGridCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure ResultGridPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure ValidateControls(Sender: TObject);
    procedure chkExportOptionClick(Sender: TObject);
    procedure btnExportOutputTargetSelectClick(Sender: TObject);
    procedure comboExportOutputTargetExit(Sender: TObject);
    procedure comboExportOutputTypeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeObjectsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure chkBulkTableEditCheckComboClick(Sender: TObject);
    procedure TreeObjectsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeObjectsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
      var Allowed: Boolean);
  private
    { Private declarations }
    FResults: TObjectList;
    FToolMode: TToolMode;
    OutputFiles, OutputDirs: TWideStringList;
    ExportStream: TStream;
    ExportLastDatabase: Widestring;
    FTargetConnection: TMySQLConnection;
    FLastOutputSelectedIndex: Integer;
    FModifiedDbs: TWideStringList;
    procedure SetToolMode(Value: TToolMode);
    procedure AddResults(SQL: WideString);
    procedure AddNotes(Col1, Col2, Col3, Col4: WideString);
    procedure SetupResultGrid(Results: TMySQLQuery=nil);
    procedure UpdateResultGrid;
    procedure DoMaintenance(DBObj: TDBObject);
    procedure DoFind(DBObj: TDBObject);
    procedure DoExport(DBObj: TDBObject);
    procedure DoBulkTableEdit(DBObj: TDBObject);
  public
    { Public declarations }
    SelectedTables: TWideStringList;
    property ToolMode: TToolMode read FToolMode write SetToolMode;
  end;


implementation

uses main, mysql_structures;

const
  STRSKIPPED = 'Skipped - ';
  OUTPUT_FILE = 'One big file';
  OUTPUT_DIR = 'Directory - one file per object';
  OUTPUT_DB = 'Database';
  OUTPUT_SERVER = 'Server: ';
  DATA_NO = 'No data';
  DATA_REPLACE = 'Replace (truncate existing data)';
  DATA_INSERT = 'Insert';
  DATA_INSERTNEW = 'Insert new data (do not update existing)';
  DATA_UPDATE = 'Update existing data';
  EXPORT_FILE_FOOTER = '/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;'+CRLF+
    '/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;'+CRLF+
    '/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;'+CRLF;

{$R *.DFM}


procedure TfrmTableTools.FormCreate(Sender: TObject);
var
  i: Integer;
  SessionNames: TStringList;
begin
  // Restore GUI setup
  InheritFont(Font);
  Width := GetRegValue(REGNAME_TOOLSWINWIDTH, Width);
  Height := GetRegValue(REGNAME_TOOLSWINHEIGHT, Height);
  TreeObjects.Width := GetRegValue(REGNAME_TOOLSTREEWIDTH, TreeObjects.Width);

  // Find text tab
  memoFindText.Text := Utf8Decode(GetRegValue(REGNAME_TOOLSFINDTEXT, ''));
  comboDatatypes.Items.Add('All data types');
  for i:=Low(DatatypeCategories) to High(DatatypeCategories) do
    comboDatatypes.Items.Add(DatatypeCategories[i].Name);
  comboDatatypes.ItemIndex := GetRegValue(REGNAME_TOOLSDATATYPE, 0);

  // SQL export tab
  chkExportDatabasesCreate.Checked := GetRegValue(REGNAME_EXP_CREATEDB, chkExportDatabasesCreate.Checked);
  chkExportDatabasesDrop.Checked := GetRegValue(REGNAME_EXP_DROPDB, chkExportDatabasesDrop.Checked);
  chkExportTablesCreate.Checked := GetRegValue(REGNAME_EXP_CREATETABLE, chkExportTablesCreate.Checked);
  chkExportTablesDrop.Checked := GetRegValue(REGNAME_EXP_DROPTABLE, chkExportTablesDrop.Checked);
  comboExportData.Items.Text := DATA_NO+CRLF +DATA_REPLACE+CRLF +DATA_INSERT+CRLF +DATA_INSERTNEW+CRLF +DATA_UPDATE;
  comboExportData.ItemIndex := GetRegValue(REGNAME_EXP_DATAHOW, 0);
  OutputFiles := TWideStringList.Create;
  OutputDirs := TWideStringList.Create;
  OutputFiles.Text := GetRegValue(REGNAME_EXP_OUTFILES, '');
  OutputDirs.Text := GetRegValue(REGNAME_EXP_OUTDIRS, '');
  comboExportOutputType.Items.Text := OUTPUT_FILE+CRLF +OUTPUT_DIR+CRLF +OUTPUT_DB;
  comboExportOutputType.ItemIndex := GetRegValue(REGNAME_EXP_OUTPUT, 0);
  comboExportOutputTarget.Text := '';
  // Add session names from registry
  SessionNames := TStringList.Create;
  MainReg.OpenKey(REGPATH + REGKEY_SESSIONS, True);
  MainReg.GetKeyNames(SessionNames);
  for i:=0 to SessionNames.Count-1 do begin
    if SessionNames[i] <> Mainform.SessionName then
      comboExportOutputType.Items.Add(OUTPUT_SERVER+SessionNames[i]);
  end;

  // Various
  udSkipLargeTables.Position := GetRegValue(REGNAME_TOOLSSKIPMB, udSkipLargeTables.Position);
  SetWindowSizeGrip( Self.Handle, True );
  FixVT(TreeObjects);
  FixVT(ResultGrid);
  FResults := TObjectList.Create;
  SelectedTables := TWideStringList.Create;
  FModifiedDbs := TWideStringList.Create;
  FModifiedDbs.Duplicates := dupIgnore;
end;


procedure TfrmTableTools.FormDestroy(Sender: TObject);
var
  OutputItem: Integer;
begin
  // Save GUI setup
  OpenRegistry;
  MainReg.WriteInteger( REGNAME_TOOLSWINWIDTH, Width );
  MainReg.WriteInteger( REGNAME_TOOLSWINHEIGHT, Height );
  MainReg.WriteInteger( REGNAME_TOOLSTREEWIDTH, TreeObjects.Width);

  MainReg.WriteString( REGNAME_TOOLSFINDTEXT, Utf8Encode(memoFindText.Text));
  MainReg.WriteInteger( REGNAME_TOOLSDATATYPE, comboDatatypes.ItemIndex);

  MainReg.WriteBool(REGNAME_EXP_CREATEDB, chkExportDatabasesCreate.Checked);
  MainReg.WriteBool(REGNAME_EXP_DROPDB, chkExportDatabasesDrop.Checked);
  MainReg.WriteBool(REGNAME_EXP_CREATETABLE, chkExportTablesCreate.Checked);
  MainReg.WriteBool(REGNAME_EXP_DROPTABLE, chkExportTablesDrop.Checked);
  MainReg.WriteInteger(REGNAME_EXP_DATAHOW, comboExportData.ItemIndex);
  // Do not remember a selected session name for the next time
  OutputItem := comboExportOutputType.ItemIndex;
  if OutputItem > 2 then
    OutputItem := 0;
  MainReg.WriteInteger(REGNAME_EXP_OUTPUT, OutputItem);
  MainReg.WriteString(REGNAME_EXP_OUTFILES, OutputFiles.Text);
  MainReg.WriteString(REGNAME_EXP_OUTDIRS, OutputDirs.Text);

  MainReg.WriteInteger( REGNAME_TOOLSSKIPMB, udSkipLargeTables.Position);
end;


procedure TfrmTableTools.FormShow(Sender: TObject);
var
  DBNode, TableNode, FirstChecked: PVirtualNode;
begin
  // When this form is displayed the second time, databases may be deleted or filtered.
  // Also, checked nodes must be unchecked and unchecked nodes may need to be checked.
  TreeObjects.Clear;
  TreeObjects.RootNodeCount := Mainform.DBtree.RootNodeCount;

  DBNode := TreeObjects.GetFirstChild(TreeObjects.GetFirst);
  while Assigned(DBNode) do begin
    if TreeObjects.Text[DBNode, 0] = Mainform.ActiveDatabase then begin
      if SelectedTables.Count = 0 then begin
        // Preselect active database
        DBNode.CheckState := csCheckedNormal;
      end else begin
        DBNode.CheckState := csMixedNormal;
        // Expand db node so checked table nodes are visible
        TreeObjects.Expanded[DBNode] := true;
        TableNode := TreeObjects.GetFirstChild(DBNode);
        while Assigned(TableNode) do begin
          if SelectedTables.IndexOf(TreeObjects.Text[TableNode, 0]) > -1 then
            TableNode.CheckState := csCheckedNormal;
          TableNode := TreeObjects.GetNextSibling(TableNode);
        end;
      end;
    end;
    DBNode := TreeObjects.GetNextSibling(DBNode);
  end;

  FirstChecked := TreeObjects.GetFirstChecked;
  if Assigned(FirstChecked) then
    TreeObjects.ScrollIntoView(FirstChecked, True);
  // CHECKSUM available since MySQL 4.1.1
  if Mainform.Connection.ServerVersionInt < 40101 then
    comboOperation.Items[comboOperation.Items.IndexOf('Checksum')] := 'Checksum ('+STR_NOTSUPPORTED+')';
  comboOperation.OnChange(Sender);
  comboExportOutputType.OnChange(Sender);

  comboBulkTableEditDatabase.Items.Text := Mainform.Databases.Text;
  if comboBulkTableEditDatabase.Items.Count > 0 then
    comboBulkTableEditDatabase.ItemIndex := 0;

  comboBulkTableEditEngine.Items := Mainform.Connection.TableEngines;
  if comboBulkTableEditEngine.Items.Count > 0 then
    comboBulkTableEditEngine.ItemIndex := comboBulkTableEditEngine.Items.IndexOf(Mainform.Connection.TableEngineDefault);

  comboBulkTableEditCollation.Items := Mainform.Connection.CollationList;
  if comboBulkTableEditCollation.Items.Count > 0 then
    comboBulkTableEditCollation.ItemIndex := 0;

  comboBulkTableEditCharset.Items := Mainform.Connection.CharsetList;
  if comboBulkTableEditCharset.Items.Count > 0 then
    comboBulkTableEditCharset.ItemIndex := 0;
end;


procedure TfrmTableTools.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Auto close temorary connection
  if Assigned(FTargetConnection) then
    FreeAndNil(FTargetConnection);
end;


procedure TfrmTableTools.ValidateControls(Sender: TObject);
var
  SomeChecked, OptionChecked: Boolean;
  op: String;
begin
  SomeChecked := TreeObjects.CheckedCount > 0;
  if tabsTools.ActivePage = tabMaintenance then begin
    btnExecute.Caption := 'Execute';
    btnExecute.Enabled := (Pos(STR_NOTSUPPORTED, comboOperation.Text) = 0) and SomeChecked;
    // Only enable available options
    op := LowerCase(comboOperation.Text);
    chkQuick.Enabled := (op = 'check') or (op = 'checksum') or (op = 'repair');
    chkFast.Enabled := op = 'check';
    chkMedium.Enabled := op = 'check';
    chkExtended.Enabled := (op = 'check') or (op = 'checksum') or (op = 'repair');
    chkChanged.Enabled := op = 'check';
    chkUseFrm.Enabled := op = 'repair';
    // CHECKSUM's options are mutually exclusive
    if comboOperation.Text = 'Checksum' then begin
      if (Sender = chkExtended) and chkExtended.Checked then chkQuick.Checked := False
      else if chkQuick.Checked then chkExtended.Checked := False;
    end;
  end else if tabsTools.ActivePage = tabFind then begin
    btnExecute.Caption := 'Find';
    btnExecute.Enabled := SomeChecked and (memoFindText.Text <> '');
  end else if tabsTools.ActivePage = tabSQLExport then begin
    btnExecute.Caption := 'Export';
    btnExecute.Enabled := SomeChecked;
  end else if tabsTools.ActivePage = tabBulkTableEdit then begin
    btnExecute.Caption := 'Update';
    chkBulkTableEditCollation.Enabled := Mainform.Connection.IsUnicode;
    chkBulkTableEditCharset.Enabled := Mainform.Connection.IsUnicode;
    OptionChecked := chkBulkTableEditDatabase.Checked or chkBulkTableEditEngine.Checked or chkBulkTableEditCollation.Checked
      or chkBulkTableEditCharset.Checked or chkBulkTableEditResetAutoinc.Checked;
    btnExecute.Enabled := SomeChecked and OptionChecked;
  end;
end;


procedure TfrmTableTools.TreeObjectsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  Mainform.DBtreeChange(Sender, Node);
end;


procedure TfrmTableTools.TreeObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  ValidateControls(Sender);
end;


procedure TfrmTableTools.TreeObjectsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var NewState: TCheckState; var Allowed: Boolean);
var
  n: PVirtualNode;
begin
  // Ensure to also toggle check state of not yet initialized nodes
  Allowed := True;
  // Weird fix: Just iterate through all sub nodes for implicit initialization. Without this
  // loop a checkbox click on a parent node would only auto-check its visible children.
  n := Sender.GetFirstChild(Node);
  while Assigned(n) do
    n := Sender.GetNextSibling(n);
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
begin
  // Attach a checkbox to all nodes
  Mainform.DBtreeInitNode(Sender, ParentNode, Node, InitialStates);
  Node.CheckType := ctTriStateCheckBox;
  Node.CheckState := csUncheckedNormal;
  ValidateControls(Sender);
end;


procedure TfrmTableTools.TreeObjectsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
begin
  Mainform.DBtreePaintText(Sender, TargetCanvas, Node, Column, TextType);
end;


procedure TfrmTableTools.btnHelpClick(Sender: TObject);
begin
  Mainform.CallSQLHelpWithKeyword(UpperCase(comboOperation.Text) + ' TABLE');
end;


procedure TfrmTableTools.Execute(Sender: TObject);
var
  DBNode, TableNode: PVirtualNode;
  DBObjects, Views: TDBObjectList;
  DBObj: TDBObject;
  i: Integer;
  ProcessNodeFunc: procedure(DBObj: TDBObject) of Object;
begin
  Screen.Cursor := crHourGlass;
  if tabsTools.ActivePage = tabMaintenance then
    FToolMode := tmMaintenance
  else if tabsTools.ActivePage = tabFind then
    FToolMode := tmFind
  else if tabsTools.ActivePage = tabSQLExport then
    FToolMode := tmSQLExport
  else if tabsTools.ActivePage = tabBulkTableEdit then
    FToolMode := tmBulkTableEdit;
  ResultGrid.Clear;
  FResults.Clear;
  Views := TDBObjectList.Create;
  Views.OwnsObjects := False; // So we can .Free that object afterwards without loosing the contained objects
  TreeObjects.SetFocus;
  ProcessNodeFunc := nil;
  DBNode := TreeObjects.GetFirstChild(TreeObjects.GetFirst);
  while Assigned(DBNode) do begin
    if not (DBNode.CheckState in [csUncheckedNormal, csUncheckedPressed]) then begin
      TableNode := TreeObjects.GetFirstChild(DBNode);
      while Assigned(TableNode) do begin
        if (csCheckedNormal in [TableNode.CheckState, DBNode.CheckState]) and (TableNode.CheckType <> ctNone) then begin
          DBObjects := Mainform.Connection.GetDBObjects(TreeObjects.Text[DBNode, 0]);
          DBObj := DBObjects[TableNode.Index];
          // Find table in cashed dataset and check its size - perhaps it has to be skipped
          if (udSkipLargeTables.Position = 0) or ((DBObj.Size div SIZE_MB) < udSkipLargeTables.Position) then try
            case FToolMode of
              tmMaintenance: ProcessNodeFunc := DoMaintenance;
              tmFind: ProcessNodeFunc := DoFind;
              tmSQLExport: ProcessNodeFunc := DoExport;
              tmBulkTableEdit: ProcessNodeFunc := DoBulkTableEdit;
            end;
            // Views have to be exported at the very end so at least all needed tables are ready when a view gets imported
            if DBObj.NodeType = lntView then
              Views.Add(DBObj)
            else
              ProcessNodeFunc(DBObj);
          except
            // The above SQL can easily throw an exception, e.g. if a table is corrupted.
            // In such cases we create a dummy row, including the error message
            on E:Exception do
              AddNotes(DBObj.Database, DBObj.Name, 'error', E.Message);
          end else begin
            AddNotes(DBObj.Database, DBObj.Name, STRSKIPPED+FormatByteNumber(DBObj.Size), '');
          end;
        end;
        TableNode := TreeObjects.GetNextSibling(TableNode);
      end;
    end;
    DBNode := TreeObjects.GetNextSibling(DBNode);
  end;

  // Special block for late created views in export mode
  for i:=0 to Views.Count-1 do begin
    try
      ProcessNodeFunc(Views[i]);
    except on E:Exception do
      AddNotes(Views[i].Database, Views[i].Name, 'error', E.Message);
    end;
  end;

  if Assigned(ExportStream) then begin
    if comboExportOutputType.Text = OUTPUT_FILE then
      StreamWrite(ExportStream, EXPORT_FILE_FOOTER);
    FreeAndNil(ExportStream);
  end;
  ExportLastDatabase := '';

  if FModifiedDbs.Count > 0 then begin
    for i:=0 to FModifiedDbs.Count-1 do
      Mainform.Connection.ClearDbObjects(FModifiedDbs[i]);
    TreeObjects.ResetNode(TreeObjects.GetFirst);
    Mainform.DBtree.ResetNode(Mainform.DBtree.GetFirst);
    FModifiedDbs.Clear;
  end;
  Screen.Cursor := crDefault;
end;


procedure TfrmTableTools.DoMaintenance(DBObj: TDBObject);
var
  SQL: WideString;
begin
  if not (DBObj.NodeType in [lntTable, lntView]) then begin
    AddNotes(DBObj.Database, DBObj.Name, STRSKIPPED+'a '+LowerCase(DBObj.ObjType)+' cannot be maintained.', '');
    Exit;
  end;
  SQL := UpperCase(comboOperation.Text) + ' TABLE ' + Mainform.mask(DBObj.Database) + '.' + Mainform.mask(DBObj.Name);
  if chkQuick.Enabled and chkQuick.Checked then SQL := SQL + ' QUICK';
  if chkFast.Enabled and chkFast.Checked then SQL := SQL + ' FAST';
  if chkMedium.Enabled and chkMedium.Checked then SQL := SQL + ' MEDIUM';
  if chkExtended.Enabled and chkExtended.Checked then SQL := SQL + ' EXTENDED';
  if chkChanged.Enabled and chkChanged.Checked then SQL := SQL + ' CHANGED';
  if chkUseFrm.Enabled and chkUseFrm.Checked then SQL := SQL + ' USE_FRM';
  AddResults(SQL);
end;


procedure TfrmTableTools.DoFind(DBObj: TDBObject);
var
  Results: TMySQLQuery;
  SQL: WideString;
  HasSelectedDatatype: Boolean;
  i: Integer;
begin
  if not (DBObj.NodeType in [lntTable, lntView]) then begin
    AddNotes(DBObj.Database, DBObj.Name, STRSKIPPED+'a '+LowerCase(DBObj.ObjType)+' does not contain rows.', '');
    Exit;
  end;
  Results := Mainform.Connection.GetResults('SHOW COLUMNS FROM '+Mainform.mask(DBObj.Database)+'.'+Mainform.mask(DBObj.Name));
  SQL := '';
  while not Results.Eof do begin
    HasSelectedDatatype := comboDatatypes.ItemIndex = 0;
    if not HasSelectedDatatype then for i:=Low(Datatypes) to High(Datatypes) do begin
      HasSelectedDatatype := (LowerCase(getFirstWord(Results.Col('Type'))) = LowerCase(Datatypes[i].Name))
        and (Integer(Datatypes[i].Category)+1 = comboDatatypes.ItemIndex);
      if HasSelectedDatatype then
        break;
    end;
    if HasSelectedDatatype then
      SQL := SQL + Mainform.mask(Results.Col('Field')) + ' LIKE ' + esc('%'+memoFindText.Text+'%') + ' OR ';
    Results.Next;
  end;
  if SQL <> '' then begin
    Delete(SQL, Length(SQL)-3, 3);
    SQL := 'SELECT '''+DBObj.Database+''' AS `Database`, '''+DBObj.Name+''' AS `Table`, COUNT(*) AS `Found rows`, '
      + 'CONCAT(ROUND(100 / '+IntToStr(Max(DBObj.Rows,1))+' * COUNT(*), 1), ''%'') AS `Relevance` FROM '+Mainform.mask(DBObj.Database)+'.'+Mainform.mask(DBObj.Name)+' WHERE '
      + SQL;
    AddResults(SQL);
  end else
    AddNotes(DBObj.Database, DBObj.Name, STRSKIPPED+'table doesn''t have columns of selected type ('+comboDatatypes.Text+').', '');
end;


procedure TfrmTableTools.AddResults(SQL: WideString);
var
  i: Integer;
  Row: TWideStringlist;
  Results: TMySQLQuery;
begin
  // Execute query and append results into grid
  Results := Mainform.Connection.GetResults(SQL);
  if Results = nil then
    Exit;

  SetupResultGrid(Results);
  Results.First;
  while not Results.Eof do begin
    Row := TWideStringlist.Create;
    for i:=0 to Results.ColumnCount-1 do begin
      if Results.DataType(i).Category = dtcInteger then
        Row.Add(FormatNumber(Results.Col(i)))
      else
        Row.Add(Results.Col(i));
    end;
    FResults.Add(Row);
    Results.Next;
  end;
  Results.Free;

  UpdateResultGrid;
end;


procedure TfrmTableTools.AddNotes(Col1, Col2, Col3, Col4: WideString);
var
  Row: TWideStringlist;
begin
  // Adds a row with non SQL results
  SetupResultGrid;
  Row := TWideStringlist.Create;
  Row.Add(Col1);
  Row.Add(Col2);
  Row.Add(Col3);
  Row.Add(Col4);
  FResults.Add(Row);
  UpdateResultGrid;
end;


procedure TfrmTableTools.SetupResultGrid(Results: TMySQLQuery=nil);
var
  ColCount, i: Integer;
  Col: TVirtualTreeColumn;
begin
  if Assigned(Results) then begin
    ColCount := Results.ColumnCount;
    ResultGrid.Header.Options := ResultGrid.Header.Options + [hoVisible];
  end else begin
    ColCount := 4;
    ResultGrid.Header.Options := ResultGrid.Header.Options - [hoVisible];
  end;

  // Add missing columns
  for i:=ResultGrid.Header.Columns.Count to ColCount-1 do begin
    Col := ResultGrid.Header.Columns.Add;
    Col.Width := 130;
  end;
  // Remove superfluous columns
  for i:=ResultGrid.Header.Columns.Count-1 downto ColCount do
    ResultGrid.Header.Columns[i].Free;

  // Set column header names
  for i:=0 to ResultGrid.Header.Columns.Count-1 do begin
    Col := ResultGrid.Header.Columns[i];
    if Assigned(Results) then begin
      Col.Text := Results.ColumnNames[i];
      if Results.DataType(i).Category in [dtcInteger, dtcIntegerNamed, dtcReal] then
        Col.Alignment := taRightJustify
      else
        Col.Alignment := taLeftJustify;
    end;
  end;
end;


procedure TfrmTableTools.UpdateResultGrid;
begin
  // Refresh resultgrid
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
  // Red text color for errors, purple for notes, grey for skipped tables
  if not (vsSelected in Node.States) then begin
    VT := Sender as TVirtualStringTree;
    Msg := VT.Text[Node, 2];
    if LowerCase(Msg) = 'note' then
      TargetCanvas.Font.Color := clPurple
    else if LowerCase(Msg) = 'error' then
      TargetCanvas.Font.Color := clRed
    else if Pos(STRSKIPPED, Msg) > 0 then
      TargetCanvas.Font.Color := clGrayText;
  end;
end;

procedure TfrmTableTools.ResultGridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
var
  Data: ^TWideStringList;
begin
  if Column > NoColumn then begin
    Data := Sender.GetNodeData(Node);
    if Data^.Count > Column then
      CellText := Data^[Column]
    else
      CellText := '';
  end;
end;


procedure TfrmTableTools.ResultGridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  // Header column clicked to sort
  Mainform.vstHeaderClick(Sender, HitInfo);
end;


procedure TfrmTableTools.comboExportOutputTypeChange(Sender: TObject);
var
  OldItem: WideString;
  NewIdx, NetType: Integer;
  DBNode: PVirtualNode;
  SessionName: String;
  Databases: TWideStringlist;
begin
  // Target type (file, directory, ...) selected
  OldItem := comboExportOutputTarget.Text;
  if Assigned(FTargetConnection) then
    FreeAndNil(FTargetConnection);
  if comboExportOutputType.Text = OUTPUT_FILE then begin
    comboExportOutputTarget.Style := csDropDown;
    comboExportOutputTarget.Items.Text := OutputFiles.Text;
    lblExportOutputTarget.Caption := 'Filename:';
    btnExportOutputTargetSelect.Enabled := True;
    btnExportOutputTargetSelect.PngImage := Mainform.PngImageListMain.PngImages[10].PngImage;
  end else if comboExportOutputType.Text = OUTPUT_DIR then begin
    comboExportOutputTarget.Style := csDropDown;
    comboExportOutputTarget.Items.Text := OutputDirs.Text;
    lblExportOutputTarget.Caption := 'Directory:';
    btnExportOutputTargetSelect.Enabled := True;
    btnExportOutputTargetSelect.PngImage := Mainform.PngImageListMain.PngImages[51].PngImage;
  end else if comboExportOutputType.Text = OUTPUT_DB then begin
    comboExportOutputTarget.Style := csDropDownList;
    lblExportOutputTarget.Caption := 'Database:';
    btnExportOutputTargetSelect.Enabled := False;
    btnExportOutputTargetSelect.PngImage := Mainform.PngImageListMain.PngImages[27].PngImage;
    // Add unchecked databases
    comboExportOutputTarget.Items.Clear;
    DBNode := TreeObjects.GetFirstChild(TreeObjects.GetFirst);
    while Assigned(DBNode) do begin
      if DBNode.CheckState in [csUncheckedNormal, csUncheckedPressed] then
        comboExportOutputTarget.Items.Add(TreeObjects.Text[DBNode, 0]);
      DBNode := TreeObjects.GetNextSibling(DBNode);
    end;
  end else begin
    // Server selected. Display databases in below dropdown
    comboExportOutputTarget.Style := csDropDownList;
    lblExportOutputTarget.Caption := 'Database:';
    btnExportOutputTargetSelect.Enabled := False;
    btnExportOutputTargetSelect.PngImage := Mainform.PngImageListMain.PngImages[27].PngImage;
    SessionName := Copy(comboExportOutputType.Text, Length(OUTPUT_SERVER)+1, Length(comboExportOutputType.Text));
    FreeAndNil(FTargetConnection);
    FTargetConnection := TMySQLConnection.Create(Self);
    FTargetConnection.Hostname := GetRegValue(REGNAME_HOST, DEFAULT_HOST, SessionName);
    FTargetConnection.Username := GetRegValue(REGNAME_USER, DEFAULT_USER, SessionName);
    FTargetConnection.Password := decrypt(GetRegValue(REGNAME_PASSWORD, DEFAULT_PASSWORD, SessionName));
    FTargetConnection.Port := StrToInt(GetRegValue(REGNAME_PORT, IntToStr(DEFAULT_PORT), SessionName));
    if GetRegValue(REGNAME_COMPRESSED, DEFAULT_COMPRESSED, SessionName) then
      FTargetConnection.Options := FTargetConnection.Options + [opCompress]
    else
      FTargetConnection.Options := FTargetConnection.Options - [opCompress];
    NetType := GetRegValue(REGNAME_NETTYPE, NETTYPE_TCPIP, SessionName);
    if NetType = NETTYPE_TCPIP then
      FTargetConnection.Socketname := ''
    else begin
      FTargetConnection.Socketname := FTargetConnection.Hostname;
      FTargetConnection.Hostname := '.';
    end;
    FTargetConnection.LogPrefix := '['+SessionName+'] ';
    FTargetConnection.OnLog := Mainform.LogSQL;
    Screen.Cursor := crHourglass;
    try
      FTargetConnection.Active := True;
      Databases := FTargetConnection.GetCol('SHOW DATABASES');
      comboExportOutputTarget.Items.Text := Databases.Text;
      comboExportOutputTarget.Items.Insert(0, '[Same as on source server]');
      comboExportOutputTarget.ItemIndex := 0;
      Screen.Cursor := crDefault;
    except
      on E:Exception do begin
        Screen.Cursor := crDefault;
        MessageDlg(E.Message, mtError, [mbOK], 0);
        comboExportOutputType.ItemIndex := FLastOutputSelectedIndex;
      end;
    end;
  end;

  FLastOutputSelectedIndex := comboExportOutputType.ItemIndex;
  chkExportDatabasesCreate.Enabled := (comboExportOutputType.Text = OUTPUT_FILE)
    or (Copy(comboExportOutputType.Text, 1, Length(OUTPUT_SERVER)) = OUTPUT_SERVER);
  chkExportDatabasesDrop.Enabled := chkExportDatabasesCreate.Enabled;
  NewIdx := comboExportOutputTarget.Items.IndexOf(OldItem);
  if (NewIdx = -1) and (comboExportOutputTarget.Items.Count > 0) then
    NewIdx := 0;
  comboExportOutputTarget.ItemIndex := NewIdx;
end;


procedure TfrmTableTools.comboExportOutputTargetExit(Sender: TObject);
var
  ItemList: TWideStringList;
  idx: Integer;
begin
  // Add typed text to recent items
  if comboExportOutputTarget.Text = '' then
    Exit;
  ItemList := nil;
  if comboExportOutputType.Text = OUTPUT_FILE then
    ItemList := OutputFiles
  else if comboExportOutputType.Text = OUTPUT_DIR then
    ItemList := OutputDirs;
  if not Assigned(ItemList) then
    Exit;
  idx := ItemList.IndexOf(comboExportOutputTarget.Text);
  if idx > -1 then
    ItemList.Delete(idx);
  ItemList.Insert(0, comboExportOutputTarget.Text);
end;


procedure TfrmTableTools.chkExportOptionClick(Sender: TObject);
  procedure WarnIfChecked(chk: TCheckBox; LabelText: String);
  begin
    if chk.Checked then begin
      chk.Caption := LabelText + '!!';
      chk.Font.Style := chk.Font.Style + [fsBold];
    end else begin
      chk.Caption := LabelText;
      chk.Font.Style := Font.Style;
    end;
  end;
begin
  if (Sender = chkExportDatabasesDrop) and chkExportDatabasesDrop.Checked then
    chkExportDatabasesCreate.Checked := True
  else if (Sender = chkExportDatabasesCreate) and (not chkExportDatabasesCreate.Checked) then
    chkExportDatabasesDrop.Checked := False
  else if (Sender = chkExportTablesDrop) and chkExportTablesDrop.Checked then
    chkExportTablesCreate.Checked := True
  else if (Sender = chkExportTablesCreate) and (not chkExportTablesCreate.Checked) then
    chkExportTablesDrop.Checked := False;
  WarnIfChecked(chkExportDatabasesDrop, 'Drop');
  WarnIfChecked(chkExportTablesDrop, 'Drop');
end;


procedure TfrmTableTools.btnExportOutputTargetSelectClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  Browse: TBrowseForFolder;
begin
  case comboExportOutputType.ItemIndex of
    0: begin
      // Select filename
      SaveDialog := TSaveDialog.Create(Self);
      SaveDialog.DefaultExt := 'sql';
      SaveDialog.Filter := 'SQL-Scripts (*.sql)|*.sql|All Files (*.*)|*.*';
      SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];
      if SaveDialog.Execute then
        comboExportOutputTarget.Text := SaveDialog.FileName;
      SaveDialog.Free;
    end;
    1: begin
      Browse := TBrowseForFolder.Create(Self);
      Browse.Folder := comboExportOutputTarget.Text;
      Browse.DialogCaption := 'Select output directory';
      // Enable "Create new folder" button
      Browse.BrowseOptions := Browse.BrowseOptions - [bifNoNewFolderButton] + [bifNewDialogStyle];
      if Browse.Execute then
        comboExportOutputTarget.Text := Browse.Folder;
      Browse.Free;
    end;
  end;
end;

procedure TfrmTableTools.SetToolMode(Value: TToolMode);
begin
  FToolMode := Value;
  case FToolMode of
    tmMaintenance: tabsTools.ActivePage := tabMaintenance;
    tmFind: tabsTools.ActivePage := tabFind;
    tmSQLExport: tabsTools.ActivePage := tabSQLExport;
    tmBulkTableEdit: tabsTools.ActivePage := tabBulkTableEdit;
  end;
end;


procedure TfrmTableTools.DoExport(DBObj: TDBObject);
var
  ToFile, ToDir, ToDb, ToServer, IsLastRowInChunk, NeedsDBStructure: Boolean;
  Struc, Header, FinalDbName, BaseInsert, Row, TargetDbAndObject: WideString;
  LogRow, MultiSQL: TWideStringlist;
  i: Integer;
  RowCount, MaxRowsInChunk, RowsInChunk, Limit, Offset, ResultCount: Int64;
  StartTime: Cardinal;
  StrucResult, Data: TMySQLQuery;
  rx: TRegExpr;
const
  TempDelim = '//';

  // Short version of Mainform.Mask()
  function m(s: WideString): WideString;
  begin
    Result := Mainform.mask(s);
  end;

  // Pass output to file or query, and append semicolon if needed
  procedure Output(SQL: WideString; IsEndOfQuery, ForFile, ForDir, ForDb, ForServer: Boolean);
  var
    SA: AnsiString;
    ChunkSize: Integer;
  begin
    if (ToFile and ForFile) or (ToDir and ForDir) then begin
      if IsEndOfQuery then
        SQL := SQL + ';'+CRLF;
      StreamWrite(ExportStream, SQL);
    end;
    if (ToDb and ForDb) or (ToServer and ForServer) then begin
      StreamWrite(ExportStream, SQL);
      if IsEndOfQuery then begin
        ExportStream.Position := 0;
        ChunkSize := ExportStream.Size;
        SetLength(SA, ChunkSize div SizeOf(AnsiChar));
        ExportStream.Read(PAnsiChar(SA)^, ChunkSize);
        ExportStream.Size := 0;
        SQL := UTF8Decode(SA);
        if ToDB then Mainform.Connection.Query(SQL)
        else if ToServer then FTargetConnection.Query(SQL);
        SQL := '';
      end;
    end;

  end;
begin
  // Handle one table, view or routine in SQL export mode
  AddResults('SELECT '+esc(DBObj.Database)+' AS '+Mainform.mask('Database')+', ' +
    esc(DBObj.Name)+' AS '+Mainform.mask('Table')+', ' +
    IntToStr(DBObj.Rows)+' AS '+Mainform.mask('Rows')+', '+
    '0 AS '+Mainform.mask('Duration')
    );
  ToFile := comboExportOutputType.Text = OUTPUT_FILE;
  ToDir := comboExportOutputType.Text = OUTPUT_DIR;
  ToDb := comboExportOutputType.Text = OUTPUT_DB;
  ToServer := Copy(comboExportOutputType.Text, 1, Length(OUTPUT_SERVER)) = OUTPUT_SERVER;
  StartTime := GetTickCount;
  try
    if ToDir then begin
      FreeAndNil(ExportStream);
      if not DirectoryExists(comboExportOutputTarget.Text) then
        ForceDirectories(comboExportOutputTarget.Text);
      ExportStream := TFileStream.Create(comboExportOutputTarget.Text+'\'+GoodFileName(DBObj.Name)+'.sql', fmCreate or fmOpenWrite);
    end;
    if ToFile and (not Assigned(ExportStream)) then
      ExportStream := TFileStream.Create(comboExportOutputTarget.Text, fmCreate or fmOpenWrite);
    if ToDb or ToServer then
      ExportStream := TMemoryStream.Create;
    if (DBObj.Database<>ExportLastDatabase) or ToDir then begin
      Header := '# --------------------------------------------------------' + CRLF +
        WideFormat('# %-30s%s', ['Host:', Mainform.Connection.HostName]) + CRLF +
        WideFormat('# %-30s%s', ['Database:', DBObj.Database]) + CRLF +
        WideFormat('# %-30s%s', ['Server version:', Mainform.Connection.ServerVersionUntouched]) + CRLF +
        WideFormat('# %-30s%s', ['Server OS:', Mainform.Connection.GetVar('SHOW VARIABLES LIKE ' + esc('version_compile_os'), 1)]) + CRLF +
        WideFormat('# %-30s%s', [APPNAME + ' version:', FullAppVersion]) + CRLF +
        WideFormat('# %-30s%s', ['Date/time:', DateTimeToStr(Now)]) + CRLF +
        '# --------------------------------------------------------' + CRLF + CRLF +
        '/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;' + CRLF +
        '/*!40101 SET NAMES '+Mainform.Connection.CharacterSet+' */;' + CRLF +
        '/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;' + CRLF +
        '/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE=''NO_AUTO_VALUE_ON_ZERO'' */;';
      Output(Header, False, DBObj.Database<>ExportLastDatabase, True, False, False);
    end;
  except
    on E:Exception do begin
      MessageDlg(E.Message, mterror, [mbOK], 0);
      Exit;
    end;
  end;

  // Database structure. Do that only in single-file and server mode. drop/create/use in directory or database mode makes no sense
  FinalDbName := DBObj.Database;
  if ToDb or (ToServer and (comboExportOutputTarget.ItemIndex > 0)) then
    FinalDbName := comboExportOutputTarget.Text;
  NeedsDBStructure := FinalDbName <> ExportLastDatabase;
  if chkExportDatabasesDrop.Checked or chkExportDatabasesCreate.Checked then begin
    Output(CRLF+'# Dumping database structure for '+DBObj.Database+CRLF, False, NeedsDBStructure, False, False, False);
    if chkExportDatabasesDrop.Checked and chkExportDatabasesDrop.Enabled then
      Output('DROP DATABASE IF EXISTS '+m(FinalDbName), True, NeedsDBStructure, False, False, NeedsDBStructure);
    if chkExportDatabasesCreate.Checked and chkExportDatabasesCreate.Enabled then begin
      if Mainform.Connection.ServerVersionInt >= 40100 then begin
        Struc := Mainform.Connection.GetVar('SHOW CREATE DATABASE '+m(DBObj.Database), 1);
        // Gracefully ignore it when target database exists, important in server mode
        Insert('IF NOT EXISTS ', Struc, Pos('DATABASE', Struc) + 9);
        // Create the right dbname
        Struc := WideStringReplace(Struc, DBObj.Database, FinalDbName, []);
      end else
        Struc := 'CREATE DATABASE IF NOT EXISTS '+m(FinalDbName);
      Output(Struc, True, NeedsDBStructure, False, False, NeedsDBStructure);
      Output('USE '+m(FinalDbName), True, NeedsDBStructure, False, False, NeedsDBStructure);
    end;
  end;
  if ToServer and (not chkExportDatabasesCreate.Checked) then begin
    // Export to server without "CREATE/USE dbname" and "Same dbs as on source server" - needs a "USE dbname"
    Output('USE '+m(FinalDbName), True, False, False, False, NeedsDBStructure);
  end;

  // Table structure
  if chkExportTablesDrop.Checked or chkExportTablesCreate.Checked then begin
    Output(CRLF+CRLF+'# Dumping structure for '+LowerCase(DBObj.ObjType)+' '+DBObj.Database+'.'+DBObj.Name+CRLF, False, True, True, False, False);
    if chkExportTablesDrop.Checked then begin
      Struc := 'DROP '+DBObj.ObjType+' IF EXISTS ';
      if ToDb then
        Struc := Struc + m(FinalDbName)+'.';
      Struc := Struc + m(DBObj.Name);
      Output(Struc, True, True, True, True, True);
    end;
    if chkExportTablesCreate.Checked then begin
      try
        case DBObj.NodeType of
          lntTable, lntView: begin
            Struc := Mainform.Connection.GetVar('SHOW CREATE TABLE '+m(DBObj.Database)+'.'+m(DBObj.Name), 1);
            // Remove AUTO_INCREMENT clause
            rx := TRegExpr.Create;
            rx.ModifierI := True;
            rx.Expression := '\sAUTO_INCREMENT\s*\=\s*\d+\s';
            Struc := rx.Replace(Struc, ' ');
            rx.Free;
            if DBObj.NodeType = lntTable then
              Insert('IF NOT EXISTS ', Struc, Pos('TABLE', Struc) + 6);
            if ToDb then begin
              if DBObj.NodeType = lntTable then
                Insert(m(FinalDbName)+'.', Struc, Pos('EXISTS', Struc) + 7 )
              else if DBObj.NodeType = lntView then
                Insert(m(FinalDbName)+'.', Struc, Pos('VIEW', Struc) + 5 );
            end;
          end;

          lntTrigger: begin
            StrucResult := Mainform.Connection.GetResults('SHOW TRIGGERS WHERE `Trigger`='+esc(DBObj.Name));
            Struc := 'SET SESSION SQL_MODE='+esc(StrucResult.Col('sql_mode'))+';'+CRLF+
              'CREATE '+DBObj.ObjType+' '+m(DBObj.Name)+' '+StrucResult.Col('Timing')+' '+StrucResult.Col('Event')+
                ' ON '+m(StrucResult.Col('Table'))+' FOR EACH ROW '+StrucResult.Col('Statement')+';'+CRLF+
              'SET SESSION SQL_MODE=@OLD_SQL_MODE';
          end;

          lntFunction, lntProcedure: begin
            Struc := Mainform.Connection.GetVar('SHOW CREATE '+DBObj.ObjType+' '+m(DBObj.Database)+'.'+m(DBObj.Name), 2);
            MultiSQL := Explode(';', Struc);
            Struc := 'DELIMITER ' + TempDelim + CRLF +
              ImplodeStr(';'+CRLF, MultiSQL) + TempDelim + CRLF +
              'DELIMITER ';
          end;
        end;
        Struc := fixNewlines(Struc);
        Output(Struc, True, True, True, True, True);
      except
        On E:Exception do begin
          // Catch the exception message and dump it into the export file for debugging reasons
          Output('/* '+E.Message+' */', False, True, True, False, False);
          Raise;
        end;
      end;
    end;
  end;

  if DBObj.NodeType = lntTable then begin
    // Table data
    if comboExportData.Text = DATA_NO then begin
      Output(CRLF+'# Data exporting was unselected.'+CRLF, False, True, True, False, False);
    end else if DBObj.Rows = 0 then begin
      Output(CRLF+'# No rows in table '+DBObj.Database+'.'+DBObj.Name+CRLF, False, True, True, False, False);
    end else begin
      Output(CRLF+'# Dumping data for table '+DBObj.Database+'.'+DBObj.Name+': '+FormatNumber(DBObj.Rows)+' rows'+CRLF, False, True, True, False, False);
      TargetDbAndObject := m(DBObj.Name);
      if ToDb then
        TargetDbAndObject := m(FinalDbName) + '.' + TargetDbAndObject;
      Offset := 0;
      RowCount := 0;
      // Calculate limit so we select ~100MB per loop
      Limit := Round(100 * SIZE_MB / Max(DBObj.AvgRowLen,1));
      // Calculate max rows per INSERT, so we always get ~800KB
      MaxRowsInChunk := Round(SIZE_MB * 0.6 / Max(DBObj.AvgRowLen,1));
      if comboExportData.Text = DATA_REPLACE then
        Output('DELETE FROM '+TargetDbAndObject, True, True, True, True, True);
      Output('/*!40000 ALTER TABLE '+TargetDbAndObject+' DISABLE KEYS */', True, True, True, True, True);
      BaseInsert := 'INSERT INTO ';
      if comboExportData.Text = DATA_INSERTNEW then
        BaseInsert := 'INSERT IGNORE INTO '
      else if comboExportData.Text = DATA_UPDATE then
        BaseInsert := 'REPLACE INTO ';
      BaseInsert := BaseInsert + TargetDbAndObject + ' (';
      while true do begin
        Data := Mainform.Connection.GetResults('SELECT * FROM '+m(DBObj.Database)+'.'+m(DBObj.Name)+' LIMIT '+IntToStr(Offset)+', '+IntToStr(Limit));
        Inc(Offset, Limit);
        if Data.RecordCount = 0 then
          break;
        for i:=0 to Data.ColumnCount-1 do
          BaseInsert := BaseInsert + m(Data.ColumnNames[i]) + ', ';
        Delete(BaseInsert, Length(BaseInsert)-1, 2);
        BaseInsert := BaseInsert + ') VALUES (';
        while true do begin
          RowsInChunk := 0;
          Output(BaseInsert, False, True, True, True, True);

          while not Data.Eof do begin
            Inc(RowCount);
            Inc(RowsInChunk);
            Row := '';
            for i:=0 to Data.ColumnCount-1 do begin
              if Data.IsNull(i) then
                Row := Row + 'NULL'
              else case Data.DataType(i).Category of
                dtcInteger, dtcReal: Row := Row + Data.Col(i);
                dtcBinary: Row := Row + '_binary 0x' + BinToWideHex(Data.Col(i));
                else Row := Row + esc(Data.Col(i));
              end;
              if i<Data.ColumnCount-1 then
                Row := Row + ', ';
            end;
            Row := Row + ')';
            Data.Next;
            IsLastRowInChunk := (RowsInChunk = MaxRowsInChunk) or Data.Eof;
            if not IsLastRowInChunk then
              Row := Row + ', (';
            Output(Row, False, True, True, True, True);
            if IsLastRowInChunk then
              break;
          end;
          Output('', True, True, True, True, True);
          LogRow := TWideStringList(FResults.Last);
          LogRow[2] := FormatNumber(RowCount) + ' / ' + FormatNumber(100/Max(DBObj.Rows,1)*RowCount, 0)+'%';
          LogRow[3] := FormatTimeNumber((GetTickCount-StartTime) DIV 1000);
          UpdateResultGrid;
          if Data.Eof then
            break;

        end;
        ResultCount := Data.RecordCount;
        FreeAndNil(Data);
        // Break if end of table data, avoid one last empty/useless SELECT in next loop
        if ResultCount < Limit then
          break;

      end;
      Output('/*!40000 ALTER TABLE '+TargetDbAndObject+' ENABLE KEYS */', True, True, True, True, True);
      Output(EXPORT_FILE_FOOTER, False, False, True, False, False);
    end;
  end;

  ExportLastDatabase := FinalDbName;
end;


procedure TfrmTableTools.chkBulkTableEditCheckComboClick(Sender: TObject);
var
  chk: TCheckBox;
  combo: TWinControl;
begin
  chk := TCheckBox(Sender);
  if chk = chkBulkTableEditDatabase then combo := comboBulkTableEditDatabase
  else if chk = chkBulkTableEditEngine then combo := comboBulkTableEditEngine
  else if chk = chkBulkTableEditCollation then combo := comboBulkTableEditCollation
  else combo := comboBulkTableEditCharset;
  combo.Enabled := chk.Checked;
  ValidateControls(Sender);
end;


procedure TfrmTableTools.DoBulkTableEdit(DBObj: TDBObject);
var
  Specs, LogRow: TWideStringList;
  CreateView: WideString;
  rx: TRegExpr;
begin
  AddResults('SELECT '+esc(DBObj.Database)+' AS '+Mainform.mask('Database')+', ' +
    esc(DBObj.Name)+' AS '+Mainform.mask('Table')+', ' +
    esc('Updating...')+' AS '+Mainform.mask('Operation')+', '+
    ''''' AS '+Mainform.mask('Result')
    );
  Specs := TWideStringlist.Create;
  if chkBulkTableEditDatabase.Checked and (comboBulkTableEditDatabase.Text <> DBObj.Database) then begin
    case DBObj.NodeType of
      lntTable: Specs.Add('RENAME ' + Mainform.mask(comboBulkTableEditDatabase.Text)+'.'+Mainform.mask(DBObj.Name));
      lntView: begin
        // Although RENAME works for views, that does not work for moving to another database without getting
        // a "Changing schema from x to y is not allowed". Instead, recreate them manually
        CreateView := Mainform.Connection.GetVar('SHOW CREATE VIEW '+Mainform.mask(DBObj.Database) + '.' + Mainform.mask(DBObj.Name), 1);
        rx := TRegExpr.Create;
        rx.ModifierI := True;
        // Replace old database references in VIEW body
        rx.Expression := '(["`])'+QuoteRegExprMetaChars(DBObj.Database)+'(["`])';
        CreateView := rx.Replace(CreateView, Mainform.mask(comboBulkTableEditDatabase.Text));
        rx.Free;
        // Temporarily switch to new database for VIEW creation, so the database references are correct
        Mainform.Connection.Database := comboBulkTableEditDatabase.Text;
        Mainform.Connection.Query(CreateView);
        Mainform.Connection.Database := DBObj.Database;
        Mainform.Connection.Query('DROP VIEW '+Mainform.mask(DBObj.Name));
      end;
    end;
    FModifiedDbs.Add(DBObj.Database);
    FModifiedDbs.Add(comboBulkTableEditDatabase.Text);
  end;
  if (DBObj.NodeType = lntTable) and chkBulkTableEditEngine.Checked then begin
    if Mainform.Connection.ServerVersionInt < 40018 then
      Specs.Add('TYPE '+comboBulkTableEditEngine.Text)
    else
      Specs.Add('ENGINE '+comboBulkTableEditEngine.Text);
  end;
  if (DBObj.NodeType = lntTable) and chkBulkTableEditCollation.Checked and (comboBulkTableEditCollation.ItemIndex > -1) then
    Specs.Add('COLLATE '+comboBulkTableEditCollation.Text);
  if (DBObj.NodeType = lntTable) and chkBulkTableEditCharset.Checked and (comboBulkTableEditCharset.ItemIndex > -1) then begin
    Mainform.Connection.CharsetTable.RecNo := comboBulkTableEditCharset.ItemIndex;
    Specs.Add('CONVERT TO CHARSET '+Mainform.Connection.CharsetTable.Col('Charset'));
  end;
  if (DBObj.NodeType = lntTable) and chkBulkTableEditResetAutoinc.Checked then
    Specs.Add('AUTO_INCREMENT=0');

  LogRow := TWideStringList(FResults.Last);
  if Specs.Count > 0 then begin
    Mainform.Connection.Query('ALTER TABLE ' + Mainform.mask(DBObj.Database) + '.' + Mainform.mask(DBObj.Name) + ' ' + ImplodeStr(', ', Specs));
    LogRow[2] := 'Done';
    LogRow[3] := 'Success';
  end else begin
    LogRow[2] := 'Nothing to do';
    LogRow[3] := 'Selected operations cannot be applied to a '+LowerCase(DBObj.ObjType);
  end;
  UpdateResultGrid;
end;


end.
