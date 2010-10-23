unit tabletools;


// -------------------------------------
// Table-diagnostics
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls, Buttons, Dialogs, StdActns,
  VirtualTrees, ExtCtrls, Contnrs, Graphics, SynRegExpr, Math, Generics.Collections,
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
    memoFindText: TMemo;
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
    comboExportOutputTarget: TComboBox;
    lblExportDatabases: TLabel;
    lblExportTables: TLabel;
    lblExportOutputTarget: TLabel;
    btnExecute: TButton;
    btnExportOutputTargetSelect: TButton;
    tabBulkTableEdit: TTabSheet;
    chkBulkTableEditDatabase: TCheckBox;
    comboBulkTableEditDatabase: TComboBox;
    chkBulkTableEditResetAutoinc: TCheckBox;
    chkBulkTableEditCollation: TCheckBox;
    comboBulkTableEditCollation: TComboBox;
    chkBulkTableEditEngine: TCheckBox;
    comboBulkTableEditEngine: TComboBox;
    chkBulkTableEditCharset: TCheckBox;
    comboBulkTableEditCharset: TComboBox;
    btnSeeResults: TButton;
    chkCaseSensitive: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure TreeObjectsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
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
      TextType: TVSTTextType; var CellText: String);
    procedure TreeObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ResultGridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure ResultGridCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure ResultGridPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure ValidateControls(Sender: TObject);
    procedure chkExportOptionClick(Sender: TObject);
    procedure btnExportOutputTargetSelectClick(Sender: TObject);
    procedure comboExportOutputTargetChange(Sender: TObject);
    procedure comboExportOutputTargetExit(Sender: TObject);
    procedure comboExportOutputTypeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeObjectsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure chkBulkTableEditCheckComboClick(Sender: TObject);
    procedure TreeObjectsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeObjectsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
      var Allowed: Boolean);
    procedure btnSeeResultsClick(Sender: TObject);
    procedure TreeObjectsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
  private
    { Private declarations }
    FResults: TObjectList<TStringList>;
    FToolMode: TToolMode;
    FSecondExportPass: Boolean; // Set to True after everything is exported and final VIEWs need to be exported again
    OutputFiles, OutputDirs: TStringList;
    ExportStream: TStream;
    ExportStreamStartOfQueryPos: Int64;
    ExportLastDatabase: String;
    FTargetConnection: TMySQLConnection;
    FLastOutputSelectedIndex: Integer;
    FModifiedDbs: TStringList;
    FHeaderCreated: Boolean;
    FFindSeeResultSQL: TStringList;
    ToFile, ToDir, ToClipboard, ToDb, ToServer: Boolean;
    procedure SetToolMode(Value: TToolMode);
    procedure Output(SQL: String; IsEndOfQuery, ForFile, ForDir, ForDb, ForServer: Boolean);
    procedure AddResults(SQL: String);
    procedure AddNotes(Col1, Col2, Col3, Col4: String);
    procedure SetupResultGrid(Results: TMySQLQuery=nil);
    procedure UpdateResultGrid;
    procedure DoMaintenance(DBObj: TDBObject);
    procedure DoFind(DBObj: TDBObject);
    procedure DoExport(DBObj: TDBObject);
    procedure DoBulkTableEdit(DBObj: TDBObject);
  public
    { Public declarations }
    PreSelectObjects: TDBObjectList;
    property ToolMode: TToolMode read FToolMode write SetToolMode;
  end;


implementation

uses main, mysql_structures;

const
  STRSKIPPED: String = 'Skipped - ';
  OUTPUT_FILE = 'One big file';
  OUTPUT_CLIPBOARD = 'Clipboard';
  OUTPUT_DIR = 'Directory - one file per object in database subdirectories';
  OUTPUT_DB = 'Database';
  OUTPUT_SERVER = 'Server: ';
  DATA_NO = 'No data';
  DATA_REPLACE = 'DELETE + INSERT (truncate existing data)';
  DATA_INSERT = 'INSERT';
  DATA_INSERTNEW = 'INSERT IGNORE (do not update existing)';
  DATA_UPDATE = 'REPLACE existing data';
  EXPORT_FILE_FOOTER = '/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;'+CRLF+
    '/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;'+CRLF+
    '/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;'+CRLF;

{$R *.DFM}


procedure TfrmTableTools.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Restore GUI setup
  InheritFont(Font);
  Width := GetRegValue(REGNAME_TOOLSWINWIDTH, Width);
  Height := GetRegValue(REGNAME_TOOLSWINHEIGHT, Height);
  TreeObjects.Width := GetRegValue(REGNAME_TOOLSTREEWIDTH, TreeObjects.Width);

  // Find text tab
  memoFindText.Text := GetRegValue(REGNAME_TOOLSFINDTEXT, '');
  comboDatatypes.Items.Add('All data types');
  for i:=Low(DatatypeCategories) to High(DatatypeCategories) do
    comboDatatypes.Items.Add(DatatypeCategories[i].Name);
  comboDatatypes.ItemIndex := GetRegValue(REGNAME_TOOLSDATATYPE, 0);
  chkCaseSensitive.Checked := GetRegValue(REGNAME_TOOLSCASESENSITIVE, chkCaseSensitive.Checked);

  // SQL export tab
  chkExportDatabasesCreate.Checked := GetRegValue(REGNAME_EXP_CREATEDB, chkExportDatabasesCreate.Checked);
  chkExportDatabasesDrop.Checked := GetRegValue(REGNAME_EXP_DROPDB, chkExportDatabasesDrop.Checked);
  chkExportTablesCreate.Checked := GetRegValue(REGNAME_EXP_CREATETABLE, chkExportTablesCreate.Checked);
  chkExportTablesDrop.Checked := GetRegValue(REGNAME_EXP_DROPTABLE, chkExportTablesDrop.Checked);
  comboExportData.Items.Text := DATA_NO+CRLF +DATA_REPLACE+CRLF +DATA_INSERT+CRLF +DATA_INSERTNEW+CRLF +DATA_UPDATE;
  comboExportData.ItemIndex := GetRegValue(REGNAME_EXP_DATAHOW, 0);
  OutputFiles := TStringList.Create;
  OutputDirs := TStringList.Create;
  OutputFiles.Text := GetRegValue(REGNAME_EXP_OUTFILES, '');
  OutputDirs.Text := GetRegValue(REGNAME_EXP_OUTDIRS, '');
  comboExportOutputType.Items.Text := OUTPUT_FILE+CRLF +OUTPUT_DIR+CRLF +OUTPUT_CLIPBOARD+CRLF +OUTPUT_DB;
  comboExportOutputTarget.Text := '';

  // Various
  SetWindowSizeGrip( Self.Handle, True );
  FixVT(TreeObjects);
  FixVT(ResultGrid);
  FResults := TObjectList<TStringList>.Create;
  PreSelectObjects := TDBObjectList.Create(False);
  FModifiedDbs := TStringList.Create;
  FModifiedDbs.Duplicates := dupIgnore;
  FFindSeeResultSQL := TStringList.Create;
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

  MainReg.WriteString( REGNAME_TOOLSFINDTEXT, memoFindText.Text);
  MainReg.WriteInteger( REGNAME_TOOLSDATATYPE, comboDatatypes.ItemIndex);
  MainReg.WriteBool(REGNAME_TOOLSCASESENSITIVE, chkCaseSensitive.Checked);

  MainReg.WriteBool(REGNAME_EXP_CREATEDB, chkExportDatabasesCreate.Checked);
  MainReg.WriteBool(REGNAME_EXP_DROPDB, chkExportDatabasesDrop.Checked);
  MainReg.WriteBool(REGNAME_EXP_CREATETABLE, chkExportTablesCreate.Checked);
  MainReg.WriteBool(REGNAME_EXP_DROPTABLE, chkExportTablesDrop.Checked);
  MainReg.WriteInteger(REGNAME_EXP_DATAHOW, comboExportData.ItemIndex);
  // Do not remember a selected session name for the next time
  OutputItem := comboExportOutputType.ItemIndex;
  if OutputItem > 3 then
    OutputItem := 0;
  MainReg.WriteInteger(REGNAME_EXP_OUTPUT, OutputItem);
  MainReg.WriteString(REGNAME_EXP_OUTFILES, OutputFiles.Text);
  MainReg.WriteString(REGNAME_EXP_OUTDIRS, OutputDirs.Text);
end;


procedure TfrmTableTools.FormShow(Sender: TObject);
var
  Node, FirstChecked: PVirtualNode;
  idx, i: Integer;
  SessionNames: TStringList;
  DBObj: TDBObject;
begin
  // When this form is displayed the second time, databases may be deleted or filtered.
  // Also, checked nodes must be unchecked and unchecked nodes may need to be checked.
  TreeObjects.Clear;
  TreeObjects.RootNodeCount := Mainform.DBtree.RootNodeCount;

  for DBObj in PreSelectObjects do begin
    Node := MainForm.FindDBObjectNode(TreeObjects, DBObj);
    if Assigned(Node) then
      TreeObjects.CheckState[Node] := csCheckedNormal;
  end;

  FirstChecked := TreeObjects.GetFirstChecked;
  if Assigned(FirstChecked) then
    TreeObjects.ScrollIntoView(FirstChecked, True);
  // CHECKSUM available since MySQL 4.1.1
  idx := comboOperation.ItemIndex;
  if idx = -1 then idx := 0;
  comboOperation.Items.CommaText := 'Check,Analyze,Checksum,Optimize,Repair';
  if Mainform.ActiveConnection.ServerVersionInt < 40101 then
    comboOperation.Items.Text := StringReplace(comboOperation.Items.Text, 'Checksum', 'Checksum ('+SUnsupported+')', [rfReplaceAll]);
  comboOperation.ItemIndex := idx;
  comboOperation.OnChange(Sender);

  // Add session names from registry
  idx := comboExportOutputType.ItemIndex;
  for i:=comboExportOutputType.Items.Count-1 downto 0 do begin
    if Pos(OUTPUT_SERVER, comboExportOutputType.Items[i]) = 1 then
      comboExportOutputType.Items.Delete(i);
  end;
  SessionNames := TStringList.Create;
  MainReg.OpenKey(RegPath + REGKEY_SESSIONS, True);
  MainReg.GetKeyNames(SessionNames);
  for i:=0 to SessionNames.Count-1 do begin
    if SessionNames[i] <> Mainform.ActiveConnection.SessionName then
      comboExportOutputType.Items.Add(OUTPUT_SERVER+SessionNames[i]);
  end;
  if (idx > -1) and (idx < comboExportOutputType.Items.Count) then
    comboExportOutputType.ItemIndex := idx
  else
    comboExportOutputType.ItemIndex := GetRegValue(REGNAME_EXP_OUTPUT, 0);
  comboExportOutputType.OnChange(Sender);

  comboBulkTableEditDatabase.Items.Text := Mainform.ActiveConnection.AllDatabases.Text;
  if comboBulkTableEditDatabase.Items.Count > 0 then
    comboBulkTableEditDatabase.ItemIndex := 0;

  comboBulkTableEditEngine.Items := MainForm.ActiveConnection.TableEngines;
  if comboBulkTableEditEngine.Items.Count > 0 then
    comboBulkTableEditEngine.ItemIndex := comboBulkTableEditEngine.Items.IndexOf(MainForm.ActiveConnection.TableEngineDefault);

  comboBulkTableEditCollation.Items := MainForm.ActiveConnection.CollationList;
  if comboBulkTableEditCollation.Items.Count > 0 then
    comboBulkTableEditCollation.ItemIndex := 0;

  comboBulkTableEditCharset.Items := MainForm.ActiveConnection.CharsetList;
  if comboBulkTableEditCharset.Items.Count > 0 then
    comboBulkTableEditCharset.ItemIndex := 0;

  ValidateControls(Sender);
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
  i: Integer;
begin
  SomeChecked := TreeObjects.CheckedCount > 0;
  btnSeeResults.Visible := tabsTools.ActivePage = tabFind;
  if tabsTools.ActivePage = tabMaintenance then begin
    btnExecute.Caption := 'Execute';
    btnExecute.Enabled := (Pos(SUnsupported, comboOperation.Text) = 0) and SomeChecked;
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
    // Enable "See results" button if there were results
    btnSeeResults.Enabled := False;
    if Assigned(FResults) then for i:=0 to FResults.Count-1 do begin
      if MakeInt(FResults[i][2]) > 0 then begin
        btnSeeResults.Enabled := True;
        break;
      end;
    end;
  end else if tabsTools.ActivePage = tabSQLExport then begin
    btnExecute.Caption := 'Export';
    btnExecute.Enabled := SomeChecked and ((comboExportOutputTarget.Text <> '') or (not comboExportOutputTarget.Enabled));
  end else if tabsTools.ActivePage = tabBulkTableEdit then begin
    btnExecute.Caption := 'Update';
    chkBulkTableEditCollation.Enabled := MainForm.ActiveConnection.IsUnicode;
    chkBulkTableEditCharset.Enabled := MainForm.ActiveConnection.IsUnicode;
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


procedure TfrmTableTools.TreeObjectsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  MainForm.DBtreeGetNodeDataSize(Sender, NodeDataSize);
end;

procedure TfrmTableTools.TreeObjectsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
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
  SessionNode, DBNode, TableNode: PVirtualNode;
  Triggers, Views: TDBObjectList;
  DBObj: PDBObject;
  i: Integer;

  procedure ProcessNode(DBObj: TDBObject);
  begin
    try
      case FToolMode of
        tmMaintenance: DoMaintenance(DBObj);
        tmFind: DoFind(DBObj);
        tmSQLExport: DoExport(DBObj);
        tmBulkTableEdit: DoBulkTableEdit(DBObj);
      end;
    except on E:EDatabaseError do
      // The above SQL can easily throw an exception, e.g. if a table is corrupted.
      // In such cases we create a dummy row, including the error message
      AddNotes(DBObj.Database, DBObj.Name, 'error', E.Message)
    end;
  end;

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
  FFindSeeResultSQL.Clear;
  Triggers := TDBObjectList.Create(False); // False, so we can .Free that object afterwards without loosing the contained objects
  Views := TDBObjectList.Create(False);
  TreeObjects.SetFocus;
  FHeaderCreated := False;
  SessionNode := TreeObjects.GetFirstChild(nil);
  while Assigned(SessionNode) do begin
    DBNode := TreeObjects.GetFirstChild(SessionNode);
    while Assigned(DBNode) do begin
      if not (DBNode.CheckState in [csUncheckedNormal, csUncheckedPressed]) then begin
        Triggers.Clear;
        Views.Clear;
        FSecondExportPass := False;
        TableNode := TreeObjects.GetFirstChild(DBNode);
        while Assigned(TableNode) do begin
          if (csCheckedNormal in [TableNode.CheckState, DBNode.CheckState]) and (TableNode.CheckType <> ctNone) then begin
            DBObj := TreeObjects.GetNodeData(TableNode);
            // Triggers have to be exported at the very end
            if (FToolMode = tmSQLExport) and (DBObj.NodeType = lntTrigger) then
              Triggers.Add(DBObj^)
            else begin
              if (FToolMode = tmSQLExport) and (DBObj.NodeType = lntView) then
                Views.Add(DBObj^);
              ProcessNode(DBObj^);
            end;
          end;
          TableNode := TreeObjects.GetNextSibling(TableNode);
        end; // End of db object node loop in db

        // Special block for late created triggers in export mode
        for i:=0 to Triggers.Count-1 do
          ProcessNode(Triggers[i]);

        // Special block for final exporting final view structure
        FSecondExportPass := True;
        for i:=0 to Views.Count-1 do
          ProcessNode(Views[i]);

      end;
      DBNode := TreeObjects.GetNextSibling(DBNode);
    end; // End of db item loop
    SessionNode := TreeObjects.GetNextSibling(SessionNode);
  end;

  if Assigned(ExportStream) then begin
    Output(EXPORT_FILE_FOOTER, False, True, False, False, False);
    Output('/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */', True, False, False, True, True);
    if comboExportOutputType.Text = OUTPUT_CLIPBOARD then
      StreamToClipboard(ExportStream, nil, false);
    FreeAndNil(ExportStream);
  end;
  ExportLastDatabase := '';

  for i:=0 to FModifiedDbs.Count-1 do
    Mainform.ActiveConnection.ClearDbObjects(FModifiedDbs[i]);
  FModifiedDbs.Clear;

  ValidateControls(Sender);
  Screen.Cursor := crDefault;
end;


procedure TfrmTableTools.DoMaintenance(DBObj: TDBObject);
var
  SQL: String;
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
  Columns: TTableColumnList;
  Col: TTableColumn;
  SQL, ResultSQL, Dummy: String;
begin
  ResultSQL := '';
  Columns := TTableColumnList.Create(True);
  case DBObj.NodeType of
    lntTable: DBObj.Connection.ParseTableStructure(DBObj.CreateCode, Columns, nil, nil);
    lntView: DBObj.Connection.ParseViewStructure(DBObj.CreateCode, DBObj.Name, Columns, Dummy, Dummy, Dummy, Dummy);
    else AddNotes(DBObj.Database, DBObj.Name, STRSKIPPED+'a '+LowerCase(DBObj.ObjType)+' does not contain rows.', '');
  end;
  if Columns.Count > 0 then begin
    SQL := '';
    for Col in Columns do begin
      if (comboDatatypes.ItemIndex = 0) or (Integer(Col.DataType.Category) = comboDatatypes.ItemIndex-1) then begin
        if chkCaseSensitive.Checked then
          SQL := SQL + Mainform.mask(Col.Name) + ' LIKE BINARY ' + esc('%'+memoFindText.Text+'%') + ' OR '
        else
          SQL := SQL + 'LOWER(CONVERT('+Mainform.mask(Col.Name)+' USING '+DBObj.Connection.CharacterSet+')) LIKE ' + esc('%'+LowerCase(memoFindText.Text)+'%') + ' OR '
      end;
    end;
    if SQL <> '' then begin
      Delete(SQL, Length(SQL)-3, 3);
      ResultSQL := 'SELECT * FROM '+Mainform.mask(DBObj.Database)+'.'+Mainform.mask(DBObj.Name)+' WHERE ' + SQL;
      SQL := 'SELECT '''+DBObj.Database+''' AS `Database`, '''+DBObj.Name+''' AS `Table`, COUNT(*) AS `Found rows`, '
        + 'CONCAT(ROUND(100 / '+IntToStr(Max(DBObj.Rows,1))+' * COUNT(*), 1), ''%'') AS `Relevance` FROM '+Mainform.mask(DBObj.Database)+'.'+Mainform.mask(DBObj.Name)+' WHERE '
        + SQL;
      AddResults(SQL);
    end else
      AddNotes(DBObj.Database, DBObj.Name, STRSKIPPED+DBObj.ObjType+' doesn''t have columns of selected type ('+comboDatatypes.Text+').', '');
  end;
  Columns.Free;
  FFindSeeResultSQL.Add(ResultSQL);
end;


procedure TfrmTableTools.btnSeeResultsClick(Sender: TObject);
var
  SQL: String;
  i: Integer;
  Tab: TQueryTab;
begin
  // "See results" clicked - auto create new query tab, and execute a batch of SELECT queries
  SQL := '';
  for i:=0 to FResults.Count-1 do begin
    if MakeInt(FResults[i][2]) > 0 then begin
      SQL := SQL + FFindSeeResultSQL[i] + ';' + CRLF;
    end;
  end;
  MainForm.actNewQueryTab.Execute;
  Tab := MainForm.QueryTabs[MainForm.QueryTabs.Count-1];
  Tab.Memo.Text := SQL;
  Tab.TabSheet.Show;
  MainForm.actExecuteQueryExecute(Sender);
end;


procedure TfrmTableTools.AddResults(SQL: String);
var
  i: Integer;
  Row: TStringList;
  Results: TMySQLQuery;
begin
  // Execute query and append results into grid
  Results := MainForm.ActiveConnection.GetResults(SQL);
  if Results = nil then
    Exit;

  SetupResultGrid(Results);
  Results.First;
  while not Results.Eof do begin
    Row := TStringList.Create;
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


procedure TfrmTableTools.AddNotes(Col1, Col2, Col3, Col4: String);
var
  Row: TStringList;
begin
  // Adds a row with non SQL results
  SetupResultGrid;
  Row := TStringList.Create;
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
    // Remove column headers if this is the first row
    if FResults.Count = 0 then
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
  NodeDataSize := SizeOf(TStringList);
end;


procedure TfrmTableTools.ResultGridInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: ^TStringList;
begin
  // Bind string list to node
  Data := Sender.GetNodeData(Node);
  Data^ := FResults[Node.Index];
end;


procedure TfrmTableTools.ResultGridPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  VT: TVirtualStringTree;
  Msg: String;
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
  TextType: TVSTTextType; var CellText: String);
var
  Data: ^TStringList;
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
  OldItem: String;
  NewIdx: Integer;
  DBNode: PVirtualNode;
  SessionName: String;
begin
  // Target type (file, directory, ...) selected
  OldItem := comboExportOutputTarget.Text;
  comboExportOutputTarget.Enabled := True;
  if Assigned(FTargetConnection) then
    FreeAndNil(FTargetConnection);
  if comboExportOutputType.Text = OUTPUT_FILE then begin
    comboExportOutputTarget.Style := csDropDown;
    comboExportOutputTarget.Items.Text := OutputFiles.Text;
    lblExportOutputTarget.Caption := 'Filename:';
    btnExportOutputTargetSelect.Enabled := True;
    btnExportOutputTargetSelect.ImageIndex := 10;
  end else if comboExportOutputType.Text = OUTPUT_DIR then begin
    comboExportOutputTarget.Style := csDropDown;
    comboExportOutputTarget.Items.Text := OutputDirs.Text;
    lblExportOutputTarget.Caption := 'Directory:';
    btnExportOutputTargetSelect.Enabled := True;
    btnExportOutputTargetSelect.ImageIndex := 51;
  end else if comboExportOutputType.Text = OUTPUT_CLIPBOARD then begin
    comboExportOutputTarget.Enabled := False;
    comboExportOutputTarget.Items.Clear;
    lblExportOutputTarget.Caption := '';
    btnExportOutputTargetSelect.Enabled := False;
    btnExportOutputTargetSelect.ImageIndex := 4;
  end else if comboExportOutputType.Text = OUTPUT_DB then begin
    comboExportOutputTarget.Style := csDropDownList;
    lblExportOutputTarget.Caption := 'Database:';
    btnExportOutputTargetSelect.Enabled := False;
    btnExportOutputTargetSelect.ImageIndex := 27;
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
    btnExportOutputTargetSelect.ImageIndex := 27;
    SessionName := Copy(comboExportOutputType.Text, Length(OUTPUT_SERVER)+1, Length(comboExportOutputType.Text));
    FreeAndNil(FTargetConnection);
    FTargetConnection := TMySQLConnection.Create(Self);
    FTargetConnection.Parameters := LoadConnectionParams(SessionName);
    FTargetConnection.LogPrefix := '['+SessionName+'] ';
    FTargetConnection.OnLog := Mainform.LogSQL;
    Screen.Cursor := crHourglass;
    try
      FTargetConnection.Active := True;
      comboExportOutputTarget.Items := FTargetConnection.GetCol('SHOW DATABASES');
      comboExportOutputTarget.Items.Insert(0, '[Same as on source server]');
      comboExportOutputTarget.ItemIndex := 0;
      Screen.Cursor := crDefault;
    except
      on E:EDatabaseError do begin
        Screen.Cursor := crDefault;
        MessageDlg(E.Message, mtError, [mbOK], 0);
        comboExportOutputType.ItemIndex := FLastOutputSelectedIndex;
        comboExportOutputType.OnChange(Sender);
      end;
    end;
  end;

  FLastOutputSelectedIndex := comboExportOutputType.ItemIndex;
  chkExportDatabasesCreate.Enabled := (comboExportOutputType.Text = OUTPUT_FILE)
    or (comboExportOutputType.Text = OUTPUT_CLIPBOARD)
    or (Copy(comboExportOutputType.Text, 1, Length(OUTPUT_SERVER)) = OUTPUT_SERVER);
  chkExportDatabasesDrop.Enabled := chkExportDatabasesCreate.Enabled;
  NewIdx := comboExportOutputTarget.Items.IndexOf(OldItem);
  if (NewIdx = -1) and (comboExportOutputTarget.Items.Count > 0) then
    NewIdx := 0;
  comboExportOutputTarget.ItemIndex := NewIdx;
  ValidateControls(Sender);
end;


procedure TfrmTableTools.comboExportOutputTargetChange(Sender: TObject);
begin
  ValidateControls(Sender);
end;


procedure TfrmTableTools.comboExportOutputTargetExit(Sender: TObject);
var
  ItemList: TStringList;
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
  ValidateControls(Sender);
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


// Pass output to file or query, and append semicolon if needed
procedure TfrmTableTools.Output(SQL: String; IsEndOfQuery, ForFile, ForDir, ForDb, ForServer: Boolean);
var
  SA: AnsiString;
  ChunkSize: Integer;
begin
  if (ToFile and ForFile) or (ToDir and ForDir) or (ToClipboard and ForFile) then begin
    if IsEndOfQuery then
      SQL := SQL + ';'+CRLF;
    StreamWrite(ExportStream, SQL);
    if IsEndOfQuery then
      ExportStreamStartOfQueryPos := ExportStream.Size;
  end;
  if (ToDb and ForDb) or (ToServer and ForServer) then begin
    StreamWrite(ExportStream, SQL);
    if IsEndOfQuery then begin
      ExportStream.Position := 0;
      ChunkSize := ExportStream.Size;
      SetLength(SA, ChunkSize div SizeOf(AnsiChar));
      ExportStream.Read(PAnsiChar(SA)^, ChunkSize);
      ExportStream.Size := 0;
      ExportStreamStartOfQueryPos := 0;
      SQL := UTF8ToString(SA);
      if ToDB then MainForm.ActiveConnection.Query(SQL)
      else if ToServer then FTargetConnection.Query(SQL);
      SQL := '';
    end;
  end;
end;


procedure TfrmTableTools.DoExport(DBObj: TDBObject);
var
  IsFirstRowInChunk, NeedsDBStructure: Boolean;
  Struc, Header, DbDir, FinalDbName, BaseInsert, Row, TargetDbAndObject, BinContent, tmp, Dummy: String;
  i: Integer;
  RowCount, Limit, Offset, ResultCount: Int64;
  StartTime: Cardinal;
  StrucResult, Data: TMySQLQuery;
  rx: TRegExpr;
  ColumnList: TTableColumnList;
  Column: TTableColumn;
const
  TempDelim = '//';

  // Short version of Mainform.Mask()
  function m(s: String): String;
  begin
    Result := Mainform.mask(s);
  end;

  procedure LogStatistic(RowsDone: Int64);
  var
    LogRow: TStringlist;
    Percent: Double;
  begin
    LogRow := FResults.Last;
    Percent := 100 / Max(DBObj.Rows,1) * RowsDone;
    LogRow[2] := FormatNumber(RowsDone) + ' / ' + FormatNumber(Percent, 0)+'%';
    LogRow[3] := FormatTimeNumber((GetTickCount-StartTime) DIV 1000);
    UpdateResultGrid;
  end;

begin
  // Handle one table, view or whatever in SQL export mode
  AddResults('SELECT '+esc(DBObj.Database)+' AS '+Mainform.mask('Database')+', ' +
    esc(DBObj.Name)+' AS '+Mainform.mask('Table')+', ' +
    IntToStr(DBObj.Rows)+' AS '+Mainform.mask('Rows')+', '+
    '0 AS '+Mainform.mask('Duration')
    );
  ToFile := comboExportOutputType.Text = OUTPUT_FILE;
  ToDir := comboExportOutputType.Text = OUTPUT_DIR;
  ToClipboard := comboExportOutputType.Text = OUTPUT_CLIPBOARD;
  ToDb := comboExportOutputType.Text = OUTPUT_DB;
  ToServer := Copy(comboExportOutputType.Text, 1, Length(OUTPUT_SERVER)) = OUTPUT_SERVER;
  StartTime := GetTickCount;
  ExportStreamStartOfQueryPos := 0;
  if ToDir then begin
    FreeAndNil(ExportStream);
    DbDir := comboExportOutputTarget.Text;
    if DbDir[Length(DbDir)] <> '\' then
      DbDir := DbDir + '\';
    DbDir := DbDir + DBObj.Database + '\';
    if not DirectoryExists(DbDir) then
      ForceDirectories(DbDir);
    ExportStream := TFileStream.Create(DbDir + DBObj.Name+'.sql', fmCreate or fmOpenWrite);
    FHeaderCreated := False;
  end;
  if ToFile and (not Assigned(ExportStream)) then
    ExportStream := TFileStream.Create(comboExportOutputTarget.Text, fmCreate or fmOpenWrite);
  if ToClipboard and (not Assigned(ExportStream)) then
    ExportStream := TMemoryStream.Create;
  if ToDb or ToServer then
    ExportStream := TMemoryStream.Create;
  if not FHeaderCreated then begin
    Header := '# --------------------------------------------------------' + CRLF +
      Format('# %-30s%s', ['Host:', MainForm.ActiveConnection.Parameters.HostName]) + CRLF +
      Format('# %-30s%s', ['Server version:', MainForm.ActiveConnection.ServerVersionUntouched]) + CRLF +
      Format('# %-30s%s', ['Server OS:', MainForm.ActiveConnection.ServerOS]) + CRLF +
      Format('# %-30s%s', [APPNAME + ' version:', Mainform.AppVersion]) + CRLF +
      Format('# %-30s%s', ['Date/time:', DateTimeToStr(Now)]) + CRLF +
      '# --------------------------------------------------------' + CRLF + CRLF +
      '/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;' + CRLF +
      '/*!40101 SET NAMES '+MainForm.ActiveConnection.CharacterSet+' */;' + CRLF +
      '/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;' + CRLF +
      '/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE=''NO_AUTO_VALUE_ON_ZERO'' */;';
    Output(Header, False, DBObj.Database<>ExportLastDatabase, True, False, False);
    Output('/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */', True, False, False, True, True);
    FHeaderCreated := True;
  end;

  // Database structure. Do that only in single-file and server mode. drop/create/use in directory or database mode makes no sense
  FinalDbName := DBObj.Database;
  if ToDb or (ToServer and (comboExportOutputTarget.ItemIndex > 0)) then
    FinalDbName := comboExportOutputTarget.Text;
  NeedsDBStructure := FinalDbName <> ExportLastDatabase;
  if chkExportDatabasesDrop.Checked or chkExportDatabasesCreate.Checked then begin
    Output(CRLF+CRLF+'# Dumping database structure for '+DBObj.Database+CRLF, False, NeedsDBStructure, False, False, False);
    if chkExportDatabasesDrop.Checked and chkExportDatabasesDrop.Enabled then
      Output('DROP DATABASE IF EXISTS '+m(FinalDbName), True, NeedsDBStructure, False, False, NeedsDBStructure);
    if chkExportDatabasesCreate.Checked and chkExportDatabasesCreate.Enabled then begin
      if MainForm.ActiveConnection.ServerVersionInt >= 40100 then begin
        Struc := MainForm.ActiveConnection.GetVar('SHOW CREATE DATABASE '+m(DBObj.Database), 1);
        // Gracefully ignore it when target database exists, important in server mode
        Insert('IF NOT EXISTS ', Struc, Pos('DATABASE', Struc) + 9);
        // Create the right dbname
        Struc := StringReplace(Struc, DBObj.Database, FinalDbName, []);
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
      Struc := 'DROP '+UpperCase(DBObj.ObjType)+' IF EXISTS ';
      if ToDb then
        Struc := Struc + m(FinalDbName)+'.';
      Struc := Struc + m(DBObj.Name);
      Output(Struc, True, True, True, True, True);
    end;
    if chkExportTablesCreate.Checked then begin
      try
        case DBObj.NodeType of
          lntTable: begin
            Struc := DBObj.CreateCode;
            // Remove AUTO_INCREMENT clause if no data gets exported
            if comboExportData.Text = DATA_NO then begin
              rx := TRegExpr.Create;
              rx.ModifierI := True;
              rx.Expression := '\sAUTO_INCREMENT\s*\=\s*\d+\s';
              Struc := rx.Replace(Struc, ' ', false);
              rx.Free;
            end;
            Insert('IF NOT EXISTS ', Struc, Pos('TABLE', Struc) + 6);
            if ToDb then
              Insert(m(FinalDbName)+'.', Struc, Pos('EXISTS', Struc) + 7 )
          end;

          lntView: begin
            if not FSecondExportPass then begin
              // Create temporary VIEW replacement
              ColumnList := TTableColumnList.Create(True);
              DBObj.Connection.ParseViewStructure(DBObj.CreateCode, DBObj.Name, ColumnList, Dummy, Dummy, Dummy, Dummy);
              Struc := '# Creating temporary table to overcome VIEW dependency errors'+CRLF+
                'CREATE TABLE ';
              if ToDb then
                Struc := Struc + m(FinalDbName) + '.';
              Struc := Struc + m(DBObj.Name)+' (';
              for Column in ColumnList do
                Struc := Struc + CRLF + #9 + Column.SQLCode + ',';
              Delete(Struc, Length(Struc), 1);
              Struc := Struc + CRLF + ') ENGINE=MyISAM';
              ColumnList.Free;
            end else begin
              Struc := '# Removing temporary table and create final VIEW structure'+CRLF+
                'DROP TABLE IF EXISTS ';
              if ToDb then
                Struc := Struc + m(FinalDbName)+'.';
              Struc := Struc + m(DBObj.Name);
              Output(Struc, True, True, True, True, True);
              Struc := DBObj.CreateCode;
              if ToDb then
                Insert(m(FinalDbName)+'.', Struc, Pos('VIEW', Struc) + 5 );
            end;
          end;

          lntTrigger: begin
            StrucResult := MainForm.ActiveConnection.GetResults('SHOW TRIGGERS FROM '+m(DBObj.Database)+' WHERE `Trigger`='+esc(DBObj.Name));
            Struc := 'CREATE '+UpperCase(DBObj.ObjType)+' '+m(DBObj.Name)+' '+StrucResult.Col('Timing')+' '+StrucResult.Col('Event')+
                ' ON '+m(StrucResult.Col('Table'))+' FOR EACH ROW '+StrucResult.Col('Statement');
            if ToDb then
              Insert(m(FinalDbName)+'.', Struc, Pos('TRIGGER', Struc) + 8 );
            if ToFile or ToClipboard or ToDir then begin
              Struc := 'SET SESSION SQL_MODE=' + esc(StrucResult.Col('sql_mode')) + ';' + CRLF +
                'DELIMITER ' + TempDelim + CRLF +
                Struc + TempDelim + CRLF +
                'DELIMITER ;' + CRLF +
                'SET SESSION SQL_MODE=@OLD_SQL_MODE';
            end;
          end;

          lntFunction, lntProcedure: begin
            Struc := DBObj.CreateCode;
            if ToDb then begin
              if DBObj.NodeType = lntProcedure then
                Insert(m(FinalDbName)+'.', Struc, Pos('PROCEDURE', Struc) + 10 )
              else if DBObj.NodeType = lntFunction then
                Insert(m(FinalDbName)+'.', Struc, Pos('FUNCTION', Struc) + 9 );
            end;
            // Change delimiter for file output, so readers split queries at the right string position
            if ToFile or ToDir or ToClipboard then
              Struc := 'DELIMITER ' + TempDelim + CRLF + Struc + TempDelim + CRLF + 'DELIMITER ';
          end;

          lntEvent: begin
            Struc := DBObj.CreateCode;
            if ToDb then
              Insert(m(FinalDbName)+'.', Struc, Pos('EVENT', Struc) + 6 );
            if ToFile or ToDir or ToClipboard then
              Struc := 'DELIMITER ' + TempDelim + CRLF + Struc + TempDelim + CRLF + 'DELIMITER ';
          end;
        end;
        Struc := fixNewlines(Struc);
        Output(Struc, True, True, True, True, True);
      except
        on E:EDatabaseError do begin
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
    end else begin
      tmp := FormatNumber(DBObj.Rows)+' rows';
      if LowerCase(DBObj.Engine) = 'innodb' then
        tmp := '~'+tmp+' (approximately)';
      Output(CRLF+'# Dumping data for table '+DBObj.Database+'.'+DBObj.Name+': '+tmp+CRLF, False, True, True, False, False);
      TargetDbAndObject := m(DBObj.Name);
      if ToDb then
        TargetDbAndObject := m(FinalDbName) + '.' + TargetDbAndObject;
      Offset := 0;
      RowCount := 0;
      // Calculate limit so we select ~100MB per loop
      Limit := Round(100 * SIZE_MB / Max(DBObj.AvgRowLen,1));
      if comboExportData.Text = DATA_REPLACE then
        Output('DELETE FROM '+TargetDbAndObject, True, True, True, True, True);
      Output('/*!40000 ALTER TABLE '+TargetDbAndObject+' DISABLE KEYS */', True, True, True, True, True);
      while true do begin
        Data := MainForm.ActiveConnection.GetResults('SELECT * FROM '+m(DBObj.Database)+'.'+m(DBObj.Name)+' LIMIT '+IntToStr(Offset)+', '+IntToStr(Limit));
        Inc(Offset, Limit);
        if Data.RecordCount = 0 then
          break;
        BaseInsert := 'INSERT INTO ';
        if comboExportData.Text = DATA_INSERTNEW then
          BaseInsert := 'INSERT IGNORE INTO '
        else if comboExportData.Text = DATA_UPDATE then
          BaseInsert := 'REPLACE INTO ';
        BaseInsert := BaseInsert + TargetDbAndObject + ' (';
        for i:=0 to Data.ColumnCount-1 do
          BaseInsert := BaseInsert + m(Data.ColumnNames[i]) + ', ';
        Delete(BaseInsert, Length(BaseInsert)-1, 2);
        BaseInsert := BaseInsert + ') VALUES'+CRLF+#9+'(';
        while true do begin
          Output(BaseInsert, False, True, True, True, True);
          IsFirstRowInChunk := True;

          while not Data.Eof do begin
            Row := '';
            if not IsFirstRowInChunk then
              Row := Row + ','+CRLF+#9+'(';
            for i:=0 to Data.ColumnCount-1 do begin
              if Data.IsNull(i) then
                Row := Row + 'NULL'
              else case Data.DataType(i).Category of
                dtcInteger, dtcReal: Row := Row + Data.Col(i);
                dtcBinary, dtcSpatial: begin
                  BinContent := Data.BinColAsHex(i);
                  if Length(BinContent) > 0 then
                    Row := Row + '_binary 0x' + BinContent
                  else
                    Row := Row + esc('');
                end;
                else Row := Row + esc(Data.Col(i));
              end;
              if i<Data.ColumnCount-1 then
                Row := Row + ', ';
            end;
            Row := Row + ')';
            // Break if stream would increase over the barrier of 1MB, and throw away current row
            if (not IsFirstRowInChunk)
              and (ExportStream.Size - ExportStreamStartOfQueryPos + Length(Row) > SIZE_MB*0.9)
              then
              break;
            Inc(RowCount);
            IsFirstRowInChunk := False;
            Output(Row, False, True, True, True, True);
            Data.Next;
          end;
          Output('', True, True, True, True, True);
          LogStatistic(RowCount);
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
      LogStatistic(DbObj.Rows); // Cosmetic fix for estimated InnoDB row count
    end;
  end;

  // Add footer in directory mode after each item
  Output(EXPORT_FILE_FOOTER, False, False, True, False, False);

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
  Specs, LogRow: TStringList;
  CreateView: String;
  rx: TRegExpr;
  HasCharsetClause: Boolean;
begin
  AddResults('SELECT '+esc(DBObj.Database)+' AS '+Mainform.mask('Database')+', ' +
    esc(DBObj.Name)+' AS '+Mainform.mask('Table')+', ' +
    esc('Updating...')+' AS '+Mainform.mask('Operation')+', '+
    ''''' AS '+Mainform.mask('Result')
    );
  Specs := TStringList.Create;
  if chkBulkTableEditDatabase.Checked and (comboBulkTableEditDatabase.Text <> DBObj.Database) then begin
    case DBObj.NodeType of
      lntTable: Specs.Add('RENAME ' + Mainform.mask(comboBulkTableEditDatabase.Text)+'.'+Mainform.mask(DBObj.Name));
      lntView: begin
        // Although RENAME works for views, that does not work for moving to another database without getting
        // a "Changing schema from x to y is not allowed". Instead, recreate them manually
        CreateView := DBObj.CreateCode;
        rx := TRegExpr.Create;
        rx.ModifierI := True;
        // Replace old database references in VIEW body
        rx.Expression := '(["`])'+QuoteRegExprMetaChars(DBObj.Database)+'(["`])';
        CreateView := rx.Replace(CreateView, Mainform.mask(comboBulkTableEditDatabase.Text), false);
        rx.Free;
        // Temporarily switch to new database for VIEW creation, so the database references are correct
        DBObj.Connection.Database := comboBulkTableEditDatabase.Text;
        DBObj.Connection.Query(CreateView);
        DBObj.Connection.Database := DBObj.Database;
        DBObj.Connection.Query('DROP VIEW '+Mainform.mask(DBObj.Name));
      end;
    end;
    FModifiedDbs.Add(DBObj.Database);
    FModifiedDbs.Add(comboBulkTableEditDatabase.Text);
  end;
  if (DBObj.NodeType = lntTable) and chkBulkTableEditEngine.Checked then begin
    if MainForm.ActiveConnection.ServerVersionInt < 40018 then
      Specs.Add('TYPE '+comboBulkTableEditEngine.Text)
    else
      Specs.Add('ENGINE '+comboBulkTableEditEngine.Text);
  end;
  if DBObj.NodeType = lntTable then begin
    HasCharsetClause := False;
    if chkBulkTableEditCharset.Checked and (comboBulkTableEditCharset.ItemIndex > -1) then begin
      MainForm.ActiveConnection.CharsetTable.RecNo := comboBulkTableEditCharset.ItemIndex;
      Specs.Add('CONVERT TO CHARSET '+DBObj.Connection.CharsetTable.Col('Charset'));
      HasCharsetClause := True;
    end;
    if chkBulkTableEditCollation.Checked and (comboBulkTableEditCollation.ItemIndex > -1) then begin
      if HasCharsetClause then // No comma between charset + collation clause
        Specs[Specs.Count-1] := Specs[Specs.Count-1] + ' COLLATE '+comboBulkTableEditCollation.Text
      else
        Specs.Add('COLLATE '+comboBulkTableEditCollation.Text);
    end;
    if chkBulkTableEditResetAutoinc.Checked then
      Specs.Add('AUTO_INCREMENT=0');
  end;

  LogRow := FResults.Last;
  if Specs.Count > 0 then begin
    DBObj.Connection.Query('ALTER TABLE ' + Mainform.mask(DBObj.Database) + '.' + Mainform.mask(DBObj.Name) + ' ' + ImplodeStr(', ', Specs));
    LogRow[2] := 'Done';
    LogRow[3] := 'Success';
  end else begin
    LogRow[2] := 'Nothing to do';
    LogRow[3] := 'Selected operations cannot be applied to a '+LowerCase(DBObj.ObjType);
  end;
  UpdateResultGrid;
end;


end.
