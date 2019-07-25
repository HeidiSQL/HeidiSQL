unit tabletools;


// -------------------------------------
// Table-diagnostics
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls, Buttons, Dialogs, StdActns,
  VirtualTrees, ExtCtrls, Graphics, SynRegExpr, Math, Generics.Collections, extra_controls,
  dbconnection, apphelpers, Menus, gnugettext, DateUtils, System.Zip, System.UITypes, StrUtils, Messages;

type
  TToolMode = (tmMaintenance, tmFind, tmSQLExport, tmBulkTableEdit);
  TfrmTableTools = class(TExtForm)
    btnCloseOrCancel: TButton;
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
    btnHelpMaintenance: TButton;
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
    lblCheckedSize: TLabel;
    popupTree: TPopupMenu;
    menuCheckAll: TMenuItem;
    menuCheckByType: TMenuItem;
    menuCheckNone: TMenuItem;
    chkForUpgrade: TCheckBox;
    lblInsertSize: TLabel;
    editInsertSize: TEdit;
    updownInsertSize: TUpDown;
    lblInsertSizeUnit: TLabel;
    btnExportOptions: TButton;
    popupExportOptions: TPopupMenu;
    menuExportAddComments: TMenuItem;
    menuExportRemoveAutoIncrement: TMenuItem;
    comboMatchType: TComboBox;
    lblMatchType: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnHelpMaintenanceClick(Sender: TObject);
    procedure TreeObjectsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure TreeObjectsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeObjectsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
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
    procedure SaveSettings(Sender: TObject);
    procedure chkExportOptionClick(Sender: TObject);
    procedure btnExportOutputTargetSelectClick(Sender: TObject);
    procedure comboExportOutputTargetChange(Sender: TObject);
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
    procedure btnCloseOrCancelClick(Sender: TObject);
    procedure TreeObjectsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure CheckAllClick(Sender: TObject);
    procedure TreeObjectsExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure btnExportOptionsClick(Sender: TObject);
  private
    { Private declarations }
    FResults: TObjectList<TStringList>;
    FToolMode: TToolMode;
    FSecondExportPass: Boolean; // Set to True after everything is exported and final VIEWs need to be exported again
    FCancelled: Boolean;
    ExportStream: TStream;
    FExportFileName: String;
    ExportStreamStartOfQueryPos: Int64;
    ExportLastDatabase: String;
    FTargetConnection: TDBConnection;
    FLastOutputSelectedIndex: Integer;
    FModifiedDbs: TStringList;
    FHeaderCreated: Boolean;
    FFindSeeResultSQL: TStringList;
    ToFile, ToDir, ToClipboard, ToDb, ToServer: Boolean;
    FObjectSizes, FObjectSizesDone, FObjectSizesDoneExact: Int64;
    procedure WMNCLBUTTONDOWN(var Msg: TWMNCLButtonDown) ; message WM_NCLBUTTONDOWN;
    procedure WMNCLBUTTONUP(var Msg: TWMNCLButtonUp) ; message WM_NCLBUTTONUP;
    procedure SetToolMode(Value: TToolMode);
    procedure Output(SQL: String; IsEndOfQuery, ForFile, ForDir, ForDb, ForServer: Boolean);
    procedure AddResults(SQL: String; Connection: TDBConnection);
    procedure AddNotes(Col1, Col2, Col3, Col4: String); overload;
    procedure AddNotes(DBObject: TDBObject; Msg1, Msg2: String); overload;
    procedure SetupResultGrid(Results: TDBQuery=nil);
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

uses main, dbstructures;

const
  STRSKIPPED: String = 'Skipped - ';
  EXPORT_FILE_FOOTER = '/*!40101 SET SQL_MODE=IFNULL(@OLD_SQL_MODE, '''') */;'+CRLF+
    '/*!40014 SET FOREIGN_KEY_CHECKS=IF(@OLD_FOREIGN_KEY_CHECKS IS NULL, 1, @OLD_FOREIGN_KEY_CHECKS) */;'+CRLF+
    '/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;'+CRLF;

var
  OUTPUT_FILE,
  OUTPUT_FILE_COMPRESSED,
  OUTPUT_CLIPBOARD,
  OUTPUT_DIR,
  OUTPUT_DB,
  OUTPUT_SERVER,
  DATA_NO,
  DATA_REPLACE,
  DATA_INSERT,
  DATA_INSERTNEW,
  DATA_UPDATE : String;

{$R *.DFM}


procedure TfrmTableTools.WMNCLBUTTONDOWN(var Msg: TWMNCLButtonDown) ;
begin
  if Msg.HitTest = HTHELP then
    Msg.Result := 0 // "eat" the message
  else
    inherited;
end;


procedure TfrmTableTools.WMNCLBUTTONUP(var Msg: TWMNCLButtonUp) ;
begin
  if Msg.HitTest = HTHELP then begin
    Msg.Result := 0;
    if tabsTools.ActivePage = tabSQLexport then
      Help(Self, 'sqlexport')
    else
      ErrorDialog(_('No help available for this tab.'));
  end else
    inherited;
end;


procedure TfrmTableTools.FormCreate(Sender: TObject);
var
  i: Integer;
  dtc: TDBDatatypeCategoryIndex;
  SessionPaths: TStringList;
  MenuItem: TMenuItem;
  dt: TListNodeType;
  Obj: TDBObject;
begin
  HasSizeGrip := True;
  OUTPUT_FILE := _('Single .sql file');
  OUTPUT_FILE_COMPRESSED := _('ZIP compressed .sql file');
  OUTPUT_CLIPBOARD := _('Clipboard');
  OUTPUT_DIR := _('Directory - one file per object in database subdirectories');
  OUTPUT_DB := _('Database');
  OUTPUT_SERVER := _('Server')+': ';
  DATA_NO := _('No data');
  DATA_REPLACE := _('Delete + insert (truncate existing data)');
  DATA_INSERT := _('Insert');
  DATA_INSERTNEW := _('Insert ignore (do not update existing)');
  DATA_UPDATE := _('Replace existing data');

  // Restore GUI setup
  Width := AppSettings.ReadInt(asTableToolsWindowWidth);
  Height := AppSettings.ReadInt(asTableToolsWindowHeight);
  TreeObjects.Width := AppSettings.ReadInt(asTableToolsTreeWidth);

  // Find text tab
  memoFindText.Text := AppSettings.ReadString(asTableToolsFindText);
  comboDatatypes.Items.Add(_('All data types'));
  for dtc:=Low(DatatypeCategories) to High(DatatypeCategories) do
    comboDatatypes.Items.Add(DatatypeCategories[dtc].Name);
  comboDatatypes.ItemIndex := AppSettings.ReadInt(asTableToolsDatatype);
  chkCaseSensitive.Checked := AppSettings.ReadBool(asTableToolsFindCaseSensitive);
  comboMatchType.ItemIndex := AppSettings.ReadInt(asTableToolsFindMatchType);

  // SQL export tab
  chkExportDatabasesCreate.Checked := AppSettings.ReadBool(asExportSQLCreateDatabases);
  chkExportTablesCreate.Checked := AppSettings.ReadBool(asExportSQLCreateTables);
  comboExportData.Items.Text := DATA_NO+CRLF +DATA_REPLACE+CRLF +DATA_INSERT+CRLF +DATA_INSERTNEW+CRLF +DATA_UPDATE;
  comboExportData.ItemIndex := AppSettings.ReadInt(asExportSQLDataHow);
  updownInsertSize.Position := AppSettings.ReadInt(asExportSQLDataInsertSize);
  menuExportAddComments.Checked := AppSettings.ReadBool(asExportSQLAddComments);
  menuExportRemoveAutoIncrement.Checked := AppSettings.ReadBool(asExportSQLRemoveAutoIncrement);
  // Add hardcoded output options and session names from registry
  comboExportOutputType.Items.Text :=
    OUTPUT_FILE + CRLF +
    OUTPUT_FILE_COMPRESSED + CRLF +
    OUTPUT_DIR + CRLF +
    OUTPUT_CLIPBOARD + CRLF +
    OUTPUT_DB;
  SessionPaths := TStringList.Create;
  AppSettings.GetSessionPaths('', SessionPaths);
  for i:=0 to SessionPaths.Count-1 do begin
    if SessionPaths[i] <> Mainform.ActiveConnection.Parameters.SessionPath then
      comboExportOutputType.Items.Add(OUTPUT_SERVER+SessionPaths[i]);
  end;
  SessionPaths.Free;
  comboExportOutputTarget.Text := '';

  // Various
  FixVT(TreeObjects);
  FixVT(ResultGrid);
  FResults := TObjectList<TStringList>.Create;
  PreSelectObjects := TDBObjectList.Create(False);
  FModifiedDbs := TStringList.Create;
  FFindSeeResultSQL := TStringList.Create;

  // Popup menu
  Obj := TDBObject.Create(nil);
  for dt:=lntTable to lntEvent do begin
    Obj.NodeType := dt;
    MenuItem := TMenuItem.Create(menuCheckByType);
    MenuItem.Caption := _(Obj.ObjType+'s');
    MenuItem.ImageIndex := Obj.ImageIndex;
    MenuItem.OnClick := CheckAllClick;
    MenuItem.Tag := Integer(dt);
    menuCheckByType.Add(MenuItem);
  end;
  Obj.Free;
end;


procedure TfrmTableTools.FormDestroy(Sender: TObject);
begin
  // Save GUI setup
  AppSettings.WriteInt(asTableToolsWindowWidth, Width);
  AppSettings.WriteInt(asTableToolsWindowHeight, Height);
  AppSettings.WriteInt(asTableToolsTreeWidth, TreeObjects.Width);
end;


procedure TfrmTableTools.FormShow(Sender: TObject);
var
  Node, FirstChecked: PVirtualNode;
  idx: Integer;
  DBObj: TDBObject;
begin
  // When this form is displayed the second time, databases may be deleted or filtered.
  // Also, checked nodes must be unchecked and unchecked nodes may need to be checked.
  TreeObjects.Clear;
  TreeObjects.RootNodeCount := Mainform.DBtree.RootNodeCount;

  FObjectSizes := 0;

  // Init all objects in active database, so the tree does not just check the db node
  // if we want the first child only. See issue #2267.
  Node := MainForm.FindDBNode(TreeObjects, MainForm.ActiveConnection, MainForm.ActiveDatabase);
  Node := TreeObjects.GetFirstChild(Node);
  while Assigned(Node) do
    Node := TreeObjects.GetNextSibling(Node);
  for DBObj in PreSelectObjects do begin
    Node := MainForm.FindDBObjectNode(TreeObjects, DBObj);
    if Assigned(Node) then
      TreeObjects.CheckState[Node] := csCheckedNormal;
  end;

  FirstChecked := TreeObjects.GetFirstChecked;
  if Assigned(FirstChecked) then
    SelectNode(TreeObjects, FirstChecked);
  // CHECKSUM available since MySQL 4.1.1
  idx := comboOperation.ItemIndex;
  if idx = -1 then idx := 0;
  comboOperation.Items.CommaText := 'Check,Analyze,Checksum,Optimize,Repair';
  if Mainform.ActiveConnection.ServerVersionInt < 40101 then
    comboOperation.Items.Text := StringReplace(comboOperation.Items.Text, 'Checksum', 'Checksum ('+_(SUnsupported)+')', [rfReplaceAll]);
  comboOperation.ItemIndex := idx;
  comboOperation.OnChange(Sender);

  // Restore output option. Avoid server preselection to avoid unwanted connects.
  // See issue #3411
  idx := AppSettings.ReadInt(asExportSQLOutput);
  if (idx = -1)
    or (idx >= comboExportOutputType.Items.Count)
    or StartsStr(OUTPUT_SERVER, comboExportOutputType.Items[idx])
    then idx := 0;
  comboExportOutputType.ItemIndex := idx;
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
  Action := caFree;
end;


procedure TfrmTableTools.SaveSettings(Sender: TObject);
var
  i: Integer;
  Items: TStringList;
begin
  case ToolMode of
    tmFind: begin
      AppSettings.WriteString(asTableToolsFindText, memoFindText.Text);
      AppSettings.WriteInt(asTableToolsDatatype, comboDatatypes.ItemIndex);
      AppSettings.WriteBool(asTableToolsFindCaseSensitive, chkCaseSensitive.Checked);
      AppSettings.WriteInt(asTableToolsFindMatchType, comboMatchType.ItemIndex);
    end;

    tmSQLExport: begin
      AppSettings.WriteBool(asExportSQLCreateDatabases, chkExportDatabasesCreate.Checked);
      AppSettings.WriteBool(asExportSQLCreateTables, chkExportTablesCreate.Checked);
      AppSettings.WriteInt(asExportSQLDataHow, comboExportData.ItemIndex);
      if comboExportData.ItemIndex > 0 then
        AppSettings.WriteInt(asExportSQLDataInsertSize, updownInsertSize.Position);
      AppSettings.WriteBool(asExportSQLAddComments, menuExportAddComments.Checked);
      AppSettings.WriteBool(asExportSQLRemoveAutoIncrement, menuExportRemoveAutoIncrement.Checked);

      if not StartsStr(OUTPUT_SERVER, comboExportOutputType.Text) then
        AppSettings.WriteInt(asExportSQLOutput, comboExportOutputType.ItemIndex);

      // Remove duplicates from recent file pulldown
      if (comboExportOutputType.Text = OUTPUT_FILE)
        or (comboExportOutputType.Text = OUTPUT_FILE_COMPRESSED)
        or (comboExportOutputType.Text = OUTPUT_DIR)
        then begin
        Items := TStringList.Create;
        Items.Assign(comboExportOutputTarget.Items);
        Items.Insert(0, comboExportOutputTarget.Text);
        for i:=Items.Count-1 downto 1 do begin
          if Items[i] = comboExportOutputTarget.Text then
            Items.Delete(i);
        end;
        comboExportOutputTarget.Items.Assign(Items);
        Items.Free;
      end;

      if comboExportOutputType.Text = OUTPUT_FILE then begin
        AppSettings.WriteString(asExportSQLFilenames, comboExportOutputTarget.Items.Text);
      end else if comboExportOutputType.Text = OUTPUT_FILE_COMPRESSED then begin
        AppSettings.WriteString(asExportZIPFilenames, comboExportOutputTarget.Items.Text);
      end else if comboExportOutputType.Text = OUTPUT_DIR then begin
        AppSettings.WriteString(asExportSQLDirectories, comboExportOutputTarget.Items.Text);
      end else if comboExportOutputType.Text = OUTPUT_DB then begin
        AppSettings.WriteString(asExportSQLDatabase, comboExportOutputTarget.Text);
      end else if copy(comboExportOutputType.Text, 1, Length(OUTPUT_SERVER)) = OUTPUT_SERVER then begin
        AppSettings.WriteString(asExportSQLServerDatabase, comboExportOutputTarget.Text);
      end;
    end;

  end;

end;


procedure TfrmTableTools.ValidateControls(Sender: TObject);
var
  SomeChecked, OptionChecked: Boolean;
  op: String;
  i: Integer;
begin
  SomeChecked := TreeObjects.CheckedCount > 0;
  btnSeeResults.Visible := tabsTools.ActivePage = tabFind;
  lblCheckedSize.Caption := f_('Selected objects size: %s', [FormatByteNumber(FObjectSizes)]);
  if tabsTools.ActivePage = tabMaintenance then begin
    btnExecute.Caption := _('Execute');
    btnExecute.Enabled := (Pos(_(SUnsupported), comboOperation.Text) = 0) and SomeChecked;
    // Only enable available options
    op := LowerCase(comboOperation.Text);
    chkQuick.Enabled := (op = 'check') or (op = 'checksum') or (op = 'repair');
    chkFast.Enabled := op = 'check';
    chkMedium.Enabled := op = 'check';
    chkExtended.Enabled := (op = 'check') or (op = 'checksum') or (op = 'repair');
    chkChanged.Enabled := op = 'check';
    chkUseFrm.Enabled := op = 'repair';
    chkForUpgrade.Enabled := op = 'check';
    // CHECKSUM's options are mutually exclusive
    if comboOperation.Text = 'Checksum' then begin
      if (Sender = chkExtended) and chkExtended.Checked then chkQuick.Checked := False
      else if chkQuick.Checked then chkExtended.Checked := False;
    end;
  end else if tabsTools.ActivePage = tabFind then begin
    btnExecute.Caption := _('Find');
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
    btnExecute.Caption := _('Export');
    btnExecute.Enabled := SomeChecked and ((comboExportOutputTarget.Text <> '') or (not comboExportOutputTarget.Enabled));
    lblInsertSize.Enabled := comboExportData.ItemIndex > 0;
    editInsertSize.Enabled := lblInsertSize.Enabled;
    updownInsertSize.Enabled := lblInsertSize.Enabled;
    lblInsertSizeUnit.Enabled := lblInsertSize.Enabled;
  end else if tabsTools.ActivePage = tabBulkTableEdit then begin
    btnExecute.Caption := _('Update');
    chkBulkTableEditCollation.Enabled := MainForm.ActiveConnection.IsUnicode;
    chkBulkTableEditCharset.Enabled := MainForm.ActiveConnection.IsUnicode;
    OptionChecked := chkBulkTableEditDatabase.Checked or chkBulkTableEditEngine.Checked or chkBulkTableEditCollation.Checked
      or chkBulkTableEditCharset.Checked or chkBulkTableEditResetAutoinc.Checked;
    btnExecute.Enabled := SomeChecked and OptionChecked;
  end;
end;


procedure TfrmTableTools.TreeObjectsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  MainForm.DBtreeBeforeCellPaint(Sender, TargetCanvas, Node, Column, CellPaintMode, CellRect, ContentRect);
end;

procedure TfrmTableTools.TreeObjectsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  Mainform.DBtreeChange(Sender, Node);
end;


procedure TfrmTableTools.TreeObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Obj: PDBObject;
  ObjSize: Int64;
begin
  // Track sum of checked objects size
  Obj := Sender.GetNodeData(Node);
  ObjSize := Max(Obj.Size, 0);
  if Node.CheckState in CheckedStates then
    Inc(FObjectSizes, ObjSize)
  else
    Dec(FObjectSizes, ObjSize);
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


procedure TfrmTableTools.TreeObjectsExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  // Auto-resizes the 2nd/"size" column
  TreeObjectsChange(Sender, Node);
end;

procedure TfrmTableTools.TreeObjectsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
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


procedure TfrmTableTools.btnHelpMaintenanceClick(Sender: TObject);
begin
  Mainform.CallSQLHelpWithKeyword(UpperCase(comboOperation.Text) + ' TABLE');
end;


procedure TfrmTableTools.Execute(Sender: TObject);
var
  SessionNode, DBNode: PVirtualNode;
  CheckedObjects, Triggers, Views: TDBObjectList;
  DBObj: TDBObject;
  i: Integer;
  Conn: TDBConnection;
  FileName, FileNameZip, FileNameInZip: TFileName;
  Zip: TZipFile;
  StartTime: Cardinal;
  LogRow: TStringlist;

  procedure ProcessNode(DBObj: TDBObject);
  begin
    try
      case FToolMode of
        tmMaintenance: DoMaintenance(DBObj);
        tmFind: DoFind(DBObj);
        tmSQLExport: DoExport(DBObj);
        tmBulkTableEdit: DoBulkTableEdit(DBObj);
      end;
    except
      on E:EDbError do begin
        // The above SQL can easily throw an exception, e.g. if a table is corrupted.
        // In such cases we create a dummy row, including the error message
        AddNotes(DBObj, 'error', E.Message);
        // Cancel further processing on critical errors
        if E.ErrorCode = 1049 then begin // "Unknown database"
          ErrorDialog(E.Message);
          FCancelled := True;
        end;
      end;
      on E:EFCreateError do begin
        // Occurs when export output file can not be created
        ErrorDialog(E.Message);
        FCancelled := True;
      end;
    end;
  end;

  procedure SetCheckedObjects(DBNode: PVirtualNode);
  var
    Child, GrandChild: PVirtualNode;
    ChildObj, GrandChildObj: PDBObject;
  begin
    CheckedObjects.Clear;
    Child := TreeObjects.GetFirstChild(DBNode);
    while Assigned(Child) do begin
      if Child.CheckState in CheckedStates then begin
        ChildObj := TreeObjects.GetNodeData(Child);

        case ChildObj.NodeType of

          lntGroup: begin
            GrandChild := TreeObjects.GetFirstChild(Child);
            while Assigned(GrandChild) do begin
              if GrandChild.CheckState in CheckedStates then begin
                GrandChildObj := TreeObjects.GetNodeData(GrandChild);
                CheckedObjects.Add(GrandChildObj^);
              end;
              GrandChild := TreeObjects.GetNextSibling(GrandChild);
            end;
          end

          else begin
            CheckedObjects.Add(ChildObj^);
          end;

        end;
      end;
      Child := TreeObjects.GetNextSibling(Child);
    end;
  end;

begin
  Screen.Cursor := crHourGlass;
  // Disable critical controls so ProcessMessages is unable to do things while export is in progress
  btnExecute.Enabled := False;
  btnCloseOrCancel.Caption := _('Cancel');
  btnCloseOrCancel.ModalResult := mrNone;
  tabsTools.Enabled := False;
  treeObjects.Enabled := False;
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
  CheckedObjects := TDBObjectList.Create(False);
  Triggers := TDBObjectList.Create(False); // False, so we can .Free that object afterwards without loosing the contained objects
  Views := TDBObjectList.Create(False);
  FHeaderCreated := False;
  FCancelled := False;
  FObjectSizesDone := 0;
  FObjectSizesDoneExact := 0;
  MainForm.EnableProgress(100);
  SessionNode := TreeObjects.GetFirstChild(nil);
  while Assigned(SessionNode) do begin
    DBNode := TreeObjects.GetFirstChild(SessionNode);
    while Assigned(DBNode) do begin
      if not (DBNode.CheckState in [csUncheckedNormal, csUncheckedPressed]) then begin
        Triggers.Clear;
        Views.Clear;
        FSecondExportPass := False;
        SetCheckedObjects(DBNode);
        for DBObj in CheckedObjects do begin
          // Triggers have to be exported at the very end
          if (FToolMode = tmSQLExport) and (DBObj.NodeType = lntTrigger) then
            Triggers.Add(DBObj)
          else begin
            ProcessNode(DBObj);
            FObjectSizesDone := FObjectSizesDone + Max(DBObj.Size, 0);
            FObjectSizesDoneExact := FObjectSizesDone;
            if (FToolMode = tmSQLExport) and (DBObj.NodeType = lntView) then
              Views.Add(DBObj);
          end;
          // File creation exception occurred or user clicked cancel button
          if FCancelled then Break;
        end; // End of db object node loop in db

        // Special block for late created triggers in export mode
        for i:=0 to Triggers.Count-1 do begin
          ProcessNode(Triggers[i]);
          if FCancelled then Break;
        end;

        // Special block for exporting final view structure
        FSecondExportPass := True;
        for i:=0 to Views.Count-1 do begin
          ProcessNode(Views[i]);
          if FCancelled then Break;
        end;

      end;
      if FCancelled then Break;
      DBNode := TreeObjects.GetNextSibling(DBNode);
    end; // End of db item loop
    if FCancelled then Break;
    SessionNode := TreeObjects.GetNextSibling(SessionNode);
  end;

  Conn := Mainform.ActiveConnection;

  if Assigned(ExportStream) then begin
    Output(EXPORT_FILE_FOOTER, False, True, False, False, False);
    Output('/*!40101 SET SQL_MODE=IFNULL(@OLD_SQL_MODE, '''') */', True, False, False, True, True);
    Output('/*!40014 SET FOREIGN_KEY_CHECKS=IF(@OLD_FOREIGN_KEY_CHECKS IS NULL, 1, @OLD_FOREIGN_KEY_CHECKS) */', True, False, False, True, True);
    if comboExportOutputType.Text = OUTPUT_CLIPBOARD then
      StreamToClipboard(ExportStream, nil, false);

    if comboExportOutputType.Text = OUTPUT_FILE_COMPRESSED then
      FileName := TFileStream(ExportStream).FileName;
    FreeAndNil(ExportStream);

    if comboExportOutputType.Text = OUTPUT_FILE_COMPRESSED then begin
      AddNotes('', '', _('Compressing')+'...', '');
      StartTime := GetTickCount;
      FileNameZip := FExportFileName;
      if FileExists(FileNameZip) then
        DeleteFile(FileNameZip);
      Zip := TZipFile.Create;
      Zip.Open(FileNameZip, zmWrite);
      FileNameInZip := ExtractFileName(ChangeFileExt(FileNameZip, '.sql'));
      Zip.Add(FileName, FileNameInZip);
      Zip.Close;
      DeleteFile(FileName);
      LogRow := FResults.Last;
      LogRow[2] := _('Compressing done.');
      LogRow[3] := FormatTimeNumber((GetTickCount-StartTime) / 1000, True);
      ResultGrid.Repaint;
    end;

    // Activate ansi mode or whatever again, locally
    Conn.Query('/*!40101 SET SQL_MODE=IFNULL(@OLD_LOCAL_SQL_MODE, '''') */');
  end;
  ExportLastDatabase := '';

  for i:=0 to FModifiedDbs.Count-1 do begin
    Conn.ClearDbObjects(FModifiedDbs[i]);
    DBNode := MainForm.FindDBNode(TreeObjects, Conn, FModifiedDbs[i]);
    TreeObjects.ReinitNode(DBNode, False);
    TreeObjects.ReinitChildren(DBNode, False)
  end;
  FModifiedDbs.Clear;

  btnCloseOrCancel.Caption := _('Close');
  btnCloseOrCancel.ModalResult := mrCancel;
  MainForm.DisableProgress;
  tabsTools.Enabled := True;
  treeObjects.Enabled := True;
  ValidateControls(Sender);
  SaveSettings(Sender);
  Screen.Cursor := crDefault;
end;


procedure TfrmTableTools.DoMaintenance(DBObj: TDBObject);
var
  SQL: String;
begin
  if not (DBObj.NodeType in [lntTable, lntView]) then begin
    AddNotes(DBObj, STRSKIPPED+'a '+LowerCase(DBObj.ObjType)+' cannot be maintained.', '');
    Exit;
  end;
  SQL := UpperCase(comboOperation.Text) + ' TABLE ' + DBObj.QuotedDatabase + '.' + DBObj.QuotedName;
  if chkQuick.Enabled and chkQuick.Checked then SQL := SQL + ' QUICK';
  if chkFast.Enabled and chkFast.Checked then SQL := SQL + ' FAST';
  if chkMedium.Enabled and chkMedium.Checked then SQL := SQL + ' MEDIUM';
  if chkExtended.Enabled and chkExtended.Checked then SQL := SQL + ' EXTENDED';
  if chkChanged.Enabled and chkChanged.Checked then SQL := SQL + ' CHANGED';
  if chkUseFrm.Enabled and chkUseFrm.Checked then SQL := SQL + ' USE_FRM';
  if chkForUpgrade.Enabled and chkForUpgrade.Checked then SQL := SQL + ' FOR UPGRADE';
  AddResults(SQL, DBObj.Connection);
end;


procedure TfrmTableTools.DoFind(DBObj: TDBObject);
var
  Columns: TTableColumnList;
  Col: TTableColumn;
  SQL, Dummy, Column, RoutineDefinitionColumn, RoutineSchemaColumn, FindText, FindTextJokers: String;
  IsRegExp: Boolean;
begin
  FFindSeeResultSQL.Add('');

  FindText := memoFindText.Text;
  case comboMatchType.ItemIndex of
    0,4: FindTextJokers := '%'+FindText+'%'; // Used as wildcard for regex on MSSQL
    1: FindTextJokers := FindText;
    2: FindTextJokers := '%'+FindText;
    3: FindTextJokers := FindText+'%';
  end;
  IsRegExp := comboMatchType.ItemIndex = 4;

  RoutineDefinitionColumn := DBObj.Connection.QuoteIdent('routine_definition');
  if not chkCaseSensitive.Checked then begin
    FindText := LowerCase(FindText);
    FindTextJokers := LowerCase(FindTextJokers);
    RoutineDefinitionColumn := 'LOWER('+RoutineDefinitionColumn+')';
  end;
  RoutineSchemaColumn := 'routine_schema';
  if DBObj.Connection.Parameters.IsMSSQL then
    RoutineSchemaColumn := 'routine_catalog';

  Columns := TTableColumnList.Create(True);
  case DBObj.NodeType of
    lntTable: DBObj.Connection.ParseTableStructure(DBObj.CreateCode, Columns, nil, nil);
    lntView: DBObj.Connection.ParseViewStructure(DBObj.CreateCode, DBObj, Columns, Dummy, Dummy, Dummy, Dummy, Dummy);
    lntProcedure, lntFunction: ;
    // TODO: Triggers + Events
    else AddNotes(DBObj, STRSKIPPED+'a '+LowerCase(DBObj.ObjType)+' does not contain rows.', '');
  end;
  case DBObj.NodeType of
    lntTable, lntView: begin
      if Columns.Count > 0 then begin
        SQL := '';
        for Col in Columns do begin
          Column := DBObj.Connection.QuoteIdent(Col.Name);
          if (comboDatatypes.ItemIndex = 0) or (Integer(Col.DataType.Category) = comboDatatypes.ItemIndex-1) then begin

            if (Col.DataType.Category in [dtcInteger, dtcReal]) and (comboMatchType.ItemIndex=1) then begin
              // Search numbers
              SQL := SQL + Column + '=' + UnformatNumber(FindText) + ' OR ';

            end else if chkCaseSensitive.Checked then begin
              // Search case sensitive
              case DBObj.Connection.Parameters.NetTypeGroup of
                ngMySQL: begin
                  if IsRegExp then
                    SQL := SQL + Column + ' REGEXP BINARY ' + esc(FindText) + ' OR '
                  else
                    SQL := SQL + Column + ' LIKE BINARY ' + esc(FindTextJokers) + ' OR ';
                end;
                ngMSSQL:
                  SQL := SQL + Column+' LIKE ' + esc(FindTextJokers) + ' COLLATE SQL_Latin1_General_CP1_CS_AS OR ';
                ngPgSQL: begin
                  if IsRegExp then
                    SQL := SQL + 'CAST(' + Column + ' AS TEXT) SIMILAR TO ' + esc(FindTextJokers) + ' OR '
                  else
                    SQL := SQL + 'CAST(' + Column + ' AS TEXT) LIKE ' + esc(FindTextJokers) + ' OR ';
                end;
              end;

            end else begin
              // Search case insensitive
              case DBObj.Connection.Parameters.NetTypeGroup of
                ngMySQL: begin
                  if IsRegExp then
                    SQL := SQL + 'LOWER(CONVERT('+Column+' USING '+DBObj.Connection.CharacterSet+')) REGEXP ' + esc(FindText) + ' OR '
                  else
                    SQL := SQL + 'LOWER(CONVERT('+Column+' USING '+DBObj.Connection.CharacterSet+')) LIKE ' + esc(FindTextJokers) + ' OR ';
                end;
                ngMSSQL: begin
                  SQL := SQL + 'LOWER('+Column+') LIKE ' + esc(FindTextJokers) + ' OR ';
                end;
                ngPgSQL: begin
                  if IsRegExp then
                    SQL := SQL + 'LOWER(CAST('+Column+' AS TEXT)) SIMILAR TO ' + esc(FindTextJokers) + ' OR '
                  else
                    SQL := SQL + 'LOWER(CAST('+Column+' AS TEXT)) LIKE ' + esc(FindTextJokers) + ' OR ';
                end;
              end;
            end;

          end;
        end; // end of loop over columns

        if SQL <> '' then begin
          Delete(SQL, Length(SQL)-3, 3);
          FFindSeeResultSQL[FFindSeeResultSQL.Count-1] := 'SELECT * FROM '+DBObj.QuotedDatabase+'.'+DBObj.QuotedName+' WHERE ' + SQL;
          case DBObj.Connection.Parameters.NetTypeGroup of
            ngMySQL, ngPgSQL:
              SQL := 'SELECT '''+DBObj.Database+''' AS '+DBObj.Connection.QuoteIdent('Database')+', '''+DBObj.Name+''' AS '+DBObj.Connection.QuoteIdent('Table')+', COUNT(*) AS '+DBObj.Connection.QuoteIdent('Found rows')+', '
                + 'CONCAT(ROUND(100 / '+IntToStr(Max(DBObj.Rows,1))+' * COUNT(*), 1), ''%'') AS '+DBObj.Connection.QuoteIdent('Relevance')+' FROM '+DBObj.QuotedDatabase+'.'+DBObj.QuotedName+' WHERE '
                + SQL;
            ngMSSQL:
              SQL := 'SELECT '''+DBObj.Database+''' AS '+DBObj.Connection.QuoteIdent('Database')+', '''+DBObj.Name+''' AS '+DBObj.Connection.QuoteIdent('Table')+', COUNT(*) AS '+DBObj.Connection.QuoteIdent('Found rows')+', '
                + 'CONVERT(VARCHAR(10), ROUND(100 / '+IntToStr(Max(DBObj.Rows,1))+' * COUNT(*), 1)) + ''%'' AS '+DBObj.Connection.QuoteIdent('Relevance')+' FROM '+DBObj.QuotedDatabase+'.'+DBObj.QuotedName+' WHERE '
                + SQL;
          end;
          AddResults(SQL, DBObj.Connection);
        end else
          AddNotes(DBObj, f_('%s%s doesn''t have columns of selected type (%s).', [STRSKIPPED, DBObj.ObjType, comboDatatypes.Text]), '');
      end;
    end;

    lntProcedure, lntFunction: begin
      SQL := 'SELECT '+
        esc(DBObj.Database)+' AS '+DBObj.Connection.QuoteIdent('Database')+', '+
        esc(DBObj.Name)+' AS '+DBObj.Connection.QuoteIdent('Table')+', '+
        DBObj.Connection.GetSQLSpecifity(spFuncCeil)+'(('+DBObj.Connection.GetSQLSpecifity(spFuncLength)+'('+RoutineDefinitionColumn+') - '+DBObj.Connection.GetSQLSpecifity(spFuncLength)+'(REPLACE('+RoutineDefinitionColumn+', '+esc(FindText)+', '+esc('')+'))) / '+DBObj.Connection.GetSQLSpecifity(spFuncLength)+'('+esc(FindText)+')) AS '+DBObj.Connection.QuoteIdent('Found rows')+', '+
        '0 AS '+DBObj.Connection.QuoteIdent('Relevance')+
        'FROM '+DBObj.Connection.QuoteIdent('information_schema')+'.'+DBObj.Connection.QuoteIdent('routines')+' '+
        'WHERE '+DBObj.Connection.QuoteIdent(RoutineSchemaColumn)+'='+esc(DBObj.Database)+' AND '+DBObj.Connection.QuoteIdent('routine_name')+'='+esc(DBObj.Name);
      AddResults(SQL, DBObj.Connection);
    end;

  end;

  Columns.Free;
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


procedure TfrmTableTools.AddResults(SQL: String; Connection: TDBConnection);
var
  i: Integer;
  Row: TStringList;
  Results: TDBQuery;
  Value: String;
begin
  // Execute query and append results into grid
  Results := Connection.GetResults(SQL);
  if Results = nil then
    Exit;

  SetupResultGrid(Results);
  Results.First;
  while not Results.Eof do begin
    Row := TStringList.Create;
    for i:=0 to Results.ColumnCount-1 do begin
      Value := Results.Col(i);
      if Results.DataType(i).Category = dtcInteger then begin
        if MakeFloat(Value) >= 0 then
          Row.Add(FormatNumber(Value))
        else
          Row.Add('');
      end else
        Row.Add(Value);
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


procedure TfrmTableTools.AddNotes(DBObject: TDBObject; Msg1, Msg2: String);
begin
  AddNotes(DBObject.Database, DBObject.Name, Msg1, Msg2);
end;


procedure TfrmTableTools.SetupResultGrid(Results: TDBQuery=nil);
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
      if Results.DataType(i).Category in [dtcInteger, dtcReal] then
        Col.Alignment := taRightJustify
      else
        Col.Alignment := taLeftJustify;
    end;
  end;
end;


procedure TfrmTableTools.UpdateResultGrid;
var
  Percent: Double;
begin
  // Refresh resultgrid
  ResultGrid.RootNodeCount := FResults.Count;
  ResultGrid.FocusedNode := ResultGrid.GetLast;
  ResultGrid.Selected[ResultGrid.FocusedNode] := True;
  Percent := 100 / Max(FObjectSizes,1) * FObjectSizesDoneExact;
  Percent := Min(Percent, 100);
  lblCheckedSize.Caption := f_('Selected objects size: %s', [FormatByteNumber(FObjectSizes)]) + '. ' +
    f_('%s%% done', [FormatNumber(Percent, 1)]) + '.';
  MainForm.SetProgressPosition(Round(Percent));
  Application.ProcessMessages;
end;

procedure TfrmTableTools.ResultGridCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
begin
  Mainform.AnyGridCompareNodes(Sender, Node1, Node2, Column, Result);
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
      TargetCanvas.Font.Color := GetThemeColor(clGrayText);
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
  Mainform.AnyGridHeaderClick(Sender, HitInfo);
end;


procedure TfrmTableTools.comboExportOutputTypeChange(Sender: TObject);
var
  SessionNode, DBNode: PVirtualNode;
  SessionName, FilenameHint: String;
  Params: TConnectionParameters;
  Placeholders: TStringList;
  i: Integer;
begin
  // Target type (file, directory, ...) selected
  comboExportOutputTarget.Enabled := True;
  comboExportOutputTarget.Text := '';
  comboExportOutputTarget.Hint := '';

  // Create filename placeholders hint
  Placeholders := GetOutputFilenamePlaceholders;
  FilenameHint := _('Allows the following replacement patterns:');
  for i:=0 to Placeholders.Count-1 do begin
    FilenameHint := FilenameHint + CRLF + '%' + Placeholders.Names[i] + ': ' + Placeholders.ValueFromIndex[i];
  end;
  Placeholders.Free;

  if Assigned(FTargetConnection) then
    FreeAndNil(FTargetConnection);
  if (comboExportOutputType.Text = OUTPUT_FILE)
    or (comboExportOutputType.Text = OUTPUT_FILE_COMPRESSED) then begin
    comboExportOutputTarget.Style := csDropDown;
    comboExportOutputTarget.Hint := FilenameHint;
    if comboExportOutputType.Text = OUTPUT_FILE then
      comboExportOutputTarget.Items.Text := AppSettings.ReadString(asExportSQLFilenames, '')
    else
      comboExportOutputTarget.Items.Text := AppSettings.ReadString(asExportZIPFilenames, '');
    if comboExportOutputTarget.Items.Count > 0 then
      comboExportOutputTarget.ItemIndex := 0;
    lblExportOutputTarget.Caption := _('Filename')+':';
    btnExportOutputTargetSelect.Enabled := True;
    btnExportOutputTargetSelect.ImageIndex := 51;
  end else if comboExportOutputType.Text = OUTPUT_DIR then begin
    comboExportOutputTarget.Style := csDropDown;
    comboExportOutputTarget.Hint := FilenameHint;
    comboExportOutputTarget.Items.Text := AppSettings.ReadString(asExportSQLDirectories, '');
    if comboExportOutputTarget.Items.Count > 0 then
      comboExportOutputTarget.ItemIndex := 0;
    lblExportOutputTarget.Caption := _('Directory')+':';
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
    lblExportOutputTarget.Caption := _('Database')+':';
    btnExportOutputTargetSelect.Enabled := False;
    btnExportOutputTargetSelect.ImageIndex := 27;
    // Add unchecked databases
    comboExportOutputTarget.Items.Clear;
    SessionNode := MainForm.GetRootNode(TreeObjects, MainForm.ActiveConnection);
    DBNode := TreeObjects.GetFirstChild(SessionNode);
    while Assigned(DBNode) do begin
      if DBNode.CheckState in [csUncheckedNormal, csUncheckedPressed] then
        comboExportOutputTarget.Items.Add(TreeObjects.Text[DBNode, 0]);
      DBNode := TreeObjects.GetNextSibling(DBNode);
    end;
    comboExportOutputTarget.ItemIndex := comboExportOutputTarget.Items.IndexOf(AppSettings.ReadString(asExportSQLDatabase));
    if comboExportOutputTarget.ItemIndex = -1 then
      comboExportOutputTarget.ItemIndex := 0;
  end else begin
    // Server selected. Display databases in below dropdown
    comboExportOutputTarget.Style := csDropDownList;
    lblExportOutputTarget.Caption := _('Database')+':';
    btnExportOutputTargetSelect.Enabled := False;
    btnExportOutputTargetSelect.ImageIndex := 27;
    SessionName := Copy(comboExportOutputType.Text, Length(OUTPUT_SERVER)+1, Length(comboExportOutputType.Text));
    FreeAndNil(FTargetConnection);
    Params := TConnectionParameters.Create(SessionName);
    FTargetConnection := Params.CreateConnection(Self);
    FTargetConnection.LogPrefix := SessionName;
    FTargetConnection.OnLog := Mainform.LogSQL;
    Screen.Cursor := crHourglass;
    try
      FTargetConnection.Active := True;
      comboExportOutputTarget.Items := FTargetConnection.AllDatabases;
      comboExportOutputTarget.Items.Insert(0, '['+_('Same as on source server')+']');
      comboExportOutputTarget.ItemIndex := comboExportOutputTarget.Items.IndexOf(AppSettings.ReadString(asExportSQLServerDatabase));
      if comboExportOutputTarget.ItemIndex = -1 then
        comboExportOutputTarget.ItemIndex := 0;
      Screen.Cursor := crDefault;
    except
      on E:EDbError do begin
        Screen.Cursor := crDefault;
        ErrorDialog(E.Message);
        comboExportOutputType.ItemIndex := FLastOutputSelectedIndex;
        comboExportOutputType.OnChange(Sender);
      end;
    end;
  end;

  FLastOutputSelectedIndex := comboExportOutputType.ItemIndex;
  chkExportDatabasesCreate.Enabled :=
    (comboExportOutputType.Text = OUTPUT_FILE)
    or (comboExportOutputType.Text = OUTPUT_FILE_COMPRESSED)
    or (comboExportOutputType.Text = OUTPUT_CLIPBOARD)
    or (Copy(comboExportOutputType.Text, 1, Length(OUTPUT_SERVER)) = OUTPUT_SERVER);
  chkExportDatabasesDrop.Enabled := chkExportDatabasesCreate.Enabled;
  ValidateControls(Sender);
end;


procedure TfrmTableTools.comboExportOutputTargetChange(Sender: TObject);
begin
  ValidateControls(Sender);
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
  WarnIfChecked(chkExportDatabasesDrop, _('Drop'));
  WarnIfChecked(chkExportTablesDrop, _('Drop'));
end;


procedure TfrmTableTools.btnCloseOrCancelClick(Sender: TObject);
begin
  // Set cancel flag to stop running loop in the next possible loop position
  if TButton(Sender).ModalResult = mrNone then begin
    FCancelled := True;
    Mainform.LogSQL(_('Processing cancelled by user, waiting for current object to finish ...'), lcInfo);
  end;
end;


procedure TfrmTableTools.btnExportOptionsClick(Sender: TObject);
var
  btn: TButton;
begin
  btn := Sender as TButton;
  btn.DropDownMenu.Popup(btn.ClientOrigin.X, btn.ClientOrigin.Y+btn.Height);
end;


procedure TfrmTableTools.btnExportOutputTargetSelectClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  Browse: TBrowseForFolder;
begin
  if (comboExportOutputType.Text = OUTPUT_FILE) or (comboExportOutputType.Text = OUTPUT_FILE_COMPRESSED) then begin
    // Select filename
    SaveDialog := TSaveDialog.Create(Self);
    SaveDialog.DefaultExt := 'sql';
    if comboExportOutputType.Text = OUTPUT_FILE then
      SaveDialog.Filter := _('SQL files')+' (*.sql)|*.sql|'+_('All files')+' (*.*)|*.*'
    else
      SaveDialog.Filter := _('ZIP files')+' (*.zip)|*.zip|'+_('All files')+' (*.*)|*.*';
    SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];
    if SaveDialog.Execute then
      comboExportOutputTarget.Text := SaveDialog.FileName;
    SaveDialog.Free;
  end else if(comboExportOutputType.Text = OUTPUT_DIR) then begin
    Browse := TBrowseForFolder.Create(Self);
    Browse.Folder := comboExportOutputTarget.Text;
    Browse.DialogCaption := _('Select output directory');
    // Enable "Create new folder" button
    Browse.BrowseOptions := Browse.BrowseOptions - [bifNoNewFolderButton] + [bifNewDialogStyle];
    if Browse.Execute then
      comboExportOutputTarget.Text := Browse.Folder;
    Browse.Free;
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
      if ToDB then MainForm.ActiveConnection.Query(SQL, False, lcScript)
      else if ToServer then FTargetConnection.Query(SQL, False, lcScript);
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
  StrucResult, Data: TDBQuery;
  rx: TRegExpr;
  ColumnList: TTableColumnList;
  Column: TTableColumn;
  Quoter: TDBConnection;
  TargetFileName, SetCharsetCode: String;
const
  TempDelim = '//';

  procedure LogStatistic(RowsDone: Int64);
  var
    LogRow: TStringlist;
    Percent: Double;
    BytesDone: Int64;
  begin
    LogRow := FResults.Last;
    Percent := 100 / Max(DBObj.Rows,1) * Max(RowsDone,1);
    Percent := Min(Percent, 100);
    BytesDone := Max(DBObj.Size,0) div Max(DBObj.Rows,1) * RowsDone;
    FObjectSizesDoneExact := FObjectSizesDone + BytesDone;
    LogRow[2] := FormatNumber(RowsDone) + ' / ' + FormatNumber(Percent, 0)+'%';
    LogRow[3] := FormatTimeNumber((GetTickCount-StartTime) / 1000, True);
    UpdateResultGrid;
  end;

begin
  // Handle one table, view or whatever in SQL export mode
  AddResults('SELECT '+esc(DBObj.Database)+' AS '+DBObj.Connection.QuoteIdent('Database')+', ' +
    esc(DBObj.Name)+' AS '+DBObj.Connection.QuoteIdent('Table')+', ' +
    IntToStr(DBObj.Rows)+' AS '+DBObj.Connection.QuoteIdent('Rows')+', '+
    '0 AS '+DBObj.Connection.QuoteIdent('Duration')
    , DBObj.Connection
    );
  ToFile := (comboExportOutputType.Text = OUTPUT_FILE) or (comboExportOutputType.Text = OUTPUT_FILE_COMPRESSED);
  ToDir := comboExportOutputType.Text = OUTPUT_DIR;
  ToClipboard := comboExportOutputType.Text = OUTPUT_CLIPBOARD;
  ToDb := comboExportOutputType.Text = OUTPUT_DB;
  ToServer := Copy(comboExportOutputType.Text, 1, Length(OUTPUT_SERVER)) = OUTPUT_SERVER;
  if not Assigned(ExportStream) then begin
    // Very first round here. Prevent "SHOW CREATE db|table" from using double quotes
    DBObj.Connection.Query('/*!40101 SET @OLD_LOCAL_SQL_MODE=@@SQL_MODE, SQL_MODE='''' */');
  end;

  if ToServer then
    Quoter := FTargetConnection
  else
    Quoter := DBObj.Connection;

  StartTime := GetTickCount;
  ExportStreamStartOfQueryPos := 0;
  if ToDir then begin
    FreeAndNil(ExportStream);
    DbDir := IncludeTrailingPathDelimiter(GetOutputFilename(comboExportOutputTarget.Text, DBObj)) + DBObj.Database + '\';
    if not DirectoryExists(DbDir) then
      ForceDirectories(DbDir);
    ExportStream := TFileStream.Create(DbDir + DBObj.Name+'.sql', fmCreate or fmOpenWrite);
    FHeaderCreated := False;
  end;
  if not Assigned(ExportStream) then begin
    if ToFile then begin
      TargetFileName := GetOutputFilename(comboExportOutputTarget.Text, DBObj);
      FExportFileName := TargetFileName;
      if comboExportOutputType.Text = OUTPUT_FILE_COMPRESSED then
        TargetFileName := ChangeFileExt(TargetFileName, '_temp.sql');
      if not IsValidFilePath(TargetFileName) then
        raise EFCreateError.CreateFmt(_('Filename or path contains illegal characters: "%s"'), [TargetFilename]);
      ExportStream := TFileStream.Create(TargetFileName, fmCreate or fmOpenWrite);
    end;
    // ToDir handled above
    if ToClipboard then
      ExportStream := TMemoryStream.Create;
    if ToDb or ToServer then
      ExportStream := TMemoryStream.Create;
  end;
  if not FHeaderCreated then begin
    if DBObj.Connection.CharacterSet = 'utf8mb4' then
      SetCharsetCode := '/*!40101 SET NAMES utf8 */;' + CRLF +
        '/*!50503 SET NAMES '+DBObj.Connection.CharacterSet+' */;' + CRLF
    else
      SetCharsetCode := '/*!40101 SET NAMES '+DBObj.Connection.CharacterSet+' */;' + CRLF;
    Header := '';
    if menuExportAddComments.Checked then begin
      Header := Header +
        '-- --------------------------------------------------------' + CRLF +
        Format('-- %-30s%s', [_('Host')+':', DBObj.Connection.Parameters.HostName]) + CRLF +
        Format('-- %-30s%s', [_('Server version')+':', DBObj.Connection.ServerVersionUntouched]) + CRLF +
        Format('-- %-30s%s', [_('Server OS')+':', DBObj.Connection.ServerOS]) + CRLF +
        Format('-- %-30s%s', [APPNAME + ' ' + _('Version')+':', Mainform.AppVersion]) + CRLF +
        '-- --------------------------------------------------------' + CRLF + CRLF;
    end;
    Header := Header +
      '/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;' + CRLF +
      SetCharsetCode +
      '/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;' + CRLF +
      '/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE=''NO_AUTO_VALUE_ON_ZERO'' */;' + CRLF;
    Output(Header, False, DBObj.Database<>ExportLastDatabase, True, False, False);
    Output('/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */', True, False, False, True, True);
    Output('/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE=''NO_AUTO_VALUE_ON_ZERO'' */', True, False, False, True, True);
    Output(CRLF, False, True, True, False, False);
    FHeaderCreated := True;
  end;

  // Database structure. Do that only in single-file and server mode. drop/create/use in directory or database mode makes no sense
  FinalDbName := DBObj.Database;
  if ToDb or (ToServer and (comboExportOutputTarget.ItemIndex > 0)) then
    FinalDbName := comboExportOutputTarget.Text;
  NeedsDBStructure := FinalDbName <> ExportLastDatabase;
  if chkExportDatabasesDrop.Checked or chkExportDatabasesCreate.Checked then begin
    if menuExportAddComments.Checked then
      Output(CRLF+'-- '+f_('Dumping database structure for %s', [DBObj.Database])+CRLF, False, NeedsDBStructure, False, False, False);
    if chkExportDatabasesDrop.Checked and chkExportDatabasesDrop.Enabled then
      Output('DROP DATABASE IF EXISTS '+Quoter.QuoteIdent(FinalDbName), True, NeedsDBStructure, False, False, NeedsDBStructure);
    if chkExportDatabasesCreate.Checked and chkExportDatabasesCreate.Enabled then begin
      if DBObj.Connection.ServerVersionInt >= 40100 then begin
        Struc := DBObj.Connection.GetVar('SHOW CREATE DATABASE '+DBObj.QuotedDatabase, 1);
        // Gracefully ignore it when target database exists, important in server mode
        Insert('IF NOT EXISTS ', Struc, Pos('DATABASE', Struc) + 9);
        // Create the right dbname
        Struc := StringReplace(Struc, DBObj.Database, FinalDbName, []);
      end else
        Struc := 'CREATE DATABASE IF NOT EXISTS '+Quoter.QuoteIdent(FinalDbName);
      Output(Struc, True, NeedsDBStructure, False, False, NeedsDBStructure);
      Output('USE '+Quoter.QuoteIdent(FinalDbName), True, NeedsDBStructure, False, False, NeedsDBStructure);
      Output(CRLF, False, NeedsDBStructure, False, False, NeedsDBStructure);
    end;
  end;
  if ToServer and (not chkExportDatabasesCreate.Checked) then begin
    // Export to server without "CREATE/USE dbname" and "Same dbs as on source server" - needs a "USE dbname"
    Output('USE '+Quoter.QuoteIdent(FinalDbName), True, False, False, False, NeedsDBStructure);
  end;

  // Table structure
  if chkExportTablesDrop.Checked or chkExportTablesCreate.Checked then begin
    if menuExportAddComments.Checked then
      Output('-- '+f_('Dumping structure for %s %s.%s', [_(LowerCase(DBObj.ObjType)), DBObj.Database, DBObj.Name])+CRLF, False, True, True, False, False);
    if chkExportTablesDrop.Checked then begin
      Struc := 'DROP '+UpperCase(DBObj.ObjType)+' IF EXISTS ';
      if ToDb then
        Struc := Struc + Quoter.QuoteIdent(FinalDbName)+'.';
      Struc := Struc + Quoter.QuoteIdent(DBObj.Name);
      Output(Struc, True, True, True, True, True);
    end;
    if chkExportTablesCreate.Checked then begin
      try
        case DBObj.NodeType of
          lntTable: begin
            Struc := DBObj.CreateCode;
            // Remove AUTO_INCREMENT clause
            if menuExportRemoveAutoIncrement.Checked then begin
              rx := TRegExpr.Create;
              rx.ModifierI := True;
              rx.Expression := '\sAUTO_INCREMENT\s*\=\s*\d+\s';
              Struc := rx.Replace(Struc, ' ', false);
              rx.Free;
            end;
            Insert('IF NOT EXISTS ', Struc, Pos('TABLE', Struc) + 6);
            if ToDb then
              Insert(Quoter.QuoteIdent(FinalDbName)+'.', Struc, Pos('EXISTS', Struc) + 7 );
            if ToServer then begin
              rx := TRegExpr.Create;
              rx.ModifierI := True;
              rx.Expression := '(\s)(TYPE|ENGINE)(\=|\s+)(\w+)';
              if FTargetConnection.ServerVersionInt < 40018 then
                Struc := rx.Replace(Struc, '${1}TYPE${3}${4}', true)
              else
                Struc := rx.Replace(Struc, '${1}ENGINE${3}${4}', true);
              rx.Free;
            end;
          end;

          lntView: begin
            if not FSecondExportPass then begin
              // Create temporary VIEW replacement
              ColumnList := TTableColumnList.Create(True);
              DBObj.Connection.ParseViewStructure(DBObj.CreateCode, DBObj, ColumnList, Dummy, Dummy, Dummy, Dummy, Dummy);
              Struc := '';
              if menuExportAddComments.Checked then
                Struc := Struc + '-- '+_('Creating temporary table to overcome VIEW dependency errors')+CRLF;
              Struc := Struc + 'CREATE TABLE ';
              if ToDb then
                Struc := Struc + Quoter.QuoteIdent(FinalDbName) + '.';
              Struc := Struc + Quoter.QuoteIdent(DBObj.Name)+' (';
              for Column in ColumnList do begin
                // Prevent DEFAULT value from coming in, to fix errors due to multiple CURRENT_TIMESTAMP values
                // See issue #2748
                Column.DefaultType := cdtNothing;
                Struc := Struc + CRLF + #9 + Column.SQLCode + ',';
              end;
              Delete(Struc, Length(Struc), 1);
              Struc := Struc + CRLF + ') ENGINE=MyISAM';
              ColumnList.Free;
            end else begin
              Struc := '';
              if menuExportAddComments.Checked then
                Struc := Struc + '-- '+_('Removing temporary table and create final VIEW structure')+CRLF;
              Struc := Struc + 'DROP TABLE IF EXISTS ';
              if ToDb then
                Struc := Struc + Quoter.QuoteIdent(FinalDbName)+'.';
              Struc := Struc + Quoter.QuoteIdent(DBObj.Name);
              Output(Struc, True, True, True, True, True);
              Struc := DBObj.CreateCode;
              if ToDb then
                Insert(Quoter.QuoteIdent(FinalDbName)+'.', Struc, Pos('VIEW', Struc) + 5 );
            end;
          end;

          lntTrigger: begin
            StrucResult := DBObj.Connection.GetResults('SHOW TRIGGERS FROM '+DBObj.QuotedDatabase+' WHERE `Trigger`='+esc(DBObj.Name));
            Struc := 'CREATE '+UpperCase(DBObj.ObjType)+' '+Quoter.QuoteIdent(DBObj.Name)+' '+StrucResult.Col('Timing')+' '+StrucResult.Col('Event')+
                ' ON '+Quoter.QuoteIdent(StrucResult.Col('Table'))+' FOR EACH ROW '+StrucResult.Col('Statement');
            if ToDb then
              Insert(Quoter.QuoteIdent(FinalDbName)+'.', Struc, Pos('TRIGGER', Struc) + 8 );
            if ToFile or ToClipboard or ToDir then begin
              Struc := 'SET @OLDTMP_SQL_MODE=@@SQL_MODE, SQL_MODE=' + esc(StrucResult.Col('sql_mode')) + ';' + CRLF +
                'DELIMITER ' + TempDelim + CRLF +
                Struc + TempDelim + CRLF +
                'DELIMITER ;' + CRLF +
                'SET SQL_MODE=@OLDTMP_SQL_MODE';
            end;
          end;

          lntFunction, lntProcedure: begin
            Struc := DBObj.CreateCode;
            if ToDb then begin
              if DBObj.NodeType = lntProcedure then
                Insert(Quoter.QuoteIdent(FinalDbName)+'.', Struc, Pos('PROCEDURE', Struc) + 10 )
              else if DBObj.NodeType = lntFunction then
                Insert(Quoter.QuoteIdent(FinalDbName)+'.', Struc, Pos('FUNCTION', Struc) + 9 );
            end;
            // Change delimiter for file output, so readers split queries at the right string position
            if ToFile or ToDir or ToClipboard then
              Struc := 'DELIMITER ' + TempDelim + CRLF + Struc + TempDelim + CRLF + 'DELIMITER ';
          end;

          lntEvent: begin
            Struc := DBObj.CreateCode;
            if ToDb then
              Insert(Quoter.QuoteIdent(FinalDbName)+'.', Struc, Pos('EVENT', Struc) + 6 );
            if ToFile or ToDir or ToClipboard then
              Struc := 'DELIMITER ' + TempDelim + CRLF + Struc + TempDelim + CRLF + 'DELIMITER ';
          end;
        end;
        Struc := fixNewlines(Struc);
        Output(Struc, True, True, True, True, True);
        Output(CRLF, False, True, True, True, True);
      except
        on E:EDbError do begin
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
      if menuExportAddComments.Checked then
        Output('-- '+_('Data exporting was unselected.')+CRLF+CRLF, False, True, True, False, False);
    end else if MatchText(DBObj.Engine, ['MRG_MYISAM', 'FEDERATED']) then begin
      if menuExportAddComments.Checked then
        Output('-- '+f_('Table data not exported because this is %s table which holds its data in separate tables.', [DBObj.Engine])+CRLF+CRLF, False, True, True, False, False);
    end else begin
      tmp := FormatNumber(DBObj.Rows)+' rows';
      if LowerCase(DBObj.Engine) = 'innodb' then
        tmp := '~'+tmp+' ('+_('approximately')+')';
      if menuExportAddComments.Checked then
        Output('-- '+f_('Dumping data for table %s.%s: %s', [DBObj.Database, DBObj.Name, tmp])+CRLF, False, True, True, False, False);
      TargetDbAndObject := Quoter.QuoteIdent(DBObj.Name);
      if ToDb then
        TargetDbAndObject := Quoter.QuoteIdent(FinalDbName) + '.' + TargetDbAndObject;
      Offset := 0;
      RowCount := 0;
      // Calculate limit so we select ~100MB per loop
      Limit := Round(100 * SIZE_MB / Max(DBObj.AvgRowLen,1));
      if comboExportData.Text = DATA_REPLACE then
        Output('DELETE FROM '+TargetDbAndObject, True, True, True, True, True);
      Output('/*!40000 ALTER TABLE '+TargetDbAndObject+' DISABLE KEYS */', True, True, True, True, True);
      while true do begin
        Data := DBObj.Connection.GetResults(
          DBObj.Connection.ApplyLimitClause(
            'SELECT',
            '* FROM '+DBObj.QuotedDbAndTableName,
            Limit,
            Offset)
          );
        Inc(Offset, Limit);
        if Data.RecordCount = 0 then
          break;
        Data.PrepareColumnAttributes;
        BaseInsert := 'INSERT INTO ';
        if comboExportData.Text = DATA_INSERTNEW then
          BaseInsert := 'INSERT IGNORE INTO '
        else if comboExportData.Text = DATA_UPDATE then
          BaseInsert := 'REPLACE INTO ';
        BaseInsert := BaseInsert + TargetDbAndObject + ' (';
        for i:=0 to Data.ColumnCount-1 do begin
          if not Data.ColIsVirtual(i) then
            BaseInsert := BaseInsert + Quoter.QuoteIdent(Data.ColumnNames[i]) + ', ';
        end;
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
              if Data.ColIsVirtual(i) then
                Continue;
              if Data.IsNull(i) then
                Row := Row + 'NULL'
              else case Data.DataType(i).Category of
                dtcInteger, dtcReal: begin
                  if Data.DataType(i).Index = dtBit then
                    Row := Row + 'b' + Quoter.EscapeString(Data.Col(i))
                  else
                    Row := Row + Data.Col(i);
                end;
                dtcBinary, dtcSpatial: begin
                  BinContent := Data.HexValue(i);
                  if Length(BinContent) > 0 then
                    Row := Row + '_binary ' + BinContent
                  else
                    Row := Row + esc('');
                end;
                else Row := Row + esc(Data.Col(i));
              end;
              Row := Row + ', ';
            end;
            Delete(Row, Length(Row)-1, 2);
            Row := Row + ')';
            // Break if stream would increase over the barrier of 1MB, and throw away current row
            if (not IsFirstRowInChunk)
              and (ExportStream.Size - ExportStreamStartOfQueryPos + Length(Row) > updownInsertSize.Position*SIZE_KB*0.9)
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
      Output(CRLF, False, True, True, True, True);
      // Cosmetic fix for estimated InnoDB row count
      DBObj.Rows := RowCount;
      LogStatistic(RowCount);
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
  AddResults('SELECT '+esc(DBObj.Database)+' AS '+DBObj.Connection.QuoteIdent('Database')+', ' +
    esc(DBObj.Name)+' AS '+DBObj.Connection.QuoteIdent('Table')+', ' +
    esc('Updating...')+' AS '+DBObj.Connection.QuoteIdent('Operation')+', '+
    ''''' AS '+DBObj.Connection.QuoteIdent('Result')
    , DBObj.Connection
    );
  Specs := TStringList.Create;
  if chkBulkTableEditDatabase.Checked and (comboBulkTableEditDatabase.Text <> DBObj.Database) then begin
    case DBObj.NodeType of
      lntTable: Specs.Add('RENAME ' + DBObj.Connection.QuoteIdent(comboBulkTableEditDatabase.Text)+'.'+DBObj.QuotedName);
      lntView: begin
        // Although RENAME works for views, that does not work for moving to another database without getting
        // a "Changing schema from x to y is not allowed". Instead, recreate them manually
        CreateView := DBObj.CreateCode;
        rx := TRegExpr.Create;
        rx.ModifierI := True;
        // Replace old database references in VIEW body
        rx.Expression := '(["`])'+QuoteRegExprMetaChars(DBObj.Database)+'(["`])';
        CreateView := rx.Replace(CreateView, DBObj.Connection.QuoteIdent(comboBulkTableEditDatabase.Text), false);
        rx.Free;
        // Temporarily switch to new database for VIEW creation, so the database references are correct
        DBObj.Connection.Database := comboBulkTableEditDatabase.Text;
        DBObj.Connection.Query(CreateView);
        DBObj.Connection.Database := DBObj.Database;
        DBObj.Connection.Query('DROP VIEW '+DBObj.QuotedName);
      end;
    end;
    if FModifiedDbs.IndexOf(DBObj.Database) = -1 then
      FModifiedDbs.Add(DBObj.Database);
    if FModifiedDbs.IndexOf(comboBulkTableEditDatabase.Text) = -1 then
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
        Specs[Specs.Count-1] := Specs[Specs.Count-1] + ' COLLATE '+esc(comboBulkTableEditCollation.Text)
      else
        Specs.Add('COLLATE '+esc(comboBulkTableEditCollation.Text));
    end;
    if chkBulkTableEditResetAutoinc.Checked then
      Specs.Add('AUTO_INCREMENT=0');
  end;

  LogRow := FResults.Last;
  if Specs.Count > 0 then begin
    DBObj.Connection.Query('ALTER TABLE ' + DBObj.QuotedDatabase + '.' + DBObj.QuotedName + ' ' + ImplodeStr(', ', Specs));
    LogRow[2] := _('Done');
    LogRow[3] := _('Success');
  end else begin
    LogRow[2] := _('Nothing to do');
    LogRow[3] := f_('Selected operations cannot be applied to a %s', [_(LowerCase(DBObj.ObjType))]);
  end;
  UpdateResultGrid;
end;


procedure TfrmTableTools.CheckAllClick(Sender: TObject);
var
  DBNode, ObjNode: PVirtualNode;
  WantedType: TListNodeType;
  DBObj: PDBObject;
  CheckNone: Boolean;
  CheckedNodes: Int64;
begin
  // Check all/none/by type via context menu
  WantedType := TListNodeType((Sender as TMenuItem).Tag);
  CheckNone := Sender = menuCheckNone;
  case TreeObjects.GetNodeLevel(TreeObjects.FocusedNode) of
    1: DBNode := TreeObjects.FocusedNode;
    2: DBNode := TreeObjects.FocusedNode.Parent;
    3: DBNode := TreeObjects.FocusedNode.Parent.Parent;
    else raise Exception.Create(_('Unhandled tree level'));
  end;
  ObjNode := TreeObjects.GetFirstChild(DBNode);
  CheckedNodes := 0;
  while Assigned(ObjNode) do begin
    DBObj := TreeObjects.GetNodeData(ObjNode);
    if CheckNone then
      TreeObjects.CheckState[ObjNode] := csUncheckedNormal
    else begin
      if (WantedType = lntNone) or (DBObj.NodeType = WantedType) or (DBObj.GroupType = WantedType) then
        TreeObjects.CheckState[ObjNode] := csCheckedNormal
      else
        TreeObjects.CheckState[ObjNode] := csUncheckedNormal;
    end;
    if ObjNode.CheckState = csCheckedNormal then
      Inc(CheckedNodes);
    TreeObjects.RepaintNode(ObjNode);
    ObjNode := TreeObjects.GetNextSibling(ObjNode);
  end;
  if CheckedNodes = 0 then
    TreeObjects.CheckState[DBNode] := csUncheckedNormal
  else if CheckedNodes = TreeObjects.ChildCount[DBNode] then
    TreeObjects.CheckState[DBNode] := csCheckedNormal
  else
    TreeObjects.CheckState[DBNode] := csMixedNormal;
end;


end.
