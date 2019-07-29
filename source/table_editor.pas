unit table_editor;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, ToolWin, VirtualTrees, SynRegExpr, ActiveX, ExtCtrls, SynEdit,
  SynMemo, Menus, Clipbrd, Math, System.UITypes,
  grideditlinks, dbstructures, dbconnection, apphelpers, gnugettext, StrUtils;

type
  TFrame = TDBObjectEditor;
  TfrmTableEditor = class(TFrame)
    btnSave: TButton;
    btnDiscard: TButton;
    btnHelp: TButton;
    listColumns: TVirtualStringTree;
    PageControlMain: TPageControl;
    tabBasic: TTabSheet;
    tabIndexes: TTabSheet;
    tabOptions: TTabSheet;
    lblName: TLabel;
    editName: TEdit;
    memoComment: TMemo;
    lblComment: TLabel;
    lblAutoinc: TLabel;
    lblAvgRowLen: TLabel;
    lblInsertMethod: TLabel;
    lblUnion: TLabel;
    lblMaxRows: TLabel;
    lblRowFormat: TLabel;
    editAutoInc: TEdit;
    editAvgRowLen: TEdit;
    editMaxRows: TEdit;
    chkChecksum: TCheckBox;
    comboRowFormat: TComboBox;
    memoUnionTables: TMemo;
    comboInsertMethod: TComboBox;
    lblCollation: TLabel;
    comboCollation: TComboBox;
    lblEngine: TLabel;
    comboEngine: TComboBox;
    treeIndexes: TVirtualStringTree;
    tlbIndexes: TToolBar;
    btnAddIndex: TToolButton;
    btnRemoveIndex: TToolButton;
    btnClearIndexes: TToolButton;
    btnMoveUpIndex: TToolButton;
    btnMoveDownIndex: TToolButton;
    pnlColumnsTop: TPanel;
    tlbColumns: TToolBar;
    btnAddColumn: TToolButton;
    btnRemoveColumn: TToolButton;
    btnMoveUpColumn: TToolButton;
    btnMoveDownColumn: TToolButton;
    SplitterTopBottom: TSplitter;
    tabCREATEcode: TTabSheet;
    tabALTERCode: TTabSheet;
    SynMemoCREATEcode: TSynMemo;
    SynMemoALTERcode: TSynMemo;
    popupIndexes: TPopupMenu;
    menuAddIndex: TMenuItem;
    menuAddIndexColumn: TMenuItem;
    menuRemoveIndex: TMenuItem;
    menuMoveUpIndex: TMenuItem;
    menuMoveDownIndex: TMenuItem;
    menuClearIndexes: TMenuItem;
    popupColumns: TPopupMenu;
    menuAddColumn: TMenuItem;
    menuRemoveColumn: TMenuItem;
    menuMoveUpColumn: TMenuItem;
    menuMoveDownColumn: TMenuItem;
    chkCharsetConvert: TCheckBox;
    N1: TMenuItem;
    menuCreateIndex: TMenuItem;
    menuAddToIndex: TMenuItem;
    tabForeignKeys: TTabSheet;
    tlbForeignKeys: TToolBar;
    btnAddForeignKey: TToolButton;
    btnRemoveForeignKey: TToolButton;
    btnClearForeignKeys: TToolButton;
    menuCopyColumnCell: TMenuItem;
    N2: TMenuItem;
    listForeignKeys: TVirtualStringTree;
    menuCopyColumns: TMenuItem;
    menuPasteColumns: TMenuItem;
    tabPartitions: TTabSheet;
    SynMemoPartitions: TSynMemo;
    procedure Modification(Sender: TObject);
    procedure btnAddColumnClick(Sender: TObject);
    procedure btnRemoveColumnClick(Sender: TObject);
    procedure listColumnsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure btnHelpClick(Sender: TObject);
    procedure listColumnsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure listColumnsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure listColumnsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: String);
    procedure btnMoveUpColumnClick(Sender: TObject);
    procedure btnMoveDownColumnClick(Sender: TObject);
    procedure listColumnsDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
		  Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure listColumnsDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
		  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure listColumnsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
		  Column: TColumnIndex; TextType: TVSTTextType);
    procedure listColumnsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure treeIndexesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure treeIndexesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
		  var CellText: String);
    procedure treeIndexesBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure treeIndexesInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure treeIndexesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure btnClearIndexesClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure editNumEditChange(Sender: TObject);
    procedure comboEngineSelect(Sender: TObject);
    procedure listColumnsClick(Sender: TObject);
    procedure listColumnsBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure listColumnsAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure btnAddIndexClick(Sender: TObject);
    procedure treeIndexesDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure treeIndexesDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure treeIndexesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: String);
    procedure treeIndexesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure treeIndexesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure treeIndexesCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure btnMoveUpIndexClick(Sender: TObject);
    procedure btnMoveDownIndexClick(Sender: TObject);
    procedure btnRemoveIndexClick(Sender: TObject);
    procedure menuAddIndexColumnClick(Sender: TObject);
    procedure PageControlMainChange(Sender: TObject);
    procedure chkCharsetConvertClick(Sender: TObject);
    procedure treeIndexesClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure popupColumnsPopup(Sender: TObject);
    procedure AddIndexByColumn(Sender: TObject);
    procedure listForeignKeysBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure listForeignKeysCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure listForeignKeysFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure listForeignKeysGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure listForeignKeysGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure listForeignKeysNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: String);
    procedure btnClearForeignKeysClick(Sender: TObject);
    procedure btnAddForeignKeyClick(Sender: TObject);
    procedure btnRemoveForeignKeyClick(Sender: TObject);
    procedure listForeignKeysEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure listColumnsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure listColumnsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure listColumnsNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure listColumnsKeyPress(Sender: TObject; var Key: Char);
    procedure vtHandleClickOrKeyPress(Sender: TVirtualStringTree;
      Node: PVirtualNode; Column: TColumnIndex; HitPositions: THitPositions);
    procedure menuCopyColumnsClick(Sender: TObject);
    procedure menuPasteColumnsClick(Sender: TObject);
  private
    { Private declarations }
    FLoaded: Boolean;
    CreateCodeValid, AlterCodeValid: Boolean;
    FColumns: TTableColumnList;
    FKeys: TTableKeyList;
    FForeignKeys: TForeignKeyList;
    DeletedKeys, DeletedForeignKeys: TStringList;
    procedure ValidateColumnControls;
    procedure ValidateIndexControls;
    procedure MoveFocusedIndexPart(NewIdx: Cardinal);
    procedure ResetModificationFlags;
    function ComposeCreateStatement: TSQLBatch;
    function ComposeAlterStatement: TSQLBatch;
    procedure UpdateSQLcode;
    function CellEditingAllowed(Node: PVirtualNode; Column: TColumnIndex): Boolean;
    procedure CalcMinColWidth;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(Obj: TDBObject); override;
    function ApplyModifications: TModalResult; override;
  end;


implementation

uses main;


{$R *.dfm}


constructor TfrmTableEditor.Create(AOwner: TComponent);
begin
  inherited;
  FixVT(listColumns);
  FixVT(treeIndexes);
  FixVT(listForeignKeys);
  // Try the best to auto fit various column widths, respecting a custom DPI setting and a pulldown arrow
  listColumns.Header.Columns[2].Width := Mainform.Canvas.TextWidth('GEOMETRYCOLLECTION') + 6*listColumns.TextMargin;
  listColumns.Header.Columns[7].Width := Mainform.Canvas.TextWidth('AUTO_INCREMENT') + 4*listColumns.TextMargin;
  listColumns.Header.Columns[9].Width := Mainform.Canvas.TextWidth('macroman_general_ci') + 6*listColumns.TextMargin;
  // Overide column widths by custom values
  Mainform.RestoreListSetup(listColumns);
  Mainform.RestoreListSetup(treeIndexes);
  Mainform.RestoreListSetup(listForeignKeys);
  comboRowFormat.Items.CommaText := 'DEFAULT,DYNAMIC,FIXED,COMPRESSED,REDUNDANT,COMPACT';
  comboInsertMethod.Items.CommaText := 'NO,FIRST,LAST';
  FColumns := TTableColumnList.Create;
  FKeys := TTableKeyList.Create;
  FForeignKeys := TForeignKeyList.Create;
  DeletedKeys := TStringList.Create;
  DeletedForeignKeys := TStringList.Create;
  editName.MaxLength := NAME_LEN;
end;


destructor TfrmTableEditor.Destroy;
begin
  // Store GUI setup
  Mainform.SaveListSetup(listColumns);
  Mainform.SaveListSetup(treeIndexes);
  Mainform.SaveListSetup(listForeignKeys);
  inherited;
end;


procedure TfrmTableEditor.Init(Obj: TDBObject);
var
  AttrName, AttrValue: String;
  rx: TRegExpr;
begin
  inherited;

  FLoaded := False;
  comboEngine.Items := DBObject.Connection.TableEngines;
  comboEngine.Items.Insert(0, '<'+_('Server default')+'>');
  comboEngine.ItemIndex := 0;
  comboCollation.Items := DBObject.Connection.CollationList;
  if DBObject.Connection.Parameters.IsMariaDB then begin
    with listColumns.Header do begin
      Columns[10].Options := Columns[10].Options + [coVisible];
      Columns[11].Options := Columns[11].Options + [coVisible];
    end;
  end;
  listColumns.BeginUpdate;
  FColumns.Clear;
  btnClearIndexesClick(Self);
  btnClearForeignKeysClick(Self);
  tabALTERcode.TabVisible := DBObject.Name <> '';
  // Clear value editors
  memoComment.Text := '';
  if Obj.Connection.ServerVersionInt < 50503 then
    memoComment.MaxLength := 60
  else
    memoComment.MaxLength := 2048;
  editAutoInc.Text := '';
  editAvgRowLen.Text := '';
  editMaxRows.Text := '';
  chkChecksum.Checked := False;
  comboRowFormat.ItemIndex := 0;
  comboCollation.ItemIndex := -1;
  memoUnionTables.Clear;
  comboInsertMethod.ItemIndex := -1;
  SynMemoPartitions.Clear;

  if DBObject.Name = '' then begin
    // Creating new table
    editName.Text := '';
    if DBObject.Connection.Parameters.IsMySQL then
      comboCollation.ItemIndex := comboCollation.Items.IndexOf(DBObject.Connection.GetSessionVariable('collation_database'));
    PageControlMain.ActivePage := tabBasic;
  end else begin
    // Editing existing table
    editName.Text := DBObject.Name;
    // Try collation from SHOW TABLE STATUS, sometimes missing in SHOW CREATE TABLE result
    comboCollation.ItemIndex := comboCollation.Items.IndexOf(DBObject.Collation);
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    rx.Expression := '\s(\S+)\s*=\s*(\S+)';
    if rx.Exec(DBObject.CreateCode) then while true do begin
      AttrName := UpperCase(rx.Match[1]);
      AttrValue := rx.Match[2];
      if (AttrName='ENGINE') or (AttrName='TYPE') then
        comboEngine.ItemIndex := comboEngine.Items.IndexOf(AttrValue)
      else if AttrName='COLLATE' then
        comboCollation.ItemIndex := comboCollation.Items.IndexOf(AttrValue)
      else if AttrName='AVG_ROW_LENGTH' then
        editAvgRowLen.Text := AttrValue
      else if AttrName='AUTO_INCREMENT' then
        editAutoInc.Text := AttrValue
      else if AttrName='ROW_FORMAT' then
        comboRowFormat.ItemIndex := comboRowFormat.Items.IndexOf(AttrValue)
      else if AttrName='CHECKSUM' then
        chkChecksum.Checked := AttrValue='1'
      else if AttrName='MAX_ROWS' then
        editMaxRows.Text := AttrValue
      else if AttrName='INSERT_METHOD' then
        comboInsertMethod.ItemIndex := comboInsertMethod.Items.IndexOf(AttrValue);
      if not rx.ExecNext then
        break;
    end;

    rx.Expression := '\bUNION=\((.+)\)';
    if rx.Exec(DBObject.CreateCode) then
      memoUnionTables.Lines.Text := rx.Match[1]
    else
      memoUnionTables.Lines.Clear;

    // Prefer to take comment from SHOW TABLE STATUS result, to support single quotes without including some create option
    // See issue #196
    memoComment.Text := DBObject.Comment;

    rx.Expression := '\b(PARTITION\s+.+)(\*/)';
    if rx.Exec(DBObject.CreateCode) then
      SynMemoPartitions.Text := rx.Match[1]
    else
      SynMemoPartitions.Clear;

    DBObject.Connection.ParseTableStructure(DBObject.CreateCode, FColumns, FKeys, FForeignKeys);
  end;
  listColumns.RootNodeCount := FColumns.Count;
  DeInitializeVTNodes(listColumns);
  listColumns.EndUpdate;

  // Set root nodes per BeforePaint event:
  treeIndexes.Invalidate;
  listForeignKeys.Invalidate;

  // Validate controls
  comboEngineSelect(comboEngine);
  ValidateColumnControls;
  ValidateIndexControls;
  ResetModificationFlags;
  CreateCodeValid := False;
  AlterCodeValid := False;
  PageControlMainChange(Self); // Foreign key editor needs a hit
  // Buttons are randomly moved, since VirtualTree update, see #440
  btnSave.Top := Height - btnSave.Height - 3;
  btnHelp.Top := btnSave.Top;
  btnDiscard.Top := btnSave.Top;
  UpdateSQLCode;
  CalcMinColWidth;
  // Indicate change mechanisms can call their events now. See Modification().
  FLoaded := True;
  // Empty status panel
  Mainform.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


procedure TfrmTableEditor.btnDiscardClick(Sender: TObject);
begin
  // Reinit GUI, discarding changes
  Modified := False;
  Init(DBObject);
end;


procedure TfrmTableEditor.btnSaveClick(Sender: TObject);
begin
  // Save changes, and make it impossible to (accidentally) click the save button twice
  btnSave.Enabled := False;
  btnSave.Repaint;
  if ApplyModifications = mrOK then
    Init(DBObject)
  else // Re-enable save button when something went wrong
    btnSave.Enabled := True;
end;


function TfrmTableEditor.ApplyModifications: TModalResult;
var
  Batch: TSQLBatch;
  Query: TSQLSentence;
  i: Integer;
  Rename: String;
begin
  // Check if all indexes have at least one column
  // If not, exit early
  for i := 0 to FKeys.Count-1 do begin
    if FKeys.Items[i].Columns.Count = 0 then begin
      ErrorDialog( f_('%s Index "%s" does not contain any column. You can add columns using drag''n drop from the columns list.', [FKeys.Items[i].IndexType, FKeys.Items[i].Name]));
      Result := mrAbort;
      Exit;
    end;
  end;

  // Create or alter table
  Result := mrOk;

  if DBObject.Name = '' then
    Batch := ComposeCreateStatement
  else
    Batch := ComposeAlterStatement;
  try
    for Query in Batch do
      DBObject.Connection.Query(Query.SQL);
    // Rename table
    if (DBObject.Name <> '') and (editName.Text <> DBObject.Name) then begin
      Rename := DBObject.Connection.GetSQLSpecifity(spRenameTable);
      Rename := Format(Rename, [DBObject.QuotedName, DBObject.Connection.QuoteIdent(editName.Text)]);
      DBObject.Connection.Query(Rename);
    end;
    tabALTERcode.TabVisible := DBObject.Name <> '';
    if chkCharsetConvert.Checked then begin
      // Autoadjust column collations
      for i:=0 to FColumns.Count-1 do begin
        if FColumns[i].Collation <> '' then
          FColumns[i].Collation := comboCollation.Text;
      end;
    end;
    // Set table name for altering if Apply was clicked
    DBObject.Name := editName.Text;
    DBObject.CreateCode := '';
    tabALTERcode.TabVisible := DBObject.Name <> '';
    Mainform.UpdateEditorTab;
    MainForm.tabData.TabVisible := True;
    Mainform.RefreshTree(DBObject);
    Mainform.RefreshHelperNode(HELPERNODE_COLUMNS);
    ResetModificationFlags;
    AlterCodeValid := False;
    CreateCodeValid := False;
  except
    on E:EDbError do begin
      ErrorDialog(E.Message);
      Result := mrAbort;
    end;
  end;
end;


procedure TfrmTableEditor.ResetModificationFlags;
var
  i: Integer;
begin
  // Enable converting data for an existing table
  chkCharsetConvertClick(comboCollation);
  // Assist the user in auto unchecking this checkbox so data doesn't get converted more than once accidently
  chkCharsetConvert.Checked := False;
  // Reset modification flags of TEdits and TMemos
  for i:=0 to ComponentCount-1 do
    Components[i].Tag := 0;
  // Reset column changes
  for i:=FColumns.Count-1 downto 0 do begin
    if FColumns[i].Status = esDeleted then
      FColumns.Delete(i)
    else begin
      FColumns[i].OldName := FColumns[i].Name;
      FColumns[i].Status := esUntouched;
    end;
  end;
  DeletedKeys.Clear;
  for i:=0 to FKeys.Count-1 do begin
    FKeys[i].OldName := FKeys[i].Name;
    FKeys[i].OldIndexType := FKeys[i].IndexType;
    FKeys[i].Added := False;
    FKeys[i].Modified := False;
  end;
  DeletedForeignKeys.Clear;
  for i:=0 to FForeignKeys.Count-1 do begin
    FForeignKeys[i].OldKeyName := FForeignKeys[i].KeyName;
    FForeignKeys[i].Added := False;
    FForeignKeys[i].Modified := False;
  end;
  Modified := False;
  btnSave.Enabled := Modified;
  btnDiscard.Enabled := Modified;
end;


function TfrmTableEditor.ComposeAlterStatement: TSQLBatch;
var
  Specs: TStringList;
  ColSpec, IndexSQL, SQL, OldColName, OverrideCollation: String;
  i: Integer;
  Results: TDBQuery;
  Col, PreviousCol: PTableColumn;
  Node: PVirtualNode;

  procedure FinishSpecs;
  begin
    if Specs.Count > 0 then begin
      SQL := SQL + Trim('ALTER TABLE '+DBObject.QuotedName + CRLF + #9 + ImplodeStr(',' + CRLF + #9, Specs)) + ';' + CRLF;
      Specs.Clear;
    end;
  end;

  procedure AddQuery(Query: String);
  begin
    FinishSpecs;
    SQL := SQL + Format(Query, [DBObject.QuotedName]) + ';' + CRLF;
  end;
begin
  // Compose ALTER query, called by buttons and for SQL code tab
  Mainform.ShowStatusMsg(_('Composing ALTER statement ...'));
  Screen.Cursor := crHourglass;
  Specs := TStringList.Create;
  SQL := '';

  // Special case for altered foreign keys: These have to be dropped in a seperate query
  // otherwise the server would return error 121 "Duplicate key on write or update"
  // See also http://dev.mysql.com/doc/refman/5.1/en/innodb-foreign-key-constraints.html :
  //   "You cannot add a foreign key and drop a foreign key in separate clauses of a single
  //   ALTER TABLE  statement. Separate statements are required."
  for i:=0 to FForeignKeys.Count-1 do begin
    if FForeignKeys[i].Modified and (not FForeignKeys[i].Added) then
      Specs.Add('DROP FOREIGN KEY '+DBObject.Connection.QuoteIdent(FForeignKeys[i].OldKeyName));
  end;
  FinishSpecs;

  // Special case for removed default values on columns, which can neither be done in
  // ALTER TABLE ... CHANGE COLUMN query, as there is no "no default" clause, nor by
  // appending an ALTER COLUMN ... DROP DEFAULT, without getting an "unknown column" error.
  // Also, do this after the data type was altered, if from TEXT > VARCHAR e.g.
  for i:=0 to FColumns.Count-1 do begin
    if DBObject.Connection.Parameters.IsMySQL
      and (FColumns[i].FStatus = esModified)
      and (FColumns[i].DefaultType = cdtNothing)
      and (FColumns[i].OldDataType.HasDefault)
      then
      Specs.Add('ALTER '+DBObject.Connection.QuoteIdent(FColumns[i].OldName)+' DROP DEFAULT');
  end;
  FinishSpecs;

  if memoComment.Tag = MODIFIEDFLAG then begin
    case DBObject.Connection.Parameters.NetTypeGroup of
      ngMySQL, ngMSSQL: begin
        Specs.Add('COMMENT=' + esc(memoComment.Text));
      end;
      ngPgSQL: begin
        AddQuery('COMMENT ON TABLE '+DBObject.QuotedName+' IS '+DBObject.Connection.EscapeString(memoComment.Text));
      end;
    end;
  end;
  if (comboCollation.Tag = MODIFIEDFLAG) or (chkCharsetConvert.Checked) then
    Specs.Add('COLLATE=' + esc(comboCollation.Text));
  if (comboEngine.Tag = MODIFIEDFLAG) and (comboEngine.ItemIndex > 0) then begin
    if DBObject.Connection.ServerVersionInt < 40018 then
      Specs.Add('TYPE=' + comboEngine.Text)
    else
      Specs.Add('ENGINE=' + comboEngine.Text);
  end;
  if comboRowFormat.Tag = MODIFIEDFLAG then
    Specs.Add('ROW_FORMAT=' + comboRowFormat.Text);
  if chkChecksum.Tag = MODIFIEDFLAG then
    Specs.Add('CHECKSUM=' + IntToStr(Integer(chkChecksum.Checked)));
  if editAutoInc.Tag = MODIFIEDFLAG then
    Specs.Add('AUTO_INCREMENT=' + IntToStr(MakeInt(editAutoInc.Text)));
  if editAvgRowLen.Tag = MODIFIEDFLAG then
    Specs.Add('AVG_ROW_LENGTH=' + IntToStr(MakeInt(editAvgRowLen.Text)));
  if editMaxRows.Tag = MODIFIEDFLAG then
    Specs.Add('MAX_ROWS=' + IntToStr(MakeInt(editMaxRows.Text)));
  if memoUnionTables.Enabled and (memoUnionTables.Tag = MODIFIEDFLAG) and (memoUnionTables.Text <> '') then
    Specs.Add('UNION=('+memoUnionTables.Text+')');
  if comboInsertMethod.Enabled and (comboInsertMethod.Tag = MODIFIEDFLAG) and (comboInsertMethod.Text <> '') then
    Specs.Add('INSERT_METHOD='+comboInsertMethod.Text);
  if chkCharsetConvert.Checked then begin
    Results := DBObject.Connection.CollationTable;
    if Assigned(Results) then while not Results.Eof do begin
      if Results.Col('Collation') = comboCollation.Text then begin
        Specs.Add('CONVERT TO CHARSET '+Results.Col('Charset'));
        break;
      end;
      Results.Next;
    end;
  end;

  // Update columns
  MainForm.EnableProgress(FColumns.Count + DeletedKeys.Count + FKeys.Count);
  Node := listColumns.GetFirst;
  PreviousCol := nil;
  while Assigned(Node) do begin
    Mainform.ProgressStep;
    Col := listColumns.GetNodeData(Node);
    if Col.Status <> esUntouched then begin
      OverrideCollation := '';
      if chkCharsetConvert.Checked then
        OverrideCollation := comboCollation.Text;
      ColSpec := Col.SQLCode(OverrideCollation);
      // Server version requirement, see http://dev.mysql.com/doc/refman/4.1/en/alter-table.html
      if (DBObject.Connection.Parameters.NetTypeGroup = ngMySQL) and (DBObject.Connection.ServerVersionInt >= 40001) then begin
        if PreviousCol = nil then
          ColSpec := ColSpec + ' FIRST'
        else
          ColSpec := ColSpec + ' AFTER '+DBObject.Connection.QuoteIdent(PreviousCol.Name);
      end;
      case DBObject.Connection.Parameters.NetTypeGroup of
        ngMySQL: OldColName := DBObject.Connection.QuoteIdent(Col.OldName);
        ngMSSQL: OldColName := '';
        // PostgreSQL?? What does this?
      end;
      if Col.Status = esModified then
        Specs.Add(Format(DBObject.Connection.GetSQLSpecifity(spChangeColumn), [OldColName, ColSpec]))
      else if Col.Status in [esAddedUntouched, esAddedModified] then
        Specs.Add(Format(DBObject.Connection.GetSQLSpecifity(spAddColumn), [ColSpec]));

      // MSSQL + Postgres want one ALTER TABLE query per ADD/CHANGE COLUMN
      case DBObject.Connection.Parameters.NetTypeGroup of
        ngMySQL:;
        ngMSSQL: begin
          AddQuery('EXECUTE sp_addextendedproperty '+DBObject.Connection.EscapeString('MS_Description')+', '+
            DBObject.Connection.EscapeString(Col.Comment)+', '+
            DBObject.Connection.EscapeString('Schema')+', '+DBObject.Connection.EscapeString(DBObject.Schema)+', '+
            DBObject.Connection.EscapeString('table')+', '+DBObject.Connection.EscapeString(DBObject.Name)+', '+
            DBObject.Connection.EscapeString('column')+', '+DBObject.Connection.EscapeString(Col.Name)
            );
        end;
        ngPgSQL: begin
          AddQuery('COMMENT ON COLUMN %s.'+DBObject.Connection.QuoteIdent(Col.Name)+' IS '+DBObject.Connection.EscapeString(Col.Comment));
        end;
      end;
    end;
    PreviousCol := Col;
    Node := listColumns.GetNextSibling(Node);
  end;

  // Deleted columns, not available as Node in above loop
  for i:=0 to FColumns.Count-1 do begin
    if FColumns[i].Status = esDeleted then begin
      Specs.Add('DROP COLUMN '+DBObject.Connection.QuoteIdent(FColumns[i].OldName));
      // MSSQL wants one ALTER TABLE query per DROP COLUMN
      if DBObject.Connection.Parameters.IsMSSQL then
        FinishSpecs;
    end;
  end;

  // Drop indexes, also changed indexes, which will be readded below
  for i:=0 to DeletedKeys.Count-1 do begin
    Mainform.ProgressStep;
    if DeletedKeys[i] = PKEY then
      IndexSQL := 'PRIMARY KEY'
    else
      IndexSQL := 'INDEX ' + DBObject.Connection.QuoteIdent(DeletedKeys[i]);
    Specs.Add('DROP '+IndexSQL);
  end;
  // Add changed or added indexes
  for i:=0 to FKeys.Count-1 do begin
    Mainform.ProgressStep;
    if FKeys[i].Modified and (not FKeys[i].Added) then begin
      if FKeys[i].OldIndexType = PKEY then
        IndexSQL := 'PRIMARY KEY'
      else
        IndexSQL := 'INDEX ' + DBObject.Connection.QuoteIdent(FKeys[i].OldName);
      Specs.Add('DROP '+IndexSQL);
    end;
    if FKeys[i].Added or FKeys[i].Modified then
      Specs.Add('ADD '+FKeys[i].SQLCode);
  end;

  for i:=0 to DeletedForeignKeys.Count-1 do
    Specs.Add('DROP FOREIGN KEY '+DBObject.Connection.QuoteIdent(DeletedForeignKeys[i]));
  for i:=0 to FForeignKeys.Count-1 do begin
    if FForeignKeys[i].Added or FForeignKeys[i].Modified then
      Specs.Add('ADD '+FForeignKeys[i].SQLCode(True));
  end;

  FinishSpecs;

  Result := TSQLBatch.Create;
  Result.SQL := SQL;

  FreeAndNil(Specs);
  Mainform.ShowStatusMsg;
  MainForm.DisableProgress;
  Screen.Cursor := crDefault;
end;


function TfrmTableEditor.ComposeCreateStatement: TSQLBatch;
var
  i, IndexCount: Integer;
  Col: PTableColumn;
  Node: PVirtualNode;
  tmp, SQL: String;
begin
  // Compose CREATE query, called by buttons and for SQL code tab
  SQL := 'CREATE TABLE '+DBObject.Connection.QuoteIdent(editName.Text)+' ('+CRLF;
  Node := listColumns.GetFirst;
  while Assigned(Node) do begin
    Col := listColumns.GetNodeData(Node);
    SQL := SQL + #9 + Col.SQLCode + ','+CRLF;
    Node := listColumns.GetNextSibling(Node);
  end;

  IndexCount := 0;
  for i:=0 to FKeys.Count-1 do begin
    tmp := FKeys[i].SQLCode;
    if tmp <> '' then begin
      SQL := SQL + #9 + tmp + ','+CRLF;
      Inc(IndexCount);
    end;
  end;

  for i:=0 to FForeignKeys.Count-1 do
    SQL := SQL + #9 + FForeignKeys[i].SQLCode(True) + ','+CRLF;

  if Integer(listColumns.RootNodeCount) + IndexCount + FForeignKeys.Count > 0 then
    Delete(SQL, Length(SQL)-2, 3);

  SQL := SQL + CRLF + ')' + CRLF;
  if memoComment.Text <> '' then
    SQL := SQL + 'COMMENT='+esc(memoComment.Text) + CRLF;
  if comboCollation.Text <> '' then
    SQL := SQL + 'COLLATE='+esc(comboCollation.Text) + CRLF;
  if (comboEngine.Text <> '') and (comboEngine.ItemIndex > 0) then begin
    if DBObject.Connection.ServerVersionInt < 40018 then
      SQL := SQL + 'TYPE='+comboEngine.Text + CRLF
    else
      SQL := SQL + 'ENGINE='+comboEngine.Text + CRLF;
  end;
  if comboRowFormat.Text <> 'DEFAULT' then
    SQL := SQL + 'ROW_FORMAT='+comboRowFormat.Text + CRLF;
  if chkChecksum.Checked then
    SQL := SQL + 'CHECKSUM='+IntToStr(Integer(chkChecksum.Checked)) + CRLF;
  if editAutoInc.Text <> '' then
    SQL := SQL + 'AUTO_INCREMENT='+editAutoInc.Text + CRLF;
  if editAvgRowLen.Text <> '' then
    SQL := SQL + 'AVG_ROW_LENGTH='+editAvgRowLen.Text + CRLF;
  if editMaxRows.Text <> '' then
    SQL := SQL + 'MAX_ROWS='+editMaxRows.Text + CRLF;
  if memoUnionTables.Enabled and (memoUnionTables.Text <> '') then
    SQL := SQL + 'UNION=('+memoUnionTables.Text+')' + CRLF;
  if comboInsertMethod.Enabled and (comboInsertMethod.Text <> '') then
    SQL := SQL + 'INSERT_METHOD='+comboInsertMethod.Text + CRLF;
  if SynMemoPartitions.GetTextLen > 0 then
    SQL := SQL +  '/*!50100 ' + SynMemoPartitions.Text + ' */';
  SQL := SQL + ';' + CRLF;

  if DBObject.Connection.Parameters.IsPostgreSQL then begin
    Node := listColumns.GetFirst;
    while Assigned(Node) do begin
      Col := listColumns.GetNodeData(Node);
      SQL := SQL + 'COMMENT ON COLUMN '+
        DBObject.Connection.QuoteIdent(editName.Text)+'.'+DBObject.Connection.QuoteIdent(Col.Name)+
        ' IS '+DBObject.Connection.EscapeString(Col.Comment) + ';' + CRLF;
      Node := listColumns.GetNextSibling(Node);
    end;
  end;

  Result := TSQLBatch.Create;
  Result.SQL := Trim(SQL);
end;


procedure TfrmTableEditor.Modification(Sender: TObject);
begin
  // Memorize modified status
  if FLoaded then begin
    if Sender is TComponent then
      TComponent(Sender).Tag := MODIFIEDFLAG;
    Modified := True;
    btnSave.Enabled := Modified and (editName.Text <> '') and (listColumns.RootNodeCount > 0);
    btnDiscard.Enabled := Modified;
    CreateCodeValid := False;
    AlterCodeValid := False;
    UpdateSQLcode;
    CalcMinColWidth;
  end;
end;


procedure TfrmTableEditor.btnAddColumnClick(Sender: TObject);
var
  NewCol: TTableColumn;
  FocusedCol: PTableColumn;
  fn, NewNode: PVirtualNode;
  idx: Integer;
  DefaultType: String;
begin
  // Add new column after selected one
  if listColumns.IsEditing then
    listColumns.EndEditNode;
  fn := listColumns.FocusedNode;
  NewCol := TTableColumn.Create(DBObject.Connection);
  if Assigned(fn) then begin
    idx := fn.Index+1;
    // Copy properties from focused node
    FocusedCol := listColumns.GetNodeData(fn);
    NewCol.DataType := FocusedCol.DataType;
    NewCol.LengthSet := FocusedCol.LengthSet;
    NewCol.Unsigned := FocusedCol.Unsigned;
    NewCol.AllowNull := FocusedCol.AllowNull;
    // There can be only one
    if FocusedCol.DefaultType = cdtAutoInc then begin
      NewCol.DefaultType := cdtText;
      NewCol.DefaultText := '0';
    end else begin
      NewCol.DefaultType := FocusedCol.DefaultType;
      NewCol.DefaultText := FocusedCol.DefaultText;
    end;
    NewCol.Collation := '';
  end else begin
    idx := listColumns.RootNodeCount;
    DefaultType := 'INT';
    NewCol.DataType := DBObject.Connection.GetDatatypeByName(DefaultType, False);
    NewCol.Unsigned := False;
    NewCol.AllowNull := True;
    NewCol.DefaultType := cdtNothing;
    NewCol.DefaultText := '';
    NewCol.Comment := '';
    NewCol.Collation := '';
  end;
  NewCol.Name := _('Column')+' '+IntToStr(idx+1);
  FColumns.Insert(idx, NewCol);
  NewNode := listColumns.InsertNode(fn, amInsertAfter, @NewCol);
  NewCol.Status := esAddedUntouched;
  SelectNode(listColumns, NewNode);
  Modification(Sender);
  ValidateColumnControls;
  listColumns.EditNode(NewNode, 1);
end;


procedure TfrmTableEditor.btnRemoveColumnClick(Sender: TObject);
var
  Node, NodeDelete, NodeFocus: PVirtualNode;
  Col: PTableColumn;
begin
  // Remove selected column(s)
  Node := GetNextNode(listColumns, nil, True);
  while Assigned(Node) do begin
    Col := listColumns.GetNodeData(Node);
    Col.Name := Col.OldName;
    Col.Status := esDeleted;
    Node := GetNextNode(listColumns, Node, True);
  end;
  // Find next unselected node for new focus
  Node := listColumns.FocusedNode;
  NodeFocus := nil;
  while Assigned(Node) do begin
    if not (vsSelected in Node.States) then begin
      NodeFocus := Node;
      break;
    end;
    Node := listColumns.GetNextSibling(Node);
  end;
  // Delete selected + visible
  Node := GetNextNode(listColumns, nil, True);
  while Assigned(Node) do begin
    NodeDelete := Node;
    Node := GetNextNode(listColumns, Node, True);
    listColumns.DeleteNode(NodeDelete);
  end;

  if not Assigned(NodeFocus) then
    NodeFocus := listColumns.GetLast;
  if Assigned(NodeFocus) then
    SelectNode(listColumns, NodeFocus.Index);
  Modification(Sender);
  ValidateColumnControls;
end;


procedure TfrmTableEditor.btnMoveUpColumnClick(Sender: TObject);
begin
  // Move column up
  listColumns.EndEditNode;
  listColumns.MoveTo(listColumns.FocusedNode, listColumns.GetPreviousSibling(listColumns.FocusedNode), amInsertBefore, False);
  ValidateColumnControls;
end;


procedure TfrmTableEditor.btnMoveDownColumnClick(Sender: TObject);
begin
  // Move column down
  listColumns.EndEditNode;
  listColumns.MoveTo(listColumns.FocusedNode, listColumns.GetNextSibling(listColumns.FocusedNode), amInsertAfter, False);
  ValidateColumnControls;
end;


procedure TfrmTableEditor.listColumnsDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := (Source = Sender) and (Mode <> dmNowhere);
  // Not sure what this effect does, probably show a specific mouse cursor?
  Effect := DROPEFFECT_MOVE;
end;


procedure TfrmTableEditor.listColumnsDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  Node: PVirtualNode;
  AttachMode: TVTNodeAttachMode;
begin
  Node := Sender.GetNodeAt(Pt.X, Pt.Y);
  if Assigned(Node) then begin
    case Mode of
      dmAbove, dmOnNode: AttachMode := amInsertBefore;
      else AttachMode := amInsertAfter;
    end;
    listColumns.MoveTo(listColumns.FocusedNode, Node, AttachMode, False);
    ValidateColumnControls;
  end;
end;


procedure TfrmTableEditor.listColumnsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  // Darken cell background to signalize it doesn't allow length/set
  // Exclude non editable checkbox columns - grey looks ugly there.
  if (not CellEditingAllowed(Node, Column)) and (not (Column in [4, 5, 6])) then begin
    TargetCanvas.Brush.Color := GetThemeColor(clBtnFace);
    TargetCanvas.FillRect(CellRect);
  end;
end;


procedure TfrmTableEditor.CalcMinColWidth;
var
  i, j, MinWidthThisCol, MinWidthAllCols: Integer;
begin
  // Find maximum column widths so the index icons have enough room after auto-fitting
  MinWidthAllCols := 0;
  for i:=0 to FColumns.Count-1 do begin
    MinWidthThisCol := 0;
    for j:=0 to FKeys.Count-1 do begin
      if FKeys[j].Columns.IndexOf(FColumns[i].Name) > -1 then
        Inc(MinWidthThisCol, listColumns.Images.Width);
    end;
    for j:=0 to FForeignKeys.Count-1 do begin
      if FForeignKeys[j].Columns.IndexOf(FColumns[i].Name) > -1 then
        Inc(MinWidthThisCol, listColumns.Images.Width);
    end;
    MinWidthAllCols := Max(MinWidthAllCols, MinWidthThisCol);
  end;
  // Add space for number
  Inc(MinWidthAllCols, listColumns.Canvas.TextWidth(IntToStr(FColumns.Count+1)) + listColumns.TextMargin*4);
  listColumns.Header.Columns[0].Width := MinWidthAllCols;
end;


procedure TfrmTableEditor.listColumnsAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  Col: PTableColumn;
  ImageIndex, X, Y, i: Integer;
  VT: TVirtualStringTree;
  Checked: Boolean;
begin
  VT := TVirtualStringTree(Sender);
  Col := Sender.GetNodeData(Node);
  Y := CellRect.Top + Integer(VT.NodeHeight[Node] div 2) - (VT.Images.Height div 2);

  // Paint one icon per index of which this column is part of
  if Column = 0 then begin
    X := 0;
    for i:=0 to FKeys.Count-1 do begin
      if FKeys[i].Columns.IndexOf(Col.Name) > -1 then begin
        ImageIndex := FKeys[i].ImageIndex;
        VT.Images.Draw(TargetCanvas, X, Y, ImageIndex);
        Inc(X, VT.Images.Width);
      end;
    end;
    for i:=0 to FForeignKeys.Count-1 do begin
      if FForeignKeys[i].Columns.IndexOf(Col.Name) > -1 then begin
        ImageIndex := ICONINDEX_FOREIGNKEY;
        VT.Images.Draw(TargetCanvas, X, Y, ImageIndex);
        Inc(X, VT.Images.Width);
      end;
    end;
  end;

  // Paint checkbox image in certain columns
  // while restricting "Allow NULL" checkbox to numeric datatypes
  if (Column in [4, 5, 6]) then begin
    Checked := (Col.Unsigned and (Column=4)) or (Col.AllowNull and (Column=5)) or (Col.ZeroFill and (Column = 6));
    if CellEditingAllowed(Node, Column) then begin
      if Checked then ImageIndex := 128
      else ImageIndex := 127;
    end else begin
      if Checked then ImageIndex := 176
      else ImageIndex := 175;
    end;
    X := CellRect.Left + (VT.Header.Columns[Column].Width div 2) - (VT.Images.Width div 2);
    VT.Images.Draw(TargetCanvas, X, Y, ImageIndex);
  end;
end;


procedure TfrmTableEditor.listColumnsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  // Column focus changed
  ValidateColumnControls;
end;


procedure TfrmTableEditor.ValidateColumnControls;
var Node: PVirtualNode;
begin
  Node := listColumns.FocusedNode;
  btnRemoveColumn.Enabled := listColumns.SelectedCount > 0;
  btnMoveUpColumn.Enabled := Assigned(Node)
    and (Node <> listColumns.GetFirst)
    and (DBObject.Connection.Parameters.NetTypeGroup = ngMySQL);
  btnMoveDownColumn.Enabled := Assigned(Node)
    and (Node <> listColumns.GetLast)
    and (DBObject.Connection.Parameters.NetTypeGroup = ngMySQL);

  menuRemoveColumn.Enabled := btnRemoveColumn.Enabled;
  menuMoveUpColumn.Enabled := btnMoveUpColumn.Enabled;
  menuMoveDownColumn.Enabled := btnMoveDownColumn.Enabled;
end;


procedure TfrmTableEditor.listColumnsEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  // Allow text editing? Explicitely block that in checkbox columns 
  Allowed := CellEditingAllowed(Node, Column) and (not (Column in [4,5,6]));
end;


function TfrmTableEditor.CellEditingAllowed(Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  Col: PTableColumn;
  i: Integer;
begin
  Col := listColumns.GetNodeData(Node);
  case Column of
    // No editor for very first column and checkbox columns
    0: Result := False;
    3: Result := Col.DataType.HasLength;
    4: begin
      Result := (Col.DataType.Category in [dtcInteger, dtcReal])
        and (Col.DataType.Index <> dtBit)
        and (DBObject.Connection.Parameters.IsMySQL);
      if (not Result) and Col.Unsigned then begin
        Col.Unsigned := False;
        Col.Status := esModified;
      end;
    end;
    5: begin
      // Do not allow NULL, and force NOT NULL, on primary key columns
      Result := True;
      for i:=0 to FKeys.Count-1 do begin
        if (FKeys[i].IndexType = PKEY) and (FKeys[i].Columns.IndexOf(Col.Name) > -1) then begin
          if Col.AllowNull then begin
            Col.AllowNull := False;
            Col.Status := esModified;
          end;
          Result := False;
          break;
        end;
      end;
    end;
    6: begin
      Result := (Col.DataType.Category in [dtcInteger, dtcReal])
        and (Col.DataType.Index <> dtBit)
        and (DBObject.Connection.Parameters.IsMySQL);
      if (not Result) and Col.ZeroFill then begin
        Col.ZeroFill := False;
        Col.Status := esModified;
      end;
    end;
    // No editing of collation allowed if "Convert data" was checked
    9: Result := not chkCharsetConvert.Checked;
    else Result := True;
  end;
end;


procedure TfrmTableEditor.listColumnsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Col: PTableColumn;
begin
  // Display column text
  Col := Sender.GetNodeData(Node);
  CellText := '';
  case Column of
    0: CellText := IntToStr(Node.Index+1);
    1: CellText := Col.Name;
    2: CellText := Col.DataType.Name;
    3: CellText := Col.LengthSet;
    4, 5, 6: CellText := ''; // Checkbox
    7: begin
      case Col.DefaultType of
        cdtNothing:      CellText := _('No default');
        cdtText:         CellText := Col.Connection.EscapeString(Col.DefaultText);
        cdtNull:         CellText := 'NULL';
        cdtExpression:   CellText := Col.DefaultText;
        cdtAutoInc:      CellText := 'AUTO_INCREMENT';
      end;
      case Col.OnUpdateType of
        // cdtNothing: leave clause away
        // cdtText: not supported
        // cdtNull: not supported
        cdtExpression:   CellText := CellText + ' ON UPDATE ' + Col.OnUpdateText;
        // cdtAutoInc: invalid here
      end;
    end;
    8: CellText := Col.Comment;
    9: begin
      CellText := Col.Collation;
      if (CellText <> '') and (chkCharsetConvert.Checked) then
        CellText := comboCollation.Text;
    end;
    10: CellText := Col.Expression;
    11: CellText := Col.Virtuality;
  end;
end;


procedure TfrmTableEditor.listColumnsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Col: PTableColumn;
begin
  // Bind data to node
  Col := Sender.GetNodeData(Node);
  Col^ := FColumns[Node.Index];
end;


procedure TfrmTableEditor.listColumnsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTableColumn);
end;


procedure TfrmTableEditor.listColumnsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  TextColor: TColor;
  i: Integer;
  Col: PTableColumn;
begin
  Col := Sender.GetNodeData(Node);
  // Bold font for primary key columns
  for i:=0 to FKeys.Count-1 do begin
    if (FKeys[i].IndexType = PKEY) and (FKeys[i].Columns.IndexOf(Col.Name) > -1) then begin
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      break;
    end;
  end;

  // No specific colors for selected nodes, would interfere with blue selection background
  if vsSelected in Node.States then Exit;
  // Break early if nothing to do
  if not (Column in [2, 7]) then Exit;

  // Give datatype column specific color, as set in preferences
  TextColor := TargetCanvas.Font.Color;
  case Column of
    2: TextColor := DatatypeCategories[Col.DataType.Category].Color;

    7: case Col.DefaultType of
      cdtNothing, cdtNull:
        TextColor := DatatypeCategories[Col.DataType.Category].NullColor;
      else
        TextColor := DatatypeCategories[Col.DataType.Category].Color;
    end;
  end;
  TargetCanvas.Font.Color := TextColor;
end;


procedure TfrmTableEditor.listColumnsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  i: Integer;
  Col: PTableColumn;
  Key: TTableKey;
  WasModified: Boolean;
begin
  // Column property edited
  Col := Sender.GetNodeData(Node);
  WasModified := True;
  case Column of
    1: begin // Name of column
      for i:=0 to FColumns.Count-1 do begin
        if (FColumns[i].Name = NewText) and (not (FColumns[i].Status in [esDeleted, esAddedDeleted])) then begin
          ErrorDialog(f_('Column "%s" already exists.', [NewText]));
          Exit;
        end;
      end;
      for Key in FKeys do begin
        for i:=0 to Key.Columns.Count-1 do begin
          if Key.Columns[i] = Col.Name then
            Key.Columns[i] := NewText;
        end;
      end;
      treeIndexes.Invalidate;
      Col.Name := NewText;
    end;
    2: begin // Data type
      Col.DataType := DBObject.Connection.GetDatatypeByName(NewText, False, Col.Name);
      // Reset length/set for column types which don't support that
      if not Col.DataType.HasLength then
        Col.LengthSet := '';
      // Suggest length/set if required
      if (not Col.LengthCustomized) or (Col.DataType.RequiresLength and (Col.LengthSet = '')) then
        Col.LengthSet := Col.DataType.DefLengthSet;
      // Auto-fix user selected default type which can be invalid now
      case Col.DataType.Category of
        dtcInteger: begin
          Col.DefaultType := cdtExpression;
          if Col.AllowNull then
            Col.DefaultType := cdtNull
          else
            Col.DefaultText := IntToStr(MakeInt(Col.DefaultText));
        end;
        dtcReal: begin
          Col.DefaultType := cdtExpression;
          if Col.AllowNull then
            Col.DefaultType := cdtNull
          else
            Col.DefaultText := FloatToStr(MakeFloat(Col.DefaultText));
        end;
        dtcText, dtcBinary, dtcSpatial, dtcOther: begin
          Col.DefaultType := cdtText;
          if Col.AllowNull then
            Col.DefaultType := cdtNull;
        end;
        dtcTemporal: begin
          if Col.DefaultType = cdtAutoinc then
            Col.DefaultType := cdtNothing;
        end;
      end;

    end; // Length / Set
    3: begin
      if Col.DataType.RequiresLength and (NewText='') then begin
        WasModified := False;
        ErrorDialog(f_('Column data type %s requires a length/set', [Col.DataType.Name]));
      end else begin
        Col.LengthSet := NewText;
        Col.LengthCustomized := True;
      end;
    end;
    // 4 + 5 are checkboxes - handled in OnClick
    7: begin // Default value
      // DefaultText/Type and OnUpdateText/Type are set in TColumnDefaultEditorLink.EndEdit
      if Col.DefaultType = cdtNull then
        Col.AllowNull := True;
    end;
    8: Col.Comment := NewText;
    9: Col.Collation := NewText;
    10: Col.Expression := NewText;
    11: Col.Virtuality := NewText;
  end;
  if WasModified then begin
    Col.Status := esModified;
    Modification(Sender);
  end;
end;


procedure TfrmTableEditor.listColumnsNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Col: PTableColumn;
begin
  Col := Sender.GetNodeData(Node);
  Col.Status := esModified;
  Modification(Sender);
end;


procedure TfrmTableEditor.listColumnsClick(Sender: TObject);
var
  VT: TVirtualStringTree;
  Click: THitInfo;
begin
  // Handle click event
  VT := Sender as TVirtualStringTree;
  VT.GetHitTestInfoAt(Mouse.CursorPos.X-VT.ClientOrigin.X, Mouse.CursorPos.Y-VT.ClientOrigin.Y, True, Click);
  vtHandleClickOrKeyPress(VT, Click.HitNode, Click.HitColumn, Click.HitPositions);
end;



procedure TfrmTableEditor.listColumnsKeyPress(Sender: TObject; var Key: Char);
var
  VT: TVirtualStringTree;
begin
  // Space/click on checkbox column
  VT := Sender as TVirtualStringTree;
  if (Ord(Key) = VK_SPACE) and (VT.FocusedColumn in [4, 5, 6]) then
    vtHandleClickOrKeyPress(VT, VT.FocusedNode, VT.FocusedColumn, []);
end;


procedure TfrmTableEditor.vtHandleClickOrKeyPress(Sender: TVirtualStringTree;
  Node: PVirtualNode; Column: TColumnIndex; HitPositions: THitPositions);
var
  Col: PTableColumn;
  VT: TVirtualStringTree;
begin
  if (not Assigned(Node)) or (Column = NoColumn) then
    Exit;
  VT := Sender as TVirtualStringTree;
  // For checkboxes, cell editors are disabled, instead toggle their state
  if CellEditingAllowed(Node, Column) then begin
    Col := VT.GetNodeData(Node);
    case Column of
      4: begin
        Col.Unsigned := not Col.Unsigned;
        Col.Status := esModified;
        Modification(Sender);
        VT.InvalidateNode(Node);
      end;
      5: begin
        Col.AllowNull := not Col.AllowNull;
        // Switch default value from NULL to Text if Allow Null is off
        if (not Col.AllowNull) and (Col.DefaultType = cdtNull) then begin
          Col.DefaultType := cdtNothing;
          Col.DefaultText := '';
        end;
        Col.Status := esModified;
        Modification(Sender);
        VT.InvalidateNode(Node);
      end;
      6: begin
        Col.ZeroFill := not Col.ZeroFill;
        Col.Status := esModified;
        Modification(Sender);
        VT.InvalidateNode(Node);
      end;
      else begin
        // All other cells go into edit mode please
        // Explicitely done on OnClick, not in OnFocusChanged which seemed annoying for keyboard users
        if hiOnItemLabel in HitPositions then
          VT.EditNode(Node, Column);
      end;
    end;
  end;
end;

procedure TfrmTableEditor.listColumnsCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  VT: TVirtualStringTree;
  EnumEditor: TEnumEditorLink;
  DefaultEditor: TColumnDefaultEditorLink;
  DatatypeEditor: TDatatypeEditorLink;
  Col: PTableColumn;
  Edit: TInplaceEditorLink;
begin
  // Start cell editor
  VT := Sender as TVirtualStringTree;
  Col := Sender.GetNodeData(Node);
  case Column of
    2: begin // Datatype pulldown
      DatatypeEditor := TDatatypeEditorLink.Create(VT);
      EditLink := DataTypeEditor;
      end;
    9: begin // Collation pulldown
      EnumEditor := TEnumEditorLink.Create(VT);
      EnumEditor.ValueList := TStringList.Create;
      EnumEditor.ValueList.Text := DBObject.Connection.CollationList.Text;
      EnumEditor.ValueList.Insert(0, '');
      EditLink := EnumEditor;
      end;
    7: begin
      DefaultEditor := TColumnDefaultEditorLink.Create(VT);
      DefaultEditor.DefaultType := Col.DefaultType;
      DefaultEditor.DefaultText := Col.DefaultText;
      DefaultEditor.OnUpdateType := Col.OnUpdateType;
      DefaultEditor.OnUpdateText := Col.OnUpdateText;
      EditLink := DefaultEditor;
    end;
    11: begin // Virtuality pulldown
      EnumEditor := TEnumEditorLink.Create(VT);
      EnumEditor.ValueList := TStringList.Create;
      if DBObject.Connection.Parameters.IsMariaDB then
        EnumEditor.ValueList.CommaText := ',VIRTUAL,PERSISTENT'
      else
        EnumEditor.ValueList.CommaText := ',VIRTUAL,STORED';
      EditLink := EnumEditor;
    end
    else begin
      Edit := TInplaceEditorLink.Create(VT);
      Edit.TitleText := VT.Header.Columns[Column].Text;
      Edit.ButtonVisible := True;
      EditLink := Edit;
    end;
  end;
  TBaseGridEditorLink(EditLink).TableColumn := Col^;
end;


procedure TfrmTableEditor.editNumEditChange(Sender: TObject);
var
  ed: TEdit;
  ShouldBe: String;
  CursorPos: Integer;
begin
  // Only numbers allowed in this TEdit
  Modification(Sender);
  ed := Sender as TEdit;
  ShouldBe := CleanupNumber(ed.Text);
  if (ed.Text = ShouldBe) or (ed.Text = '') then Exit;
  MessageBeep(MB_ICONEXCLAMATION);
  CursorPos := ed.SelStart;
  ed.OnChange := nil;
  ed.Text := ShouldBe;
  ed.SelStart := CursorPos;
  ed.OnChange := editNumEditChange;
end;


procedure TfrmTableEditor.comboEngineSelect(Sender: TObject);
var
  IsMerge: Boolean;
begin
  // Enable/disable engine specific option controls
  IsMerge := UpperCase((Sender as TComboBox).Text) = 'MRG_MYISAM';
  memoUnionTables.Enabled := IsMerge;
  comboInsertMethod.Enabled := IsMerge;
  Modification(Sender);
end;


procedure TfrmTableEditor.btnAddIndexClick(Sender: TObject);
var
  TblKey: TTableKey;
begin
  // Add new index
  TblKey := TTableKey.Create(DBObject.Connection);
  TblKey.Name := _('Index')+' '+IntToStr(FKeys.Count+1);
  TblKey.OldName := TblKey.Name;
  TblKey.IndexType := KEY;
  TblKey.OldIndexType := TblKey.IndexType;
  TblKey.Added := True;
  FKeys.Add(TblKey);
  Modification(Sender);
  treeIndexes.Invalidate;
  SelectNode(treeIndexes, FKeys.Count-1);
end;


procedure TfrmTableEditor.menuAddIndexColumnClick(Sender: TObject);
var
  Node: PVirtualNode;
  i, j: Integer;
  NewCol, PartLength: String;
  ColExists: Boolean;
  Column: TTableColumn;
  TblKey: TTableKey;
begin
  // Add column to index
  Node := treeIndexes.FocusedNode;
  if not Assigned(Node) then
    Exit;
  if treeIndexes.GetNodeLevel(Node) = 1 then
    Node := Node.Parent;
  TblKey := FKeys[Node.Index];
  // Find the first unused column for that index as default
  ColExists := False;
  NewCol := '';
  PartLength := '';
  for i:=0 to FColumns.Count-1 do begin
    Column := FColumns[i];
    if Column.Status = esDeleted then
      Continue;
    for j:=0 to TblKey.Columns.Count - 1 do begin
      ColExists := TblKey.Columns[j] = Column.Name;
      if ColExists then
        break;
    end;
    if not ColExists then begin
      NewCol := Column.Name;
      if (TblKey.IndexType <> FKEY) and (Column.DataType.Index in [dtTinyText, dtText, dtMediumText, dtLongText, dtTinyBlob, dtBlob, dtMediumBlob, dtLongBlob]) then
        PartLength := '100';
      break;
    end;
  end;
  treeIndexes.AddChild(Node);
  TblKey.Columns.Add(NewCol);
  TblKey.SubParts.Add(PartLength);
  Modification(Sender);
  treeIndexes.Invalidate;
  SelectNode(treeIndexes, FKeys.Count-1, Node);
end;


procedure TfrmTableEditor.btnRemoveIndexClick(Sender: TObject);
var
  idx: Integer;
  NewSelectNode: PVirtualNode;
begin
  // Remove index or part
  if treeIndexes.IsEditing then
    treeIndexes.CancelEditNode;
  case treeIndexes.GetNodeLevel(treeIndexes.FocusedNode) of
    0: begin
      idx := treeIndexes.FocusedNode.Index;
      if not FKeys[idx].Added then
        DeletedKeys.Add(FKeys[idx].OldName);
      FKeys.Delete(idx);
      // Delete node although ReinitChildren would do the same, but the Repaint before
      // creates AVs in certain cases. See issue #2557
      treeIndexes.DeleteNode(treeIndexes.FocusedNode);
    end;
    1: begin
      idx := treeIndexes.FocusedNode.Parent.Index;
      FKeys[idx].Columns.Delete(treeIndexes.FocusedNode.Index);
      FKeys[idx].SubParts.Delete(treeIndexes.FocusedNode.Index);
      treeIndexes.DeleteNode(treeIndexes.FocusedNode);
    end;
  end;
  Modification(Sender);
  treeIndexes.Repaint;
  treeIndexes.ReinitChildren(nil, True);
  NewSelectNode := treeIndexes.GetPreviousVisible(treeIndexes.FocusedNode);
  if Assigned(NewSelectNode) then
    SelectNode(treeIndexes, NewSelectNode.Index, NewSelectNode.Parent);
end;


procedure TfrmTableEditor.btnClearIndexesClick(Sender: TObject);
var
  TblKey: TTableKey;
begin
  // Clear all indexes
  // Column data gets freed below - end any editor which could cause AV's
  if treeIndexes.IsEditing then
    treeIndexes.CancelEditNode;
  // Trigger ValidateIndexControls
  SelectNode(treeIndexes, nil);
  for TblKey in FKeys do begin
    if not TblKey.Added then
      DeletedKeys.Add(TblKey.OldName);
  end;
  FKeys.Clear;
  Modification(Sender);
  treeIndexes.Clear;
end;


procedure TfrmTableEditor.treeIndexesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  VT: TVirtualStringTree;
begin
  // Icon image showing type of index
  VT := Sender as TVirtualStringTree;
  if Column <> 0 then Exit;
  if not (Kind in [ikNormal, ikSelected]) then Exit;
  case VT.GetNodeLevel(Node) of
    0: ImageIndex := FKeys[Node.Index].ImageIndex;
    1: ImageIndex := 42;
  end;
end;


procedure TfrmTableEditor.treeIndexesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  TblKey: TTableKey;
begin
  // Index tree showing cell text
  case Sender.GetNodeLevel(Node) of
    0: begin
      TblKey := FKeys[Node.Index];
      case Column of
        0: if TblKey.IndexType = PKEY then
             CellText := TblKey.IndexType + ' KEY' // Fixed name "PRIMARY KEY", cannot be changed
           else
             CellText := TblKey.Name;
        1: CellText := TblKey.IndexType;
        2: CellText := TblKey.Algorithm;
      end;
    end;
    1: begin
      TblKey := FKeys[Node.Parent.Index];
      case Column of
        0: CellText := TblKey.Columns[Node.Index];
        1: CellText := TblKey.SubParts[Node.Index];
        2: CellText := '';
      end;
    end;
  end;
end;


procedure TfrmTableEditor.treeIndexesInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  // Tell number of columns contained in index
  ChildCount := FKeys[Node.Index].Columns.Count;
  ListColumns.Invalidate;
end;


procedure TfrmTableEditor.treeIndexesInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  // Show plus sign on first level nodes
  if Sender.GetNodeLevel(Node) = 0 then
    Include( InitialStates, ivsHasChildren);
end;


procedure TfrmTableEditor.treeIndexesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  ValidateIndexControls;
end;


procedure TfrmTableEditor.treeIndexesClick(Sender: TObject);
var
  VT: TVirtualStringTree;
  Click: THitInfo;
begin
  // Handle click event
  VT := Sender as TVirtualStringTree;
  VT.GetHitTestInfoAt(Mouse.CursorPos.X-VT.ClientOrigin.X, Mouse.CursorPos.Y-VT.ClientOrigin.Y, True, Click);
  if Assigned(Click.HitNode) and (Click.HitColumn > NoColumn) and (hiOnItemLabel in Click.HitPositions) then
    VT.EditNode(Click.HitNode, Click.HitColumn);
end;


procedure TfrmTableEditor.ValidateIndexControls;
var
  Node: PVirtualNode;
  HasNode: Boolean;
  Level: Integer;
begin
  // Enable/disable buttons
  Node := treeIndexes.FocusedNode;
  HasNode := Assigned(Node);
  if HasNode then Level := treeIndexes.GetNodeLevel(Node)
  else Level := -1;

  btnRemoveIndex.Enabled := HasNode;
  btnClearIndexes.Enabled := FKeys.Count > 0;
  btnMoveUpIndex.Enabled := HasNode and (Level = 1) and (Node <> treeIndexes.GetFirstChild(Node.Parent));
  btnMoveDownIndex.Enabled := HasNode and (Level = 1) and (Node <> treeIndexes.GetLastChild(Node.Parent));

  menuAddIndexColumn.Enabled := HasNode;
  menuRemoveIndex.Enabled := btnRemoveIndex.Enabled;
  menuClearIndexes.Enabled := btnClearIndexes.Enabled;
  menuMoveUpIndex.Enabled := btnMoveUpIndex.Enabled;
  menuMoveDownIndex.Enabled := btnMoveDownIndex.Enabled;
end;


procedure TfrmTableEditor.treeIndexesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  VT: TVirtualStringtree;
  IndexedColName: String;
  i: Integer;
begin
  VT := Sender as TVirtualStringtree;
  Allowed := False;
  if VT.GetNodeLevel(Node) = 0 then begin
    // Disallow renaming primary key
    if (Column <> 0) or (VT.Text[Node, 1] <> PKEY) then
      Allowed := True
  end else case Column of
    0: Allowed := True;
    1: begin
      // Column length is allowed for (var)char/text types only, even mandantory for text and blobs
      IndexedColName := VT.Text[Node, 0];
      for i:=0 to FColumns.Count-1 do begin
        if FColumns[i].Name = IndexedColName then begin
          Allowed := FColumns[i].DataType.Category = dtcText;
          break;
        end;
      end;
    end;
  end;
end;


procedure TfrmTableEditor.treeIndexesCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  VT: TVirtualStringTree;
  EnumEditor: TEnumEditorLink;
  Level: Cardinal;
  ColNode: PVirtualNode;
  Col: PTableColumn;
begin
  // Start cell editor
  VT := Sender as TVirtualStringTree;
  Level := (Sender as TVirtualStringtree).GetNodeLevel(Node);
  if (Level = 0) and (Column = 1) then begin
    // Index type pulldown
    EnumEditor := TEnumEditorLink.Create(VT);
    EnumEditor.ValueList := TStringList.Create;
    EnumEditor.ValueList.CommaText := PKEY +','+ KEY +','+ UKEY +','+ FKEY +','+ SKEY;
    EditLink := EnumEditor;
  end else if (Level = 0) and (Column = 2) then begin
    // Algorithm pulldown
    EnumEditor := TEnumEditorLink.Create(VT);
    EnumEditor.ValueList := Explode(',', ',BTREE,HASH,RTREE');
    EditLink := EnumEditor;
  end else if (Level = 1) and (Column = 0) then begin
    // Column names pulldown
    EnumEditor := TEnumEditorLink.Create(VT);
    ColNode := listColumns.GetFirst;
    while Assigned(ColNode) do begin
      Col := listColumns.GetNodeData(ColNode);
      EnumEditor.ValueList.Add(Col.Name);
      ColNode := listColumns.GetNext(ColNode);
    end;
    EnumEditor.AllowCustomText := True; // Allows adding a subpart in index parts: "TextCol(20)"
    EditLink := EnumEditor;
  end else
    EditLink := TInplaceEditorLink.Create(VT);
end;


procedure TfrmTableEditor.treeIndexesNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  VT: TVirtualStringtree;
  TblKey: TTableKey;
  rx: TRegExpr;
begin
  // Rename index of column
  VT := Sender as TVirtualStringtree;
  case VT.GetNodeLevel(Node) of
    0: begin
       TblKey := FKeys[Node.Index];
       case Column of
         0: TblKey.Name := NewText;
         1: begin
             TblKey.IndexType := NewText;
             if NewText = PKEY then
               TblKey.Name := PKEY;
           end;
         2: TblKey.Algorithm := NewText;
       end;
       // Needs to be called manually for Name and IndexType properties:
       TblKey.Modification(Sender);
    end;
    1: begin
       TblKey := FKeys[Node.Parent.Index];
       case Column of
         0: begin
           // Detect input of "col(123)" and move "123" into subpart
           rx := TRegExpr.Create;
           rx.Expression := '.+\((\d+)\)';
           if rx.Exec(NewText) then begin
             TblKey.Columns[Node.Index] := Copy(NewText, 1, Length(NewText)-rx.MatchLen[1]-2);
             TblKey.Subparts[Node.Index] := rx.Match[1];
           end else
             TblKey.Columns[Node.Index] := NewText;
         end;
         1: TblKey.SubParts[Node.Index] := NewText;
       end;
       TblKey.Modified := True;
    end;
  end;

  Modification(Sender);
  VT.RepaintNode(Node);
end;


procedure TfrmTableEditor.treeIndexesBeforePaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas);
begin
  // (Re)paint index list
  treeIndexes.RootNodeCount := FKeys.Count;
end;


procedure TfrmTableEditor.treeIndexesDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var
  TargetNode: PVirtualNode;
  VT: TVirtualStringtree;
begin
  // Accept nodes from the column list and allow column moving
  VT := Sender as TVirtualStringtree;
  TargetNode := VT.GetNodeAt(Pt.X, Pt.Y);

  if Source = listColumns then begin
    // Do not accept above or below a root level (index) node
    Accept := (VT.GetNodeLevel(TargetNode) = 1) or (Mode = dmOnNode);

  end else if Source = Sender then begin
    Accept := Assigned(TargetNode) and (Sender.GetNodeLevel(TargetNode) = 1) and
      (TargetNode <> Sender.FocusedNode) and (TargetNode.Parent = Sender.FocusedNode.Parent);
  end;
end;


procedure TfrmTableEditor.treeIndexesDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  FocusedNode, TargetNode, IndexNode: PVirtualNode;
  ColName, PartLength: String;
  ColPos: Cardinal;
  VT, SourceVT: TVirtualStringtree;
  Col: PTableColumn;
  TblKey: TTableKey;
begin
  // Column node dropped here
  VT := Sender as TVirtualStringtree;
  SourceVT := Source as TVirtualStringtree;
  TargetNode := VT.GetNodeAt(Pt.X, Pt.Y);
  FocusedNode := VT.FocusedNode;
  IndexNode := nil;
  ColPos := 0;
  if not Assigned(TargetNode) then begin
    MessageBeep(MB_ICONEXCLAMATION);
    Exit;
  end;
  Mainform.LogSQL('TargetNode.Index: '+TargetNode.Index.ToString, lcDebug);

  case VT.GetNodeLevel(TargetNode) of
    0: begin
      // DragOver only accepts dmOnNode in root tree level
      IndexNode := TargetNode;
      ColPos := IndexNode.ChildCount;
    end;

    1: begin
      IndexNode := TargetNode.Parent;
      // Find the right new position for the dropped column
      ColPos := TargetNode.Index;
      if Source = Sender then begin
        // Drop within index tree: Take care if user dragged from above or from below the target node
        if FocusedNode <> nil then begin
          if (FocusedNode.Index < TargetNode.Index) and (Mode = dmAbove) and (ColPos > 0) then
            Dec(ColPos);
          if (FocusedNode.Index > TargetNode.Index) and (Mode = dmBelow) and (ColPos < IndexNode.ChildCount-1) then
            Inc(ColPos);
        end;
      end else begin
        // Drop from columns list
        if Mode = dmBelow then
          Inc(ColPos);
      end;
    end;

  end;

  if Source = Sender then
    MoveFocusedIndexPart(ColPos)
  else begin
    TblKey := FKeys[IndexNode.Index];
    Col := SourceVT.GetNodeData(SourceVT.FocusedNode);
    ColName := Col.Name;
    if TblKey.Columns.IndexOf(ColName) > -1 then begin
      if MessageDialog(_('Add duplicated column to index?'),
        f_('Index "%s" already contains the column "%s". It is possible to add a column twice into a index, but total nonsense in practice.', [VT.Text[IndexNode, 0], ColName]),
        mtConfirmation, [mbYes, mbNo]) = mrNo then
        Exit;
    end;

    TblKey.Columns.Insert(ColPos, ColName);
    PartLength := '';
    if (TblKey.IndexType <> FKEY) and (Col.DataType.Index in [dtTinyText, dtText, dtMediumText, dtLongText, dtTinyBlob, dtBlob, dtMediumBlob, dtLongBlob]) then
      PartLength := '100';
    TblKey.Subparts.Insert(ColPos, PartLength);
    IndexNode.States := IndexNode.States + [vsHasChildren, vsExpanded];
  end;
  Modification(Sender);
  // Finally tell parent node to update its children
  VT.ReinitChildren(IndexNode, False);
  VT.Repaint;
end;


procedure TfrmTableEditor.btnMoveUpIndexClick(Sender: TObject);
begin
  // Move index part up
  MoveFocusedIndexPart(treeIndexes.FocusedNode.Index-1);
end;


procedure TfrmTableEditor.btnMoveDownIndexClick(Sender: TObject);
begin
  // Move index part down
  MoveFocusedIndexPart(treeIndexes.FocusedNode.Index+1);
end;


procedure TfrmTableEditor.MoveFocusedIndexPart(NewIdx: Cardinal);
var
  TblKey: TTableKey;
begin
  // Move focused index or index part
  if treeIndexes.IsEditing then
    treeIndexes.EndEditNode;
  TblKey := FKeys[treeIndexes.FocusedNode.Parent.Index];
  if NewIdx >= Cardinal(TblKey.Columns.Count) then begin
    MessageBeep(MB_ICONEXCLAMATION);
    Exit;
  end;
  TblKey.Columns.Move(treeIndexes.FocusedNode.Index, NewIdx);
  TblKey.SubParts.Move(treeIndexes.FocusedNode.Index, NewIdx);
  Modification(treeIndexes);
  SelectNode(treeIndexes, NewIdx, treeIndexes.FocusedNode.Parent);
end;


procedure TfrmTableEditor.PageControlMainChange(Sender: TObject);
begin
  treeIndexes.EndEditNode;
  listForeignKeys.EndEditNode;
  // Ensure SynMemo's have focus, otherwise Select-All and Copy actions may fail
  if PageControlMain.ActivePage = tabCREATEcode then begin
    if SynMemoCreateCode.CanFocus then
      SynMemoCreateCode.SetFocus;
  end
  else if PageControlMain.ActivePage = tabALTERcode then begin
    if SynMemoALTERcode.CanFocus then
      SynMemoAlterCode.SetFocus;
  end;
  UpdateSQLcode;
end;


procedure TfrmTableEditor.UpdateSQLcode;
var
  OldTopLine: Integer;
  Query: TSQLSentence;
begin
  if (PageControlMain.ActivePage = tabALTERCode) and (not AlterCodeValid) then begin
    SynMemoALTERcode.BeginUpdate;
    OldTopLine := SynMemoALTERcode.TopLine;
    SynMemoALTERcode.Clear;
    for Query in ComposeAlterStatement do
      SynMemoALTERcode.Text := SynMemoALTERcode.Text + Query.SQL + ';' + CRLF;
    SynMemoALTERcode.TopLine := OldTopLine;
    SynMemoALTERcode.EndUpdate;
    AlterCodeValid := True;
  end else if (PageControlMain.ActivePage = tabCREATECode) and (not CreateCodeValid) then begin
    SynMemoCREATEcode.BeginUpdate;
    OldTopLine := SynMemoCREATEcode.TopLine;
    SynMemoCREATEcode.Clear;
    for Query in ComposeCreateStatement do
      SynMemoCREATEcode.Text := SynMemoCREATEcode.Text + Query.SQL + ';' + CRLF;
    SynMemoCREATEcode.TopLine := OldTopLine;
    SynMemoCREATEcode.EndUpdate;
    CreateCodeValid := True;
  end;
end;


procedure TfrmTableEditor.chkCharsetConvertClick(Sender: TObject);
begin
  chkCharsetConvert.Enabled := (DBObject.Name <> '') and (comboCollation.ItemIndex > -1);
  listColumns.Repaint;
  Modification(Sender);
end;


procedure TfrmTableEditor.popupColumnsPopup(Sender: TObject);
  function AddItem(Parent: TMenuItem; Caption: String; ImageIndex: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(Parent.GetParentMenu);
    Result.Caption := Caption;
    Result.ImageIndex := ImageIndex;
    Result.OnClick := AddIndexByColumn;
    Parent.Add(Result);
  end;
var
  i: Integer;
  Item: TMenuItem;
  PrimaryKeyExists,
  ColumnsSelected: Boolean;
  IndexName: String;
  Node: PVirtualNode;
  Col: PTableColumn;
begin
  ColumnsSelected := ListColumns.SelectedCount > 0;
  menuCopyColumns.Enabled := ColumnsSelected;
  menuPasteColumns.Enabled := Clipboard.HasFormat(CF_TEXT);
  menuAddToIndex.Clear;
  menuCreateIndex.Clear;
  menuAddToIndex.Enabled := ColumnsSelected;
  menuCreateIndex.Enabled := ColumnsSelected;
  if not ColumnsSelected then
    Exit;

  // Auto create submenu items for "Add to index" ...
  PrimaryKeyExists := False;
  for i:=0 to FKeys.Count-1 do begin
    if FKeys[i].IndexType = PKEY then begin
      PrimaryKeyExists := True;
      IndexName := PKEY;
    end else
      IndexName := FKeys[i].Name + ' ('+FKeys[i].IndexType+')';
    Item := AddItem(menuAddToIndex, IndexName, FKeys[i].ImageIndex);
    // Disable menuitem if all selected columns are already part of this index,
    // enable it if one or more selected columns are not.
    Item.Enabled := False;
    Node := GetNextNode(listColumns, nil, True);
    while Assigned(Node) do begin
      Col := listColumns.GetNodeData(Node);
      if FKeys[i].Columns.IndexOf(Col.Name) = -1 then begin
        Item.Enabled := True;
        Break;
      end;
      Node := GetNextNode(listColumns, Node, True);
    end;
  end;
  menuAddToIndex.Enabled := menuAddToIndex.Count > 0;

  // ... and for item "Create index"
  Item := AddItem(menuCreateIndex, PKEY, ICONINDEX_PRIMARYKEY);
  Item.Enabled := not PrimaryKeyExists;
  AddItem(menuCreateIndex, KEY, ICONINDEX_INDEXKEY);
  AddItem(menuCreateIndex, UKEY, ICONINDEX_UNIQUEKEY);
  AddItem(menuCreateIndex, FKEY, ICONINDEX_FULLTEXTKEY);
  AddItem(menuCreateIndex, SKEY, ICONINDEX_SPATIALKEY);
end;


procedure TfrmTableEditor.AddIndexByColumn(Sender: TObject);
var
  Item: TMenuItem;
  i: Integer;
  NewType: String;
  NewParts: TStringList;
  TblKey: TTableKey;
  Node: PVirtualNode;
begin
  // Auto create index or add columns to existing one by rightclicking a column
  Item := (Sender as TMenuItem);
  NewParts := TStringList.Create;
  Node := GetNextNode(listColumns, nil, True);
  while Assigned(Node) do begin
    NewParts.Add(listColumns.Text[Node, 1]);
    Node := GetNextNode(listColumns, Node, True);
  end;
  if Item.Parent = menuCreateIndex then begin
    NewType := StripHotkey(Item.Caption);
    // Avoid creating a second key with the same columns
    for i:=0 to FKeys.Count-1 do begin
      TblKey := FKeys[i];
      if (TblKey.IndexType = NewType) and (TblKey.Columns.Text = NewParts.Text) then begin
        if MessageDialog(_('Key already exists. Really create another identical one?'),
          _('This will increase disk usage and probably slow down queries on this table.'),
          mtConfirmation, [mbYes, mbNo]) = mrNo then
          Exit;
        break;
      end;
    end;
    TblKey := TTableKey.Create(DBObject.Connection);
    TblKey.Name := ImplodeStr('_', NewParts);
    TblKey.IndexType := NewType;
    TblKey.Added := True;
    TblKey.Columns := NewParts;
    for i:=0 to TblKey.Columns.Count do
      TblKey.SubParts.Add('');
    FKeys.Add(TblKey);
    PageControlMain.ActivePage := tabIndexes;
    treeIndexes.Repaint;
    SelectNode(treeIndexes, FKeys.Count-1);
    SelectNode(treeIndexes, 0, treeIndexes.FocusedNode);
  end else begin
    PageControlMain.ActivePage := tabIndexes;
    TblKey := FKeys[Item.MenuIndex];
    for i:=0 to NewParts.Count-1 do begin
      if TblKey.Columns.IndexOf(NewParts[i]) = -1 then begin
        TblKey.Columns.Add(NewParts[i]);
        TblKey.Subparts.Add('');
      end;
    end;
    SelectNode(treeIndexes, Item.MenuIndex);
    treeIndexes.ReinitNode(treeIndexes.FocusedNode, False);
    SelectNode(treeIndexes, TblKey.Columns.Count-1, treeIndexes.FocusedNode);
  end;
  Modification(Sender);
end;


procedure TfrmTableEditor.btnAddForeignKeyClick(Sender: TObject);
var
  Key: TForeignKey;
  idx: Integer;
begin
  // Add new foreign key
  Key := TForeignKey.Create(DBObject.Connection);
  idx := FForeignKeys.Add(Key);
  Key.KeyName := 'FK'+IntToStr(idx+1);
  Key.OnUpdate := '';
  Key.OnDelete := '';
  Key.Added := True;
  Modification(Sender);
  listForeignKeys.Repaint;
  SelectNode(listForeignKeys, idx);
  listForeignKeys.EditNode(listForeignKeys.FocusedNode, listForeignKeys.Header.MainColumn);
end;


procedure TfrmTableEditor.btnRemoveForeignKeyClick(Sender: TObject);
var
  Key: TForeignKey;
begin
  // Remove a foreign key
  if listForeignKeys.IsEditing then
    listForeignKeys.CancelEditNode;
  Key := FForeignKeys[listForeignKeys.FocusedNode.Index];
  if not Key.Added then
    DeletedForeignKeys.Add(Key.OldKeyName);
  FForeignKeys.Delete(listForeignKeys.FocusedNode.Index);
  Modification(Sender);
  listForeignKeys.Repaint;
end;


procedure TfrmTableEditor.btnClearForeignKeysClick(Sender: TObject);
var
  i: Integer;
begin
  // Clear all foreign keys
  if listForeignKeys.IsEditing then
    listForeignKeys.CancelEditNode;
  for i:=FForeignKeys.Count-1 downto 0 do begin
    if not FForeignKeys[i].Added then
      DeletedForeignKeys.Add(FForeignKeys[i].OldKeyName);
    FForeignKeys.Delete(i);
  end;
  Modification(Sender);
  listForeignKeys.Repaint;
end;


procedure TfrmTableEditor.listForeignKeysBeforePaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
begin
  // Set RootNodeCount
  listForeignKeys.RootNodeCount := FForeignKeys.Count;
  btnClearForeignKeys.Enabled := listForeignKeys.RootNodeCount > 0;
end;


procedure TfrmTableEditor.listForeignKeysEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var
  Key: TForeignKey;
begin
  // Disallow editing foreign columns when no reference table was selected.
  // Also, check for existance of reference table and warn if it's missing.
  if Column = 3 then begin
    Key := FForeignKeys[Node.Index];
    Allowed := False;
    if Key.ReferenceTable = '' then
      ErrorDialog(_('Please select a reference table before selecting foreign columns.'))
    else begin
      try
        DBObject.Connection.GetVar('SELECT 1 FROM '+DBObject.Connection.QuoteIdent(Key.ReferenceTable, True, '.')+' LIMIT 1');
        Allowed := True;
      except
        // Leave Allowed = False
        ErrorDialog(f_('Reference table "%s" seems to be missing, broken or non-accessible.', [Key.ReferenceTable]))
      end;
    end;
  end else
    Allowed := True;
end;


procedure TfrmTableEditor.listForeignKeysCreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);
var
  VT: TVirtualStringTree;
  EnumEditor: TEnumEditorLink;
  SetEditor: TSetEditorLink;
  DBObjects: TDBObjectList;
  Key: TForeignKey;
  ColNode: PVirtualNode;
  Col: PTableColumn;
  Obj: TDBObject;
begin
  // Init grid editor in foreign key list
  VT := Sender as TVirtualStringTree;
  case Column of
    0: EditLink := TInplaceEditorLink.Create(VT);
    1: begin
        SetEditor := TSetEditorLink.Create(VT);
        ColNode := listColumns.GetFirst;
        while Assigned(ColNode) do begin
          Col := listColumns.GetNodeData(ColNode);
          SetEditor.ValueList.Add(Col.Name);
          ColNode := listColumns.GetNextSibling(ColNode);
        end;
        EditLink := SetEditor;
      end;
    2: begin
        EnumEditor := TEnumEditorLink.Create(VT);
        EnumEditor.AllowCustomText := True;
        DBObjects := DBObject.Connection.GetDBObjects(DBObject.Connection.Database);
        for Obj in DBObjects do begin
          if (Obj.NodeType = lntTable) and (LowerCase(Obj.Engine) = 'innodb') then
            EnumEditor.ValueList.Add(Obj.Name);
        end;
        EditLink := EnumEditor;
      end;
    3: begin
        Key := FForeignKeys[Node.Index];
        SetEditor := TSetEditorLink.Create(VT);
        SetEditor.ValueList := DBObject.Connection.GetCol('SHOW COLUMNS FROM '+DBObject.Connection.QuoteIdent(Key.ReferenceTable, True, '.'));
        EditLink := SetEditor;
      end;
    4, 5: begin
        EnumEditor := TEnumEditorLink.Create(VT);
        EnumEditor.ValueList.Text := 'RESTRICT'+CRLF+'CASCADE'+CRLF+'SET NULL'+CRLF+'NO ACTION';
        EditLink := EnumEditor;
      end;
  end;
end;


procedure TfrmTableEditor.listForeignKeysFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  // Focus on foreign key list changed
  btnRemoveForeignKey.Enabled := Assigned(Sender.FocusedNode);
end;


procedure TfrmTableEditor.listForeignKeysGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  // Return image index for node cell in foreign key list
  if not (Kind in [ikNormal, ikSelected]) then Exit;
  case Column of
    0: ImageIndex := 136;
    else ImageIndex := -1;
  end;
end;


procedure TfrmTableEditor.listForeignKeysGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Key: TForeignKey;
begin
  // Return cell text in foreign key list
  Key := FForeignKeys[Node.Index];
  case Column of
    0: CellText := Key.KeyName;
    1: CellText := ImplodeStr(',', Key.Columns);
    2: CellText := Key.ReferenceTable;
    3: CellText := ImplodeStr(',', Key.ForeignColumns);
    4: begin
        CellText := Key.OnUpdate;
        // Both ON UPDATE + DELETE default to "RESTRICT", see http://dev.mysql.com/doc/refman/5.1/en/innodb-foreign-key-constraints.html
        if CellText = '' then
          CellText := 'RESTRICT';
      end;
    5: begin
        CellText := Key.OnDelete;
        if CellText = '' then
          CellText := 'RESTRICT';
      end;
  end;
end;


procedure TfrmTableEditor.listForeignKeysNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  Key, OtherKey: TForeignKey;
  i, j, k: Integer;
  NameInUse: Boolean;
  RefCreateCode, RefDatabase, RefTable: String;
  KeyColumnsSQLCode, RefColumnsSQLCode: String;
  Err: String;
  RefColumns: TTableColumnList;
  TypesMatch: Boolean;
  RefObj: TDBObject;
begin
  // Cell text in foreign key list edited
  Key := FForeignKeys[Node.Index];
  Key.Modified := True;
  Modification(Sender);
  case Column of
    0: begin
      Key.KeyName := NewText;
      Key.KeyNameWasCustomized := True;
    end;
    1: Key.Columns := Explode(',', NewText);
    2: begin
      Key.ReferenceTable := NewText;
      if not Key.KeyNameWasCustomized then begin
        Key.KeyName := 'FK_'+DBObject.Name+'_'+Key.ReferenceTable;
        i := 1;
        NameInUse := True;
        while NameInUse do begin
          for OtherKey in FForeignKeys do begin
            NameInUse := (Key <> OtherKey) and (Key.KeyName = OtherKey.KeyName);
            if NameInUse then break;
          end;
          if NameInUse then begin
            Inc(i);
            Key.KeyName := 'FK_'+DBObject.Name+'_'+Key.ReferenceTable+'_'+IntToStr(i);
          end;
        end;

      end;
    end;
    3: begin
      Key.ForeignColumns := Explode(',', NewText);

      // Compare data types and unsigned flags of source and reference columns. See issue #400
      Err := '';
      if Key.Columns.Count <> Key.ForeignColumns.Count then begin
        Err := _('Foreign column count does not match source column count.');
      end else begin
        RefDatabase := DBObject.Database;
        RefTable := Key.ReferenceTable;
        i := Pos('.', RefTable);
        if i > 0 then begin
          RefDatabase := Copy(RefTable, 1, i-1);
          RefTable := Copy(RefTable, i+1, MaxInt);
        end;
        RefObj := TDBObject.Create(DBObject.Connection);
        RefObj.Name := RefTable;
        RefObj.Database := RefDatabase;
        RefObj.NodeType := lntTable;
        RefCreateCode := DBObject.Connection.GetCreateCode(RefObj);
        RefColumns := TTableColumnList.Create(True);
        DBObject.Connection.ParseTableStructure(RefCreateCode, RefColumns, nil, nil);
        TypesMatch := True;
        KeyColumnsSQLCode := '';
        RefColumnsSQLCode := '';
        for i:=0 to Key.Columns.Count-1 do begin
          for j:=0 to FColumns.Count-1 do begin
            if FColumns[j].Name = Key.Columns[i] then begin
              KeyColumnsSQLCode := KeyColumnsSQLCode + FColumns[j].SQLCode + CRLF;
              for k:=0 to RefColumns.Count-1 do begin
                if RefColumns[k].Name = Key.ForeignColumns[i] then begin
                  RefColumnsSQLCode := RefColumnsSQLCode + RefColumns[k].SQLCode + CRLF;
                  TypesMatch := TypesMatch
                    and (RefColumns[k].DataType.Index = FColumns[j].DataType.Index)
                    and (RefColumns[k].Unsigned = FColumns[j].Unsigned);
                end;
              end;
            end;
          end;
        end;
        if not TypesMatch then begin
          Err := _('The selected foreign column do not match the source columns data type and unsigned flag. This will give you an error message when trying to save this change. Please compare yourself:');
          Err := Err + CRLF + CRLF + KeyColumnsSQLCode + CRLF + Trim(RefColumnsSQLCode);
        end;
      end;
      if Err <> '' then
        ErrorDialog(_('Foreign key mismatch'), Err);

    end;
    4: Key.OnUpdate := NewText;
    5: Key.OnDelete := NewText;
  end;
end;


procedure TfrmTableEditor.btnHelpClick(Sender: TObject);
begin
  // Help button
  Help(Self, 'createtable');
end;


procedure TfrmTableEditor.menuCopyColumnsClick(Sender: TObject);
var
  Node: PVirtualNode;
  Col: PTableColumn;
  SQL: String;
begin
  // Copy selected columns as a CREATE TABLE query to clipboard
  Node := GetNextNode(listColumns, nil, True);
  SQL := 'CREATE TABLE dummy ('+CRLF;
  while Assigned(Node) do begin
    Col := listColumns.GetNodeData(Node);
    SQL := SQL + #9 + Col.SQLCode + ','+CRLF;
    Node := GetNextNode(listColumns, Node, True);
  end;
  Delete(SQL, Length(SQL)-2, 3);
  SQL := SQL + CRLF + ')';
  Clipboard.AsText := SQL;
end;


procedure TfrmTableEditor.menuPasteColumnsClick(Sender: TObject);
var
  Columns: TTableColumnList;
  Node: PVirtualNode;
  Col: TTableColumn;
begin
  Columns := TTableColumnList.Create(False);
  DBObject.Connection.ParseTableStructure(Clipboard.AsText, Columns, nil, nil);
  Node := listColumns.FocusedNode;
  if not Assigned(Node) then
    Node := listColumns.GetLast;
  listcolumns.BeginUpdate;
  try
    for Col in Columns do begin
      Col.Status := esAddedUntouched;
      // Create new node, insert column structure into list, and let OnInitNode bind its pointer
      Node := listColumns.InsertNode(Node, amInsertAfter, nil);
      FColumns.Insert(Node.Index, Col);
    end;
  finally
    listcolumns.EndUpdate;
  end;
  listColumns.Invalidate;
  Modification(Sender);
  Columns.Free;
end;


end.
